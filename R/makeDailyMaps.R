#' Calculate risk
#'
#' @param I Number infected
#' @param g Event size
#' @param pop Population size
#'
calc_risk <- function(I, g, pop) {
  p_I <- I / pop
  r <- 1 - (1 - p_I)**g
  return(round(r * 100, 1))
}


#' Create covid19 risk prediction data
#'
#' @param output Output file
#' @param event_size Vector of event sizes to predict risk with
#' @param asc_bias_list Vector of ascertainment biases to predict risk with
#' @param scale_factor Scaling factor, should be =< 1
#'
#' @export
#'
#' @examples
#' create_c19r_data(output = "./2022-01-01-usa-counties.csv")
create_c19r_data <- function(output = "usa_risk_counties.csv",
                             event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000),
                             asc_bias_list = c(3, 4, 5),
                             scale_factor = (10 / 14)) {
  if (!all(is.numeric(event_size)) & !all(event_size > 0)) {
    stop("'event_size' must be a vector of positive numbers")
  }
  if (!all(is.numeric(asc_bias_list)) & !all(asc_bias_list > 0)) {
    stop("'asc_bias_list' must be a vector of positive numbers")
  }
  if (!is.numeric(scale_factor) | scale_factor > 1) {
    stop("Scaling factor is either not numeric or >1")
  }

  if (file.access(dirname(output), mode = 2) != 0) {
    stop("Directory for output file does not appear to be writeable.")
  }


  dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  data <- vroom::vroom(dataurl)

  cur_date <- lubridate::ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- lubridate::ymd(cur_date) - 14
  vacc_data <-
    vroom::vroom(
      "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
    )

  VaccImm <-
    vacc_data[which(vacc_data$date == past_date), ] %>% dplyr::select(location,
      pct_partially_vacc = people_vaccinated_per_hundred,
      pct_fully_vacc = people_fully_vaccinated_per_hundred
    )
  VaccImm$location[which(VaccImm$location == "New York State")] <-
    "New York"

  data_cur <- data %>%
    dplyr::filter(date == cur_date) %>%
    dplyr::mutate(fips = dplyr::case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    dplyr::select(state, fips, cases, deaths)
  data_past <- data %>%
    dplyr::filter(date == past_date) %>%
    dplyr::mutate(fips = dplyr::case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    dplyr::select(fips = fips, cases_past = cases)
  data_join <- data_cur %>%
    dplyr::inner_join(data_past, by = "fips") %>%
    dplyr::inner_join(pop, by = "fips")

  risk_data <- list()

  for (asc_bias in asc_bias_list) {
    data_Nr <- data_join %>%
      dplyr::mutate(Nr = (cases - cases_past) * asc_bias * scale_factor)

    if (dim(data_Nr)[1] > 2000) {
      maps <- list()
      for (size in event_size) {
        cn <- glue::glue("{asc_bias}_{size}")

        riskdt <- data_Nr %>%
          dplyr::mutate(
            risk = if_else(Nr > 10, round(calc_risk(
              Nr, size, pop
            )), 0),
            "asc_bias" = asc_bias,
            "event_size" = size
          )
        risk_data[[cn]] <-
          riskdt %>% dplyr::select(state, fips, "{cn}" := risk)
        riskdt_map <-
          county %>% dplyr::left_join(riskdt, by = c("GEOID" = "fips"))
        id <- paste(asc_bias, size, sep = "_")
      }
    }
  }
  risk_data <- county %>%
    dplyr::left_join(plyr::join_all(risk_data, by = c("fips", "state")), by = c("GEOID" = "fips")) %>%
    dplyr::left_join(VaccImm, by = c("state" = "location")) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(imOp = dplyr::case_when(pct_fully_vacc < 50 ~ 0.0, pct_fully_vacc > 50 ~ pct_fully_vacc / 100)) %>%
    dplyr::mutate(updated = lubridate::ymd(gsub("-", "", Sys.Date())))

  utils::write.csv(risk_data,
    output,
    quote = F,
    row.names = F
  )
}
