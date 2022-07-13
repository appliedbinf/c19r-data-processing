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
#' @param event_size Vector of event sizes to predict risk with
#' @param asc_bias_list Vector of ascertainment biases to predict risk with
#' @param scale_factor Scaling factor, should be =< 1
#' @param risk_output Risk output
#' @param vaccine_output Risk outut
#' @param output_prefix Output file prefux to applu to risk_output and vaccine_output
#' @param year Year, starting with 2020, to grab data for
#'
#' @export
#'
create_c19r_data <- function(risk_output = "usa_risk_counties.csv",
                             vaccine_output = "usa_risk_countiesV.csv",
                             output_prefix = ".",
                             event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000),
                             asc_bias_list = c(3, 4, 5),
                             scale_factor = (10 / 14),
                             year = 2022) {

  library(sf) # needed to make tibble happen for joins

  if (!all(is.numeric(event_size)) & !all(event_size > 0)) {
    stop("'event_size' must be a vector of positive numbers")
  }

  if (!all(is.numeric(asc_bias_list)) & !all(asc_bias_list > 0)) {
    stop("'asc_bias_list' must be a vector of positive numbers")
  }

  if (!is.numeric(scale_factor) | scale_factor > 1) {
    stop("Scaling factor is either not numeric or >1")
  }

  risk_output <- file.path(output_prefix, risk_output)
  if (file.access(dirname(risk_output), mode = 2) != 0) {
    stop("Directory for risk_output file does not appear to be writeable.")
  }
  vaccine_output <- file.path(output_prefix, vaccine_output)
  if (file.access(dirname(vaccine_output), mode = 2) != 0) {
    stop("Directory for vaccine_output file does not appear to be writeable.")
  }



  dataurl <- glue::glue("https://github.com/nytimes/covid-19-data/raw/master/us-counties-{year}.csv")
  data <- vroom::vroom(dataurl)

  cur_date <- lubridate::ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- lubridate::ymd(cur_date) - 14

  nyc <- c(36005, 36047, 36061, 36085, 36081)
  vaccurl = "https://github.com/bansallab/vaccinetracking/raw/main/vacc_data/data_county_timeseries.csv"
  vacc_data <- vroom::vroom(vaccurl)
  vacc_data <- vacc_data %>%
      tidyr::drop_na(DATE) %>%
    dplyr::filter(CASE_TYPE %in% c("Complete"))%>%
    tidyr::pivot_wider(
      names_from = CASE_TYPE,
      values_from = CASES
    )%>%
    dplyr::select(
      county = COUNTY,
      cnt_fully_vacc = Complete,
      date = DATE
    )%>%
    dplyr::mutate(
      date = lubridate::as_date(date),
      county = dplyr::case_when(
        as.numeric(county) %in% c(2164, 2060) ~ 2997,
        as.numeric(county) %in% c(2282, 2105) ~ 2998,
        as.numeric(county) %in% nyc ~ 99999,
        TRUE ~ as.numeric(county)
      )
    ) %>%
    dplyr::group_by(
      date, county
    ) %>%
    dplyr::summarise(
      cnt_fully_vacc = sum(cnt_fully_vacc),
      # cnt_partially_vacc = sum(cnt_partially_vacc)
    ) %>%
    dplyr::ungroup()

  ex_dates <- c(
    vacc_data$date %>%
      sort() %>%
      dplyr::first(),
    vacc_data$date %>%
      sort() %>%
      dplyr::last()
  )

  ex_dates[2] <- max(ex_dates[2], past_date)

  all_dates <- ex_dates[1]+0:as.numeric(ex_dates[2]-ex_dates[1])
  all_county <- c(vacc_data$county, c(29991, 29992)) %>% # add Joplin and KC
    unique()

  add_dates <- purrr::map_df(all_county,~dplyr::tibble(
      county = .x,
      date = all_dates
    )
  )

  vacc_data_fill <- vacc_data%>%
    dplyr::mutate(
      last_date = date
    )%>%
    dplyr::full_join(
      add_dates
    )%>%
    dplyr::group_by(county)%>%
    dplyr::arrange(date)%>%
    dplyr::mutate(
      cnt_fully_vacc = zoo::na.approx(cnt_fully_vacc, na.rm=F),
      # cnt_partially_vacc = na.approx(cnt_partially_vacc, na.rm=F),
      # the 'is_last' variable is telling us that it could not interpolate cnt_fully_vacc because the date is after the last value in the bansal data set.
      # Therefore, when is_last == T, we can display the "last date" in the mouseover UI
      is_last = dplyr::case_when(
        is.na(cnt_fully_vacc)==T ~ TRUE,
        TRUE ~ FALSE
      )
    )%>%
    tidyr::fill(
      cnt_fully_vacc,
      # cnt_partially_vacc,
      last_date
    )%>%
    dplyr::ungroup()

 VaccImm <- vacc_data_fill%>%
    dplyr::filter(date==past_date)%>%
    dplyr::inner_join(pop, by = c("county"="fips"))%>%
    dplyr::select(
      -date
    )%>%
    dplyr::mutate( # Creating a new column to group by so we can give the same vaccination rates to areas surrounding Joplin and KC
      v = dplyr::case_when(
        county %in% c(29095, 29047, 29165, 29037, 29991) ~ 29993,
        county %in% c(29097, 29145, 29992) ~ 29994,
        TRUE ~ county
      )
    )%>%
    dplyr::group_by(v)%>%
    dplyr::mutate(
      pct_fully_vacc = sum(cnt_fully_vacc, na.rm=T)/sum(pop, na.rm=T)*100
    )%>%
    dplyr::select(-v)

  data_cur <- data %>%
    dplyr::filter(date == cur_date) %>%
    dplyr::mutate(fips = dplyr::case_when(
      county == "New York City" ~ 99999,
      county == "Kansas City" ~ 29991,
      county == "Joplin" ~ 29992,
      TRUE ~ as.numeric(fips)
    )) %>%
    dplyr::select(c(state, fips, cases, deaths))

  data_past <- data %>%
    dplyr::filter(date == past_date) %>%
    dplyr::mutate(fips = dplyr::case_when(
      county == "New York City" ~ 99999,
      county == "Kansas City" ~ 29991,
      county == "Joplin" ~ 29992,
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
            risk = dplyr::if_else(Nr > 10, round(calc_risk(
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
  risk_data_df <-  sf::st_as_sf(county) %>%
    dplyr::left_join(plyr::join_all(risk_data, by = c("fips", "state")), by = c("GEOID" = "fips")) %>%
    dplyr::left_join(VaccImm, by = c("GEOID" = "county")) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(imOp = dplyr::case_when(pct_fully_vacc < 50 ~ 0.0, pct_fully_vacc > 50 ~ 0.7)) %>% # binary filter
    dplyr::mutate(updated = lubridate::ymd(gsub("-", "", Sys.Date())))


  utils::write.csv(risk_data_df,
    risk_output,
    quote = F,
    row.names = F
  )

}
