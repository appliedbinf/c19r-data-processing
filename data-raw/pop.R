pop <- vroom::vroom("data-raw/map_data/county-population.csv")
popC <- vroom::vroom("data-raw/map_data/county_populationCV.csv")
pop <- pop%>%
    dplyr::full_join(
        dplyr::select(popC, c(fips, popC=pop)), by="fips"
    )
usethis::use_data(pop, overwrite = TRUE, internal = TRUE)
