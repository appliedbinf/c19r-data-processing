popC <- vroom::vroom("data-raw/map_data/county_populationCV.csv")
usethis::use_data(popC, overwrite = TRUE, internal = TRUE)
