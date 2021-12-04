pop <- vroom::vroom("map_data/county-population.csv")
usethis::use_data(pop, overwrite = TRUE, internal = TRUE)
