stateline <- sf::st_read("data-raw/map_data/US_stateLines.geojson")[, c("STUSPS", "NAME")]
names(stateline) <- c("stname", "name", "geometry")
usethis::use_data(stateline, overwrite = TRUE, internal = TRUE)
