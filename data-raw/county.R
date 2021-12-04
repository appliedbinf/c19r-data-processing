county <- sf::st_read("data-raw/map_data/geomUnitedStates.geojson")
usethis::use_data(county, overwrite = TRUE, internal = TRUE)
