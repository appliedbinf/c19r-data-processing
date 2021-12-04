county <- sf::st_read("map_data/geomUnitedStates.geojson")
usethis::use_data(county, overwrite = TRUE, internal = TRUE)
