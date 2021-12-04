countyVacc <- st_read("data-raw/map_data/geomUnitedStatesCV.geojson")
usethis::use_data(countyVacc, overwrite = TRUE, internal = TRUE)
