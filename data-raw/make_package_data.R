rm(list=ls())

source_files = list.files("data-raw/", pattern = "*.R", full.names = T)

sapply(source_files, source)

usethis::use_data(county, pop, stateline, overwrite = T, internal = T)

rm(list=ls())
