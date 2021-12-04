#' USA county geoms
#'
#' A simple feature collection of USA county geoms for use in Leaflet
#'
#' @format Simple feature collection with 6 features and 3 fields:
#' \describe{
#'   \item{GEOID}{FIPS number}
#'   \item{NAME}{County common name}
#'   \item{stname}{Abbreviation of state the county is in}
#'   \item{geometry}{Polygon geometry for mapping}
#' }

"county"

#' USA state geoms
#'
#' A simple feature collection of USA state geoms for use in Leaflet.
#'
#' @format Simple feature collection with 6 features and 2 fields:
#' \describe{
#'   \item{stname}{Abbreviation of state}
#'   \item{name}{State full name}
#'   \item{geomtry}{Polygon geometry for mappings}
#' }

"stateline"


#' USA county population.
#'
#' A dataset containing FIPS and county population.
#'
#' @format A data frame with 3221 rows and 2 variables:
#' \describe{
#'   \item{fips}{FIPS number}
#'   \item{pop}{Population}
#' }

"pop"
