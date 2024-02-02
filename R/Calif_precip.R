#' Annual precipitation in California locations
#'
#' These data are from an article in the journal *Geography* that illustrates
#' precipitation modeling.
#'
#'
#'
#' @docType data
#'
#'
#' @format A data.frame with 30 rows, each a weather station in California
#'
#' - `station`: the name of the station
#' - `precip` average annual precipitation in inches
#' - `altitude` in feet
#' - `latitude` the station's north-south location (degrees North)
#' - `distance`: distance in miles from the coast
#' - `orientation`: related to the rain shadow effect of the mountains.
#' "W" means westward facing (toward the prevailing winds). "L" mean "leeward," that is,
#' facing away from the prevailing winds.
#'
#' @keywords datasets
#'
#' @source P. J. Taylor (1980)  "A Pedagogic Application of Multiple Regression Analysis: Precipitation in California" *Geography* **65** (3) 203-212
"Calif_precip"
