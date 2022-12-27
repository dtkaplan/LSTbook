#' World records in the 100 & 200 m butterfly swim
#'
#'
#'
#' @docType data
#'
#' @usage data(Butterfly)
#'
#' @format A data.frame object with one row for each world record and variables
#'
#' - `time`: the record time in seconds
#' - `swimmer` the name of the swimmer
#' - `date` a Date object containing the date the record was made
#' - `place` string descripting the location
#' - `sex`: coded as `F` and `M`
#' - `lengths`: the total distance was divided into lengths of either 25 or 50 meters. `lengths` gives the number of such lengths in the total distance.
#' - `dist`: the total distance (in meters) of the race
#'
#' @keywords datasets
#'
#' @source  \href{https://en.wikipedia.org/wiki/World_record_progression_100_metres_butterfly}{Wikipedia}
#'
"Butterfly"
