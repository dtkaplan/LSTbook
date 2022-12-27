#' School bus delays in New York City
#'
#' @docType data
#'
#' @details The data have been lightly cleaned from their original form as a CSV file. 305 rows from the original
#' data were removed: those with a number of passengers greater than 48. (These were presumably
#' data-entry errors since a common legal limit for bus passengers is 48.)
#'
#' @usage data(Bus_delays)
#'
#' @format A data.frame object with one row for each of 238,266 bus-delay
#' incidents in New York City.
#'
#' - `breakdown_id`: A unique (almost) id number for each incident
#' - `year` the school year of the incident
#' - `route_name` the name of the bus route
#' - `delay_duration` how long (minutes) the delay lasted
#' - `pre_k` whether the bus was for Pre-kindergarden students. If not, it was for elementary
#' school students. (Older students ride the city bus in NYC.)
#' - `reason` an explanation of the cause of the delay
#' - `boro` which of the jurisdictions of the NYC area the delay occurred in.
#' - `n_students` how many student passengers were on the bus when the delay occurred.
#' - `company` the name of the company operating the bus
#' - `date` the date of the incident
#'
#'
#' @keywords datasets
#'
#' @source  The data themselves were scraped from the New York City OpenData site,
#' <https://data.cityofnewyork.us/Transportation/Bus-Breakdown-and-Delays/ez4e-fazm> on Dec. 13, 2018.
#' The data was brought to the author's attention on a blog written by Nils Ross <https://datascienceplus.com/nyc-bus-delays/>. Much
#' of the code used to clean the data was copied from that blog post.
"Bus_delays"
