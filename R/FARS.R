#' Annual summaries concerning motor-vehicle related fatalities in the US#'
#'
#' @docType data
#'
#' @usage data(FARS)
#'
#' @format A data.frame object with one row per year from 1994 to 2016
#'
#' - `year`: The year covered by the summary
#' - `crashes` the number of incidents in that year
#' - `drivers` the number of drivers killed in those incidents
#' - `passengers` the number of passengers killed in those incidents
#' - `unknown` vehicle occupants killed whose status as driver or passenger is unknown
#' - `motorcyclists` the number of motorcyclists killed in those incidents
#' - `pedestrians` the number of pedestrians killed in those incidentss
#' - `pedalcyclists` the number of non-motorized cyclist deaths
#' - `other_nonvehicle` the number of other deaths in those incidents
#' - `vehicle_miles` the number of miles driven by all vehicles, whether they were
#' involved in an incident or not. (billions of miles)
#' - `population` the population of the US (thousands of people)
#' - `registered_vehicles` the number of motor vehicles registered in the US (thousands)
#' - `licenced_drivers` the number of licenced drivers in the US (thousands)
#'
#'
#' @keywords datasets
#'
#' @source  From the Fatality Analysis Reporting System <https://www-fars.nhtsa.dot.gov/Main/index.aspx>
#' of the US Department of Transportation (DOT). The
#' data have been put into a tidy form from the untidy version published by the DOT,
#' removing columns calculated from other columns and so on.
"FARS"
