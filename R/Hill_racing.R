#' Winning times in Scottish Hill races, 2005-2017
#'
#' @docType data
#'
#' @usage data(Hill_racing)
#'
#' @format A data.frame object with one row for each race winner. Most races have both a
#' male and female winner.
#'
#' "year"     "sex"      "name"     "time"     "race"     "distance" "climb"
#'
#' - `time`: the winning time in seconds
#' - `race` the name of the race. Many races are repeated over successive years.
#' - `year` the year the race was run
#' - `name` the name of the winning runner
#' - `sex`: the runner's sex, coded as `F` and `M`
#' - `distance`: the total distance of the race in km
#' - `climb`: the total vertical climb of the race in meters
#'
#'
#' @keywords datasets
#'
#' @source  The data were scraped from the \href{http://www.scottishhillracing.co.uk/ResultsSummary.aspx}{Scottish Hill Racing site}.
#'
"Hill_racing"
