#' Data from the trial of serial killer Kristen Gilbert
#'
#'
#' Intensive care unit nurse \href{https://en.wikipedia.org/wiki/Kristen_Gilbert}{Kristen Gilbert} worked
#' for several years in the 1990s at a Veterans Administration Hospital.
#' Her co-workers became suspicious. The co-workers observed that unexpected
#' patient deaths occurred more frequently on her shifts than on other shifts.
#' They also noticed a shortage of supplies
#' of the cardiac stimulant epinephrine, which can be fatal
#' when administered in large enough doses through an IV drip. The hospital
#' investigators went through all the shifts during the years Gilbert
#' worked at the hospital, noting whether Gilbert was on duty during
#' that shift and whether there was a death during the shift.
#'
#' @usage data(Gilbert)
#'
#' @docType data
#' @format A data frame with one row for each shift at the VA hospital.
#'
#' - `death` Whether a patient death occurred during the shift.
#' - `gilbert` Whether nurse Kristen Gilbert was on duty during the shift.
#'
#' - `time`: the winning time in seconds
#' - `race` the name of the race. Many races are repeated over successive years.
#' - `year` the year the race was run
#' - `name` the name of the winning runner
#' - `sex`: the runner's sex, coded as `F` and `M`
#' - `distance`: the total distance of the race in km
#' - `climb`: the total vertical climb of the race in meters
#'
#' @details Only tabular summaries of the shift/death information
#' are public. This data frame was reconstructed from those summaries.
#'
#' @keywords datasets
#'
#'
"Gilbert"
