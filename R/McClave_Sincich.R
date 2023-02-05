#' Data from McClave-Sincich *Statistics* 11/e
#'
#' These are relatively small data frames used for exercises
#' @name McClave_Sincich
#'
#' @docType data
#'
#'
#' @keywords datasets
#'
#' @source \href{https://www.statcrunch.com/books/?book=mcclave_mstat12e}{StatCrunch}
#'
#' @rdname McClave_Sincich
#' @format `Clock_auction`: Prices for grandfather clocks sold in auction
#' - Sales price for the clock
#' - Age of the clock
#' - Number of bidders for the clock
"Clock_auction"

#' @rdname McClave_Sincich
#' @format `Geography_journals`: Prices for geography journals, c. 2005
#' - `journal` name of journal
#' - `cost` for a one-year subscription
#' - `jif` journal impact factor
#' - `cites` number of citations of the journal in the past five years
#' - `rpi` relative price index
#'
"Geography_journals"

#' @rdname McClave_Sincich
#' @format `PGA_index`: Driving distance, accuracy, and a derived index from the PGA tour
#' - `player` name of the player
#' - `dist` driving distance in yards
#' - `accuracy` percent of drives that land in the fairway
#' - `index` an index score for ranking players.
#'
"PGA_index"

#' @rdname McClave_Sincich
#' @format `Dowsing`: Locations identified by dowsers in an experiment
#' - `trial` just the row number
#' - `subject` identifying number assigned to the subject
#' - `pipe` location of the flowing-water pipe along a 10-meter line (decimeters)
#' - `guess` the dowser's guess of the location of the pipe in that trial (decimeters)
#'
"Dowsing"
