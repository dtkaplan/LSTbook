#' Ballots in the 2013 Mayoral election in Minneapolis
#'
#' Ballot information for the 2013 Minneapolis Mayoral election, which was run
#' as a rank-choice election. The choices marked on each (valid) ballot for the election, which was run
#' using a rank-choice, instant runoff system.
#'
#' @docType data
#'
#' @details In rank-choice, a voter can indicate first,
#' second, and third choices.  If a voter's first choice is eliminated (by
#' being last in the count across voters), the second choice is promoted to
#' that voter's first choice, and similarly third -> second.  Eliminations are
#' done successively until one candidate has a majority of the first-choice
#' votes.
#'
#' @format A data frame with 80,101 observations on the following 5 variables. The unit of observation
#' is a (valid) ballot cast in the election.
#' All variables are stored as character strings.
#'
#' - `Precinct` Precincts are sub-divisions within Wards.
#' - `First` The voter's first choice. "undervote" means that no candidate was selected.
#' - `Second` The voter's second choice
#' - `Third` The voter's third choice
#' - `Ward` The city is divided spatially into districts or 'wards'. These are further subdivided into precincts.
#'
#'
#' @references
#' Description of ranked-choice voting:
#' \url{https://vote.minneapolismn.gov/ranked-choice-voting/}
#'
#' A Minnesota Public Radio story about the election ballot tallying process:
#' \url{https://www.mprnews.org/2013/11/22/politics/ranked-choice-vote-count-programmers/}
#'
#' The Wikipedia article about the election:
#' \url{https://en.wikipedia.org/wiki/2013_Minneapolis_mayoral_election}
#'
#' Ballot data from the Minneapolis city government:
#' \url{https://vote.minneapolismn.gov/results-data/election-results/2013/mayor/}

"Minneapolis2013"
