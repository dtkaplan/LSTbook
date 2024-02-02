#' Voting patterns in the 1933 German national election
#'
#' 1933 was the year that Hitler and the Nazi party came to power. The initial basis for this
#' was a national election in which the Nazis secured a substantial fraction of the vote. (Immediately
#' after the election, the Nazis burned the Reichtag (the German parliament) and started repressing their political
#' opposition though a campaign of imprisonment and murder.)
#'
#' @usage data("Germany1933vote")
#'
#' @docType data
#'
#' @format A data frame with 681 rows and 7 variables. Each row is a German precinct.
#'
#' - self: share of potential voters who are self-employed
#' - blue: share of potential voters who are blue-collar workers
#' - white: share of potential voters who are white-collar workers
#' - domestic: share of potential voters who are employed domestically
#' - unemployed: share of potential voters who are un-employed
#' - nvoter: number of eligible voters (not clear if this include people who didn't vote)
#' - nazivote: number of votes for the Nazis
#'
#' @references
#' -  Imai, Kosuke. 2017. Quantitative Social Science: An Introduction. Princeton University Press. [URL](https://press.princeton.edu/books/hardcover/9780691167039/quantitative-social-science) from whence
#' these data were added to this package. In QSS, the data are called `nazis`.
#' - G. King, O. Rosen, M. Tanner, A.F. Wagner (2008) “Ordinary economic
#' voting behavior in the extraordinary election of Adolf Hitler.”
#' Journal of Economic History, vol. 68, pp. 951–996.#'
"Germany1933vote"
