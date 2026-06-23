#' Galton's dataset of parent and child heights
#'
#' In the 1880's, Francis Galton was developing ways to quantify the
#' heritability of traits.  As part of this work, he collected data on
#' the heights of adult children and their parents.
#'
#' @docType data
#' @keywords datasets
#' @name Galton
#' @usage data(Galton)
#' @format
#'   A data frame with 898 observations on the following variables.
#'   \describe{
#'     \item{\code{family}}{a factor with levels for each family}
#'     \item{\code{father}}{the father's height (in inches)}
#'     \item{\code{mother}}{the mother's height (in inches)}
#'     \item{\code{sex}}{the child's sex: \code{F} or \code{M}}
#'     \item{\code{height}}{the child's height as an adult (in inches)}
#'     \item{\code{nkids}}{the number of adult children in the family, or, at least,
#' the number whose heights Galton recorded.}
#'   }
#'
#' @details
#' Entries were deleted for
#' those children whose heights were not recorded numerically by Galton,
#' who sometimes used entries such as "tall", "short", "idiotic",
#' "deformed" and so on.
#'
#' @source
#' The data were transcribed by J.A. Hanley who has published them at
#' \url{http://www.medicine.mcgill.ca/epidemiology/hanley/galton/}
#'
#' @references
#' "Transmuting" women into men: Galton's family data on human stature. (2004)
#' \emph{The American Statistician}, 58(3):237-243.
#'
#' @examples
#' data(Galton)
#'
#' @keywords datasets
#'
'Galton'
