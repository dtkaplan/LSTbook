#' STAR Project Data
#'
#' Data from the STAR (Student–Teacher Achievement Ratio) Project, a
#' four-year longitudinal study examining the effect of class size in early grade
#' levels on educational performance and personal development
#'
#' @usage data("STAR")
#'
#' @docType data
#'
#' @format A data frame with 6325 rows and 6 variables:
#'
#' - race: black or white
#' - classtype: kindergarten class type: small, regular, regular with aid
#' - yearssmall: number of years (0 to 4) in small classes
#' - hsgrad: high-school graduation (graduated or not). NOTE: There are many NAs
#' - g4math: total scaled score for the math portion of the fourth-grade standardized test
#' - g4reading: total scaled score for the reading portion of the fourth-grade standardized test
#' @references
#' -  Imai, Kosuke. 2017. Quantitative Social Science: An Introduction. Princeton University Press. [URL](https://press.princeton.edu/books/hardcover/9780691167039/quantitative-social-science) from whence
#' these data were added to `{math300}`.
#' - Mosteller, Frederick. 1997. “The Tennessee Study of Class Size in the Early School Grades.” Bulletin of the American Academy of Arts and Sciences 50(7): 14-25. doi = 10.2307/3824562
#'
"STAR"
