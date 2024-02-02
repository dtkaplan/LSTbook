#' 1984 salaries in various professional fields
#'
#' These data were published in the American Association of University Professsor's journal,
#'  *Academe*. There were compiled by Marcia Bellas drawing on data from the 1984 Carnegie
#'  survey of faculty, the US National Science Foundation, the National Research Council, and the US Census
#'  Bureau. The motivation for the work was to investigate salary "disparities among faculty whose education
#'  and experience are comparable and whose duties are broadly similar," in particular those
#'  due to sex. Regretably, the data do not include measures of the production of workers
#'  in the various fields or the numbers of people employed in each field.
#'
#' @usage data(AAUP)
#'
#' @docType data
#'
#' @format 28 rows, each of which is a professional discipline:
#' - `subject` name of the discipline
#' - `ac`: average salary (USD) for academics
#' - `nonacsal` median salary (USD) for non-academics
#' - `fem`: fraction of the workforce that is female
#' - `unemp`: unemployment rate in the discipline
#' - `nonac`: fraction of the workforce that is non-academic,
#' - `licenced`: Does work in the profession require a license (from George Cobb's paper)
#'
#' @references M Bellas & BF Reskin (1994) "On comparable worth" *Academe* **80**:83-85
#'
#' @source George Cobb (2011) "Teaching statistics: some important tensions" *Chilean Journal of Statistics* **2**(1):31-62 [link](https://soche.cl/chjs/volumes/02/01/Cobb(2011).pdf)
"AAUP"
