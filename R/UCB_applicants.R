#' Roster of applicants to six major departments at UC Berkeley
#'
#' @docType data
#'
#' @details In 1973, officials at the University of California Berkeley noticed
#' disturbing trends in graduate admissions rates. The data, with department names redacted,
#' was presented and interpretted in a famous paper in *Science*, Bickel et al. 1975. In
#' that paper, summary tables were presented. `UCB_applicants` was reverse
#' engineered from `datasets::UCBAdmissions` into
#' a data table where the unit of observation is an individual applicant. The origin of
#' `datasets::UCBAdmissions` is not clear; those data are not explicitly provided in Bickel et al.
#'
#'
#' @usage data(UCB_applicants)
#'
#' @format A data.frame object with 4236 rows, one for each of the applicants to
#' graduate school at UC Berkeley for the Fall 1973 quarter.
#'
#' - `admit`: Whether the applicant was admitted.
#' - `gender`: male or female
#' - `dept`: The graduate department applied to. Rather than identifying the actual
#' departments involved, the data released by Berkeley used letter codes.
#'
#' @keywords datasets
#'
#' @references
#' Bickel, P. J., Hammel, E. A., and O'Connell, J. W. (1975).
#' Sex bias in graduate admissions: Data from Berkeley. Science, 187, 398–403. \href{https://www.jstor.org/stable/1739581}{link to JSTOR}.

#' @source The `UCBApplicants` summary table in the `datasets` R package.
"UCB_applicants"
