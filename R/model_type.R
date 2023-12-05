#' Check model type against model specification and data
#'
#' This can be used to automatically determine a model type or to determine if the specified
#' model type is consistent with the specification/data
#'
#' @param .data A data frame or equivalent
#' @param .tilde A model specification as a tilde expression
#' @param family Requested model type, if any.
#'
model_family <- function(.data, .tilde, family = c("auto", "lm", "binomial", "poisson", "svm", "gaussian", "rlm")) {
  family <- match.arg(family)
  # Specification must be a two-sided formula, response variable on the left.
  if (!inherits(.tilde, "formula")) stop("Must provide a tilde formula specification.")
  if (length(.tilde) == 2) stop("Specification must be two sided.")

  # get the response variable
  data <- data_from_tilde(.data, .tilde)[[1]]


  if (inherits(data, "zero_one")) allowable <- c("binomial", "lm")
  else if (is.numeric(data) || is.logical(data)) {
    # It's a numeric type
    allowable <- c("lm", "gaussian", "rlm", "svm")
    if (all(data >= 0, na.rm=TRUE)) {
      if (all(data <= 1, na.rm = TRUE) &&
          diff(range(data, na.rm = TRUE)) == 1) {
        allowable <- c("binomial", allowable)
      } else if (all(data == round(data), na.rm = TRUE)) {
        allowable <- c(allowable, "poisson")
      }
    }
  } else {
    # It's categorical
    allowable <- "svm"
    if (length(unique(data)) == 2) allowable <- c("binomial", allowable)
    else whats_wrong <- "not two level categorical variable."
  }

  if (family == "auto") return(allowable) # return names of allowable model families.
  else if (family %in% allowable) return(family) # the selected model family works!
  else stop("Model type not allowed: ", not_allowed(data, family)) # something not anticipated

}

# Attempt to explain why
not_allowed <- function(data, family) {
  if (is.numeric(data) || is.logical(data)) {
    if (family == "binomial") "response not in numerical range [0,1]"
    else if (family=="poisson") "response not non-negative integers"
    else glue::glue("software error. Model type {family} not anticipated.")
  } else {
    if (family == "binomial") "categorical response does not have *two* levels"
    else if (family %in% c("lm", "gaussian", "rlm", "svm"))
      glue::glue("can't use model type {family} with categorical response variable")
    else glue::glue("software error. Model type {family} not anticipated.")
  }
}
