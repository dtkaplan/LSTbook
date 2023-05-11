#' Fits models of a variety of types
#'
#' At its simplest, `model_fit()` is a replacement for `group_by() |> summarized()` with
#' only one response variable involved.
#'
#' You can use `var ~ 1` if no stratification is desired.
#'
#' @param data A dataframe: the training data for the model
#' @param specification A tilde expression specifying the model is to be fitted.
#' @param family The type of model to fit: `"gaussian"` (or `"lm"`), `"binomial"`,
#' `"poisson"`, `"svm"`, `"rlm"`.
#' @param wrangle Evaluate on a skeleton, returning a data frame
#' @param \ldots other arguments
#'
#' @export
model_fit <- function(data, specification,
                      family=c("gaussian", "binomial", "poisson", "svm", "lm", "rlm"),
                      wrangle=FALSE, ...) {
  family <- match.arg(family)
  # Specification must be a two-sided formula, response variable on the left.
  if (!inherits(specification, "formula")) stop("Must provide a tilde formula specification.")
  if (length(specification) == 2) stop("Specification must be two sided.")
  if (!(inherits(data, "data.frame") || is.environment(data)))
      stop("Data must be the first argument. You can use a pipe for data.")

  vnames <- all.vars(specification)
  not_in_data <- !vnames %in% names(data)
  if (any(not_in_data)) stop(
    paste("Variable", ifelse(sum(not_in_data)>1, "s", ""),
          paste0("<", vnames[not_in_data], ">", collapse=", "),
          "not in data frame.")
  )

  if (family %in% c("gaussian", "lm", "binomial", "poisson", "rlm")) {
    response <- eval(specification[[2]], envir=data)
    if (!(is.numeric(response) || is.logical(response)))
        stop("Response variable must be quantitative or logical.")
    model <-
      if (family=="rlm") rlm(specification, data=data)
      else if (family %in% c("gaussian", "lm"))
        lm(specification, data=data, ...)
      else glm(specification, data=data, family=family, ...)

    if (wrangle) {
      # return the model evaluated at a skeleton of inputs
      return(model_eval(model, skeleton=TRUE, ncont=5, interval="none"))
    } else {
      return(model)
    }
  }
}
