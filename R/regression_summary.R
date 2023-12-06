#' Summaries of regression models
#'
#' The summaries are always in the form of a data frame
#' - `conf_interval()` --- displays coefficients and their confidence intervals
#' - `R2()` --- R-squared of a model together with related measures such as F, adjusted R-squared,
#' the p-value, and degrees of freedom used in calculating the p-value.
#' - `regression_summary()` -- A regression report in data-frame format.
#' - `anova_summary()` --- An ANOVA report in data-frame format. If only one model
#' is passed as an argument, the data frame will have one line per model term. If multiple models
#' are given as arguments, the ANOVA report will show the increments from one model
#' to the next.
#'
#' @details Many of these are  wrappers around `broom::tidy()` used to
#' emphasize to students that the results are a summary in the form of a regression
#' report, similar to the summaries produced by `stats::confint()`, `stats::coef()`, etc.
#'
#' @param mod A model as produced by `model_train()`, `lm()`, `glm()`, and so on
#' @param level Confidence level to use in `conf_interval()` (default: 0.95)
#' @param show_p For `conf_interval()`, append the p-value to the report.
#' @param \ldots One or more models (for ANOVA)
#'
#'
#' @rdname regression_summaries
#' @export
conf_interval <- function(mod, level=0.95, show_p = FALSE) {
  Raw <- stats::confint(mod, level=level)
  Tmp <- stats::coefficients(mod)
  Res <- tibble::tibble(term = row.names(Raw), .lwr = Raw[, 1], .coef=as.numeric(Tmp),  .upr = Raw[, 2])

  if (show_p) { # add the p-values to the report
    Res$p.value <- regression_summary(mod)$p.value
  }
  Res
}
#'
#'
#' @rdname regression_summaries
#' @export
R2 <- function(model) {
  r2 <- mosaic_rsquared(model)
  k <- length(stats::na.omit(model$coefficients)) - 1
  n <- k + 1 + model$df.residual
  f <- ((r2)/k) / ((1-r2)/(n-k-1))
  p <- 1 - stats::pf(f, k, n)
  adj <- 1 - (1 - r2)*(n-1)/(n-k-1)
  data.frame(n=n, k=k, Rsquared=r2, F=f, adjR2=adj, p=p, df.num=k, df.denom=model$df.residual)
}

# Copied from {mosaic} package to avoid dependency

mosaic_rsquared <- function(x, ...) {
  NULLFUN <- function(e) NULL
  result <- tryCatch(x$r.squared, error = NULLFUN)
  if (is.null(result))
    result <- tryCatch(summary(x, ...)$r.squared, error = NULLFUN)
  return(result)
}


#'
#'
#' @rdname regression_summaries
#' @export
regression_summary <- function(model) {
  broom::tidy(model)
}
#'
#'
#' @rdname regression_summaries
#' @export
anova_summary <- function(...) {
  broom::tidy(stats::anova(...))
}

#' A convenience function to calculate Rsquared and F from a model, along with
#' the degrees of freedom in the numerator and denominator


