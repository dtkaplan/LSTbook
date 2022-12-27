#' Generate a regression or ANOVA report in the form of a data frame.
#'
#' @details These are simply wrappers around `broom::tidy()` used to
#' emphasize to students that the results are a summary in the form of a regression
#' report, similar to the summaries produced by `confint()`, `coef()`, etc.
#'
#' @param model A model as produced by `lm()`, `glm()`, and so on
#'
#' @export
regression_summary <- function(model, ...) {
  broom::tidy(model)
}

#' @rdname regression_summary
#' @export
anova_summary <- function(...) {
  broom::tidy(anova(...))
}

#' @rdname regression_summary
#' @export
coef_summary <- function(mod) {
  Tmp <- stats::coefficients(mod)

  data.frame(term = names(Tmp),
             coefficient=as.numeric(Tmp))
}

#' @rdname regression_summary
#' @export
conf_interval <- function(mod, level=0.95) {
  Raw <- confint(mod, level=level)

  tibble::tibble(term = row.names(Raw), .lwr = Raw[, 1], .upr = Raw[, 2])
}
