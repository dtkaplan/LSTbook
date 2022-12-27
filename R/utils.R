#' Utilities
#'
#' These are mostly pulled from the original mosaicModel package
#'
#' @param model the model in question
#' @param ... (not used)
#'
#' @rdname utils
explanatory_vars <- function(model, ...) {
  all.vars(formula_from_mod(model)[[3]])
}
#' @rdname utils
response_var <- function(model, ...) {
  formula_from_mod(model)[[2]]
}
#' @rdname utils
response_values <- function(model, ...) {
  eval(parse(text = response_var(model)), envir = data_from_mod(model))
}
#' @rdname utils
formula_from_mod <- function(model, ...) {
  if ("terms" %in% names(model)) return(formula(model$terms))
  if ("Terms" %in% names(model)) return(formula(model$Terms))
  stop("Model architecture '", class(model), "' not recognized by mosaicModel package.")
}
