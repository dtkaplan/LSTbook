#' Utilities
#'
#' Functions for pulling various components from model objects.
#' These work mainly for lm and glm objects. It's a future project
#' to add facilities for other object types.
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
  eval(parse(text = response_var(model)), envir = get_training_data(model))
}
#' @rdname utils
formula_from_mod <- function(model, ...) {
  if ("terms" %in% names(model)) return(stats::formula(model$terms))
  if ("Terms" %in% names(model)) return(stats::formula(model$Terms))
  stop("Model architecture '", class(model), "' not recognized by mosaicModel package.")
}

#' @rdname utils
get_training_data <- function(model, ...) {
  if ("training_data" %in% names(attributes(model)))
    attributes(model)$training_data
  else {
    error_string <- paste0("Model architecture '",
                           paste(class(model), collapse = "', "),
                           "' not recognized by LSTbook.")
    stop(error_string)
  }
}
