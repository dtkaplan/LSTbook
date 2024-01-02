#' A printing method for model objects
#'
#' @export
print.model_object <- function(model) {
  response_name <- as.character(deparse(response_var(model)))
  explanatory_names <- explanatory_vars(model)
  plural <- ifelse(length(explanatory_names) > 1, "s", "")

  str <- glue::glue("A trained model relating the response variable \"{response_name}\"\nto explanatory variable{plural} {paste0('\"', explanatory_names, '\"', collapse=' & ')}.\n\nTo see relevant details, use model_eval(), conf_intervals(),\nR2(), regression_summary(), anova_summary(), or model_plot(),\nor the native R model-reporting functions.")
  cat(str)
  invisible(model)
}
