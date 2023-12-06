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
  eval(parse(text = response_var(model)), envir = get_training_data(model))
}
#' @rdname utils
formula_from_mod <- function(model, ...) {
  if ("terms" %in% names(model)) return(formula(model$terms))
  if ("Terms" %in% names(model)) return(formula(model$Terms))
  stop("Model architecture '", class(model), "' not recognized by mosaicModel package.")
}

#' @rdname utils
get_training_data <- function(model, ...) {
  if ("training_data" %in% names(attributes(model))) attributes(model)$training_data
  else {
    # Get what you can from the model frame
    # but this will have transformed variables
    data_from_model(model)
  }
}

#' Extract training data from model
#'
#' This typically will *not* be used by an end-user.
#'
#' @param model the model from which to extract the training data
#' @param ... additional arguments (not used)
#'
#' @details not all model architectures keep track of the training data
#' If a model architecture isn't recognized, you'll have to add a method for that class. See vignette.
#'
data_from_model <- function(model, ...) {
  UseMethod("data_from_model")
}
#' @exportS3Method
data_from_model.lm <- function(model, ...) get_training_data(model)

#' @exportS3Method
data_from_model.glm <- function(model, ...) get_training_data(model)


data_from_model.default <- function(model, ...) {
  error_string <- paste0("Model architecture '",
                         paste(class(model), collapse = "', "),
                         "' not recognized by LST.")
  stop(error_string)
}

#' @exportS3Method
data_from_model.knn3 <- function(model, ...) {
  res <- data.frame(model$learn$y, model$learn$X)
  names(res)[1] <- as.character(response_var(model))

  res
}

#' Construct a call for refitting a model from the model itself
#'
#' This will typically *not* be used by the end-user.
#'
#' @param model the model in question
#' @param data_name character string specifying the name of the data
#' frame used for the refitting. This object *must* be defined in the environment in which the
#' call is being made.
#
#' @param ... (not used)
#'
#'
#' @details This provides a way to refit a model on either resampled or sub-sampled data.
#' Not all model architectures support this. If not, then you can't use `model_ensemble` or `model_cv`,
#' or use the `bootstrap=` argument in any of the other functions.
#'
construct_fitting_call <- function(model, data_name = "training", ...) {
  UseMethod("construct_fitting_call")
}

construct_fitting_call.default <- function(model, data_name, ...) {
  # set up the call for fitting the model to the training data
  if (! "call" %in% names(model))
    stop("No 'call' component to model, so the model can't be retrained.")
  architecture <- model$call[[1]]
  fit_call <- model$call
  if (data_name != "") fit_call[["data"]] <- as.name(data_name)

  fit_call
}

construct_fitting_call.knn3 <- function(model, data_name = "training", ...) {
  res <- call("knn3")
  formula <- model$terms
  attributes(formula) <- NULL
  res[[2]] <- formula
  res[["data"]] <- as.name(data_name)
  res[["k"]] <- model$k

  res
}


