#' train a model, easily
#'
#' An interface to several of the most often used model-fitting routines
#' designed to make it easy to construct.
#'
#' @details Since data may be piped into this function, the training data
#' frame will be called simply "data", the name of the first argument to this
#' function. In order to be able to access the training data in such cases, the
#' training data is assigned to an attribute of the resulting model, "training_data".
#'
#' @param data Data frame to use as training data
#' @param tilde Formula for the model
#' @param family Character string: the family of model to fit, e.g. "lm", "binomial", "poisson", "rlm", ...
#' @export
model_train <- function(data, tilde, prob_of = NULL,
                      family = "auto", logs=FALSE, ...) {
  # Figure out (or confirm) the model family
  mod_family <- model_family(data, tilde, family)[1]


  if (mod_family == "lm") {
    mod <- stats::lm(tilde, data = data)
  } else if (mod_family == "binomial") {
    mod <- stats::glm(tilde, data= data, family="binomial")
  } else if (mod_family == "poisson") {
    mod <- stats::glm(tilde, data = data, family="poisson")
    return(mod)
  } else if (mod_family == "rlm") {
    mod <- MASS::rlm(tilde, data = data)
  } else {
    stop(glue::glue("Model type {mod_family} not yet available."))
  }

  attr(mod, "training_data") <- data
  class(mod) <- c(class(mod), "model_train")

  mod
}
