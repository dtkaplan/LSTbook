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
#' @param type Character string: the type of model to fit
#' @param verbose whether to report on decisions being made
#' @param prob_of which of two levels to use for logistic regression
#' @export
model_train <- function(data, tilde, verbose=FALSE, prob_of = NULL,
                      type = c("auto", "linear", "prob", "counts"), logs=FALSE, ...) {
  type <- match.arg(type)

  response_vals <- eval(tilde[[2]], envir=data)
  # if we already know what type of model ...
  if (type=="linear") {
    mod <- lm(tilde, data = data)
    attr(mod, "training_data") <- data
    class(mod) <- c(class(mod), "model_train")
    return(mod)
  } else if (type == "prob") {
    # make sure response is of the right type
    if (inherits(response_vals, "zero_one") ||
        is.logical(response_vals)) {
      # nothing needs to be done
    } else if (is.numeric(response_vals)) {
      # check that the values are in bounds
      if (max(response_vals) > 1 || min(response_vals) < 0)
        stop("Response variable has values outside the range [0,1]. Not suitable
               for logistic (probability) regression.")
    } else {
      # convert to a zero-one value
      if (!is.null(prob_of) && prob_of %in% unique(response_vals)) {
        response_vals <- zero_one(response_vals, one=prob_of)
      } else {
        response_vals <- zero_one(response_vals)
      }
    }

    # do the logisitic regression on ".response" rather than original name.
    data$.response <- response_vals

    tilde[[2]] <- as.name(".response")
    mod <- glm(tilde, data=data, family="binomial")
    attr(mod, "training_data") <- data
    class(mod) <- c(class(mod), "model_train")
    return(mod)
  } else if (type=="counts") {
    if (min(response_vals, na.rm=TRUE) < 0) stop("Can't do binomial regression on negative response values.")
    mod <- glm(tilde, data = data, family="poisson")
    attr(mod, "training_data") <- data
    class(mod) <- c(class(mod), "model_train")
    return(mod)
  } else if (type=="auto") {
    # Figure out what the response variable is
    response_vals <- na.omit(response_vals)
    if (is.numeric(response_vals)) {
      if (min(response_vals, na.rm=TRUE) >= 0 ) {
        type <- ifelse(max(response_vals <= 1), "prob", "linear")
    } else type = "linear"
    } else if (is.logical(response_vals) || inherits(response_vals, "zero_one")) {
      type = "prob"
    }
    # Make a recursive call to this function
    return(Recall(data, tilde, verbose=verbose, prob_of=prob_of, type=type, logs=logs))
  } else {
    stop("Model type not yet implemented.")
  }




}
