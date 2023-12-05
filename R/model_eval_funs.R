#' Helper functions to evaluate models
#'
#' Only used internally in `{LST}`
#' @param model A model object of the classes permitted
#' @param data Usually, a data table specifying the inputs to the model. But if
#' not specified, the training data will be used.
#' @param interval One of "none", "confidence", or "prediction". Not all model
#' types support "prediction" or even "confidence".
#' @param ... additional arguments
#'
#'
#' @details All of the `eval_` functions are ex
#' These functions return a numerical vector (for regression types) or
#' a matrix of probabilities (for classifiers)
model_eval_fun <- function(model, data=NULL, interval="none", level=0.95, ...) {
  UseMethod("model_eval_fun")
}
#' @exportS3Method
model_eval_fun.default <- function(model, data=NULL, interval="none", level=0.95, ...) {
  stop("The LST package doesn't have access to an evaluation function for this kind of model object.")
}
#' @exportS3Method
model_eval_fun.lm <- function(model, data=NULL, interval="none", level=0.95, ...) {
  interval <- match.arg(interval, c("none", "confidence", "prediction"))

  if (is.null(data)) data <- get_training_data(model)

  res <- as.data.frame(
    predict(model, newdata = data, type = "response", interval = interval, level=level )
  )

  if (interval == "none" || ncol(res) == 1)
    names(res) <- ".output"
  else
    names(res) <- c(".output", ".lwr", ".upr")

  tibble::remove_rownames(res)
}

#' @exportS3Method
model_eval_fun.randomForest <- function(model, data = NULL, interval="none",
                                      ...) {
  interval <- match.arg(interval,
                        choices = c("none", "confidence", "prediction"))

  if (is.null(data)) data <- get_training_data(model)

  if (model$type == "classification") {
    res <- tibble::remove_rownames(
      as.data.frame(
        predict(model, newdata = data, type = "prob")))
  } else if (model$type == "regression") {
    res <- data.frame(.output =
                        predict(model, newdata = data, type = "response"))
  }

  res
}

#' @exportS3Method
model_eval_fun.glm <- function(model, data=NULL, interval="none",
                             level=0.95, ...) {
  interval <- match.arg(interval, choices = c("none", "confidence"))

  if (is.null(data)) data <- get_training_data(model)

  vals <- predict(model, newdata = data,
                  type = "link", se.fit = interval == "confidence")


  two <- qnorm(1 - (1-level)/2)
  if (interval == "confidence") {
    res <- data.frame(.output = model$family$linkinv(vals$fit),
                      .lwr = vals$fit - two * vals$se.fit,
                      .upr = vals$fit + two * vals$se.fit)
    res$.lwr <- model$family$linkinv(res$.lwr)
    res$.upr <- model$family$linkinv(res$.upr)
  } else {
    names(vals) <- NULL # strip off case labels
    res <- data.frame(.output = model$family$linkinv(vals))
  }


  tibble::remove_rownames(res)
}

#' @exportS3Method
model_eval_fun.rpart <- function(model, data = NULL, interval = "none",
                               level=0.95, # ignored
                               ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- get_training_data(model)

  if (model$method == "class") { # classifier
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob", ... )
    )
  } else {
    res <- as.data.frame(
      predict(model, newdata = data, ...)
    )
    names(res) <- ".output"
  }

  tibble::remove_rownames(res)
}

#' @exportS3Method
model_eval_fun.randomForest <- function(model, data = NULL, interval = "none",
                                      level=0.95, # ignored
                                      ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- get_training_data(model)

  if (model$type == "classification") { # classifier
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob", ... )
    )
  } else {
    res <- as.data.frame(
      predict(model, newdata = data, ...)
    )
    names(res) <- ".output"
  }

  tibble::remove_rownames(res)
}

#' @exportS3Method
model_eval_fun.knn3 <- function(model, data = NULL, interval = "none",
                              level=0.95, #ignored
                              ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- get_training_data(model)

  res <- as.data.frame(
    predict(model, newdata = data, type = "prob", ... )
  )

  tibble::remove_rownames(res)
}

#' @exportS3Method
model_eval_fun.train <- function(model, data = NULL, interval = "none",
                               level=0.95, # ignored
                               ...) { # caret-package
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- get_training_data(model)

  if (model$modelInfo$type[1] == "Regression") {
    res <- as.data.frame(
      predict(model, newdata = data, type = "raw", ...)
    )
    names(res) <- ".output"
  } else if (model$modelInfo$type[1] == "Classification") {
    res <- as.data.frame(
      predict(model, newdata = data, type = "prob" ))
  } else {
    stop("Caret model is neither classifier nor regression. LST doesn't know what to do.")
  }

  tibble::remove_rownames(res)
}

#' @exportS3Method
model_eval_fun.lda <- function(model, data = NULL, interval = "none",
                             level=0.95, # ignored
                             ...) {
  if (is.null(data)) data <- get_training_data(model)

  res <- as.data.frame(predict(model, newdata = data)$posterior)

  tibble::remove_rownames(res)
}
#' @exportS3Method
model_eval_fun.qda <- model_eval_fun.lda


#' @exportS3Method
model_eval_fun.nls <- function(model, data = NULL, interval = "none",
                             level=0.95, # ignored
                             ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- get_training_data(model)

  res <- data.frame(.output = predict(model, newdata = data))

  tibble::remove_rownames(res)
}
