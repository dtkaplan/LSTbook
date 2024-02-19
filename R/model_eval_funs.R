#' Helper functions to evaluate models
#'
#' Only used internally in `{LSTbook}`. These were originally arranged as S3
#' methods, but now the dispatch is done "by hand" in order to eliminate any
#' exported S3 methods.
#'
#' @param model A model object of the classes permitted
#' @param data Usually, a data table specifying the inputs to the model. But if
#' not specified, the training data will be used.
#' @param interval One of "none", "confidence", or "prediction". Not all model
#' types support "prediction" or even "confidence".
#' @param level (default 0.95) confidence or prediction level. Must be in `[0,1]`
#' @param ... additional arguments
#'
#' @returns a data frame
#'
#'
model_eval_fun <- function(model, data=NULL, interval="none", level=0.95, ...) {
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    model_eval_fun_lm(model, data=data, interval=interval, level=level, ...)
  } else if (inherits(model, "glm")) {
    model_eval_fun_glm(model, data=data, interval=interval, level=level, ...)
  } else {
    stop("The LSTbook package doesn't have access to an evaluation function for this kind of model object.")
  }
}
model_eval_fun_lm <- function(model, data=NULL, interval="none", level=0.95, ...) {
  interval <- match.arg(interval, c("none", "confidence", "prediction"))

  if (is.null(data)) data <- get_training_data(model)

  res <- as.data.frame(
    stats::predict(model, newdata = data, type = "response", interval = interval, level=level )
  )

  if (interval == "none" || ncol(res) == 1)
    names(res) <- ".output"
  else
    names(res) <- c(".output", ".lwr", ".upr")

  tibble::remove_rownames(res)
}

model_eval_fun_randomForest <- function(model, data = NULL, interval="none",
                                      ...) {
  interval <- match.arg(interval,
                        choices = c("none", "confidence", "prediction"))

  if (is.null(data)) data <- get_training_data(model)

  if (model$type == "classification") {
    res <- tibble::remove_rownames(
      as.data.frame(
        stats::predict(model, newdata = data, type = "prob")))
  } else if (model$type == "regression") {
    res <- data.frame(.output =
                        stats::predict(model, newdata = data, type = "response"))
  }

  res
}

model_eval_fun_glm <- function(model, data=NULL, interval="none",
                             level=0.95, ...) {
  interval <- match.arg(interval, choices = c("none", "confidence"))

  if (is.null(data)) data <- get_training_data(model)

  vals <- stats::predict(model, newdata = data,
                  type = "link", se.fit = interval == "confidence")


  two <- stats::qnorm(1 - (1-level)/2)
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


model_eval_fun_rpart <- function(model, data = NULL, interval = "none",
                               level=0.95, # ignored
                               ...) {
  interval <- match.arg(interval, choices = c("none"))

  if (is.null(data)) data <- get_training_data(model)

  if (model$method == "class") { # classifier
    res <- as.data.frame(
      stats::predict(model, newdata = data, type = "prob", ... )
    )
  } else {
    res <- as.data.frame(
      predict(model, newdata = data, ...)
    )
    names(res) <- ".output"
  }

  tibble::remove_rownames(res)
}


# model_eval_fun_randomForest <- function(model, data = NULL, interval = "none",
#                                       level=0.95, # ignored
#                                       ...) {
#   interval <- match.arg(interval, choices = c("none"))
#
#   if (is.null(data)) data <- get_training_data(model)
#
#   if (model$type == "classification") { # classifier
#     res <- as.data.frame(
#       stats::predict(model, newdata = data, type = "prob", ... )
#     )
#   } else {
#     res <- as.data.frame(
#       predict(model, newdata = data, ...)
#     )
#     names(res) <- ".output"
#   }
#
#   tibble::remove_rownames(res)
# }
#
# model_eval_fun_knn3 <- function(model, data = NULL, interval = "none",
#                               level=0.95, #ignored
#                               ...) {
#   interval <- match.arg(interval, choices = c("none"))
#
#   if (is.null(data)) data <- get_training_data(model)
#
#   res <- as.data.frame(
#     stats::predict(model, newdata = data, type = "prob", ... )
#   )
#
#   tibble::remove_rownames(res)
# }
#
# model_eval_fun_train <- function(model, data = NULL, interval = "none",
#                                level=0.95, # ignored
#                                ...) { # caret-package
#   interval <- match.arg(interval, choices = c("none"))
#
#   if (is.null(data)) data <- get_training_data(model)
#
#   if (model$modelInfo$type[1] == "Regression") {
#     res <- as.data.frame(
#       stats::predict(model, newdata = data, type = "raw", ...)
#     )
#     names(res) <- ".output"
#   } else if (model$modelInfo$type[1] == "Classification") {
#     res <- as.data.frame(
#       stats::predict(model, newdata = data, type = "prob" ))
#   } else {
#     stop("Caret model is neither classifier nor regression. LSTbook doesn't know what to do.")
#   }
#
#   tibble::remove_rownames(res)
# }
#
# model_eval_fun_lda <- function(model, data = NULL, interval = "none",
#                              level=0.95, # ignored
#                              ...) {
#   if (is.null(data)) data <- get_training_data(model)
#
#   res <- as.data.frame(stats::predict(model, newdata = data)$posterior)
#
#   tibble::remove_rownames(res)
# }
#
# model_eval_fun_nls <- function(model, data = NULL, interval = "none",
#                              level=0.95, # ignored
#                              ...) {
#   interval <- match.arg(interval, choices = c("none"))
#
#   if (is.null(data)) data <- get_training_data(model)
#
#   res <- data.frame(.output = stats::predict(model, newdata = data))
#
#   tibble::remove_rownames(res)
# }
