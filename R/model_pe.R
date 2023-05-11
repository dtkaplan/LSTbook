#' Prediction error
#'
#' Compares model predictions to the actual value of the response variable.
#' To do this, testing data must be provided with *both* the input variables and the
#' corresponding response variable. The measure calculated for a quantitative response
#' variable is the mean square prediction error (MSPE).
#' For categorical response variables, an analog of MSPE can be calculated (see details)
#' but by default, a mean log-likelihood (mean per case) is computed instead.
#'
#' @param model The model whose prediction error is to be estimated.
#' @param testdata A data frame giving both model inputs and the actual value of the response
#' variable. If no testing data is provided, the training data will be used and a warning issued.
#' @param error_type The measure of error you are interested in. By default, this is mean-square error for
#' regression models and log-likelihood for classifiers. The choices are:
#' - `"mse"` -- mean square error
#' - `"sse"` -- sum of square errors
#' - `"mad"` -- mean absolute deviation
#' - `"LL"`  -- log-likelihood
#' - `"mLL"` -- mean log-likehood (per case in the testing data)
#' - `"dev"` -- deviance. (Plus a constant, which is often zero. The constant is fixed for a given testing data set,
#' regardless of the model. So differences between deviances of two models are correct.)
#' - `"class_error"` -- classification error rate.
#'
#' @details When the response variable is categorical, the model
#' (called a 'classifier' in such situations) must be capable of
#' computing *probabilities* for each output rather than just a bare category.
#' This is true for many commonly encountered classifier model architectures.
#'
#' The analog of the mean squared error for classifiers is the mean of (1-p)^2, where p is the
#' probability assigned by the model to the actual output. This is a rough approximation
#' to the log-likelihood. By default, the log-likelihood will be calculated, but for pedagogical
#' reasons you may prefer (1-p)^2, in which case set `error_type = "mse"`. Classifiers can assign a probability
#' of zero to the actual output, in which case the log-likelihood is `-Inf`. The `"mse"` error type avoids this.
#'
#' @examples
#' mod <- lm(mpg ~ hp + wt, data = mtcars)
#' mod_pe(mod) # In-sample prediction error.
#' \dontrun{
#' classifier <- rpart::rpart(Species ~ ., data = iris)
#' model_pe(classifier)
#' model_pe(classifier, error_type = "LL")
#' # More typically
#' inds <- sample(1:nrow(iris), size = 100)
#' Training <- iris[inds, ]
#' Testing  <- iris[ - inds, ]
#' classifier <- rpart::rpart(Species ~ ., data = Training)
#' # This may well assign zero probability to events that appeared in the
#' # Testing data
#' model_pe(classifier, testdata = Testing)
#' model_pe(classifier, testdata = Testing, error_type = "mse")
#' }
#' @export
model_pe <- function(model, testdata,
                      error_type = c("default", "mse", "sse", "mad", "LL", "mLL", "dev", "class_error")) {
  error_type = match.arg(error_type)

  if (missing(testdata)) {
    testdata <- data_from_model(model)
    warning("Calculating error from training data.")
  }
  # error functions
  mse <- function(actual, model_output) {
    mean((actual - model_output)^2, na.rm = TRUE)
  }
  sse <- function(actual, model_output) {
    sum((actual - model_output)^2, na.rm = TRUE)
  }
  mad <- function(actual, model_output) {
    mean(abs((actual - model_output)[[1]]), na.rm = TRUE)
  }
  LL <- function(actual, model_output) {
    probs <- p_of_actual(model_output, actual)
    sum(log(probs))
  }
  mLL <- function(actual, model_output) {
    probs <- p_of_actual(model_output, actual)
    mean(log(probs))
  }
  dev <- function(actual, model_output) {
    -2 * LL(actual, model_output)
  }
  class_error <- function(actual, model_output) {
    # fraction of times the right classification was made
    winner <- max.col(model_output, ties.method = "first")
    winner <- names(model_output)[winner]
    mean(winner != actual, na.rm = TRUE)
  }
  bad_regression_error <- function(...) {
    stop("Invalid error type for regression model.")
  }
  bad_classifier_error <- function(...) {
    stop("Invalid error type for classifier.")
  }
  mean_prob <- function(actual, model_output) { # mean of (1-p)^2
    probs <- p_of_actual(model_output, actual)
    mean((1-probs)^2, na.rm = TRUE)
  }
  sum_prob <- function(actual, model_output) { # mean of (1-p)^2
    probs <- p_of_actual(model_output, actual)
    sum((1-probs)^2, na.rm = TRUE)
  }
  # get the response values from the test data
  actual <- eval(parse(text = response_var(model)), envir = testdata)
  model_vals <- model_eval(model, data = testdata, interval="none")
  if (is.numeric(actual)) {
    if (error_type == "default") {
      #warning('Setting error_type = "mse"')
      error_type <- "mse"
    }
    model_vals <- model_vals[[".output"]] # get just the one column for a regression model
    fun <- switch(tolower(error_type),
                  mse = mse,
                  sse = sse,
                  mad = mad,
                  ll = bad_regression_error,
                  mll = bad_regression_error,
                  dev = bad_regression_error,
                  class_error = bad_regression_error)
    res <- fun(actual, model_vals)
  }
  else {
    if (error_type == "default") {
      #warning('Setting error_type = "LL"')
      error_type <- "LL"
    }
    fun <- switch(tolower(error_type),
                  mse = mean_prob,
                  sse = sum_prob,
                  mad = bad_classifier_error,
                  ll = LL,
                  mll = mLL,
                  dev = dev,
                  class_error = class_error)
    res <- fun(actual, model_vals)
  }
  names(res) <- error_type
  res
}

# Compute the probability corresponding to the actual categorical
# result
p_of_actual <- function(probs, actual_value) {
  res <- numeric(nrow(probs))
  possibilities <- gsub("^model_output.", "", names(probs))
  for (k in 1:length(possibilities)) {
    inds <- actual_value == possibilities[k]
    res[inds] <- probs[inds, k]
  }

  res
}
