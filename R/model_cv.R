#' Compare models with k-fold cross validation
#'
#' @param ... one or more models on which to perform the cross validation
#' @param k the k in k-fold. cross-validation will use k-1/k of the data for training.
#' @param ntrials how many random partitions to make. Each partition will be one case in the
#' output of the function
#' @param error_type The kind of output to produce from each cross-validation. See \code{\link{model_pe}} for details.
#' @param blockwise When TRUE carries out the trials in a blockwise mode, suitable for time series.
#'
#' @details The purpose of cross-validation is to provide "new" data on which to test a model's
#' performance. In k-fold cross-validation, the data set used to train the model is broken into
#' new training and testing data. This is accomplished simply by using most of the data for training while
#' reserving the remaining data for evaluating the model: testing. Rather than training a single model, k models
#' are trained, each with its own particular testing set. The testing sets in the k models are arranged to cover the
#' whole of the data set. On each of the k testing sets, a performance output is calculated. Which output is
#' most appropriate depends on the kind of model: regression model or classifier. The most basic measure is the mean square error: the
#' difference between the actual response variable in the testing data and the output of the model
#' when presented with inputs from the testing data. This is appropriate in many regression models.
#'
#' The blockwise mode is intended for time-series like data, where successive rows may be correlated. Rather than
#' picking each k block as a random sample of rows, adjacent rows will be used for model evaluation.
#'

#'
#' @export
model_cv <- function(..., k = 10, ntrials = 5,
                   error_type = c("default", "mse", "sse", "mad", "LL", "mLL", "dev", "class_error"),
                   blockwise = FALSE) {
  error_type = match.arg(error_type)

  # Get just the names of the models
  full_names <- as.character(lapply(lazyeval::lazy_dots(...), FUN = function(x) x$expr))
  # Now for the models themselves
  models <- list(...)
  # model can be a list. If so, repeat over all the models.

  result = NULL
  for (counter in 1:length(models)) {
    this_mod <- models[[counter]]
    truth <- response_values(this_mod)

    if (blockwise) {
      pred_error_results <- kfold_block(this_mod, k = k, type = error_type)
      error_type <- names(pred_error_results)[1] # get the actual name, not "default"
    } else {
      pred_error_results <- numeric(ntrials)
      for (this_trial in 1:ntrials) {
        # get the model outputs for each test group against
        # the rest of the data
        tmp <- kfold_trial(this_mod, k = k, type = error_type)
        if (this_trial == 1) error_type <- names(tmp)[1]
        pred_error_results[this_trial] <- tmp
      }
    }
    from_this_mod <- data.frame(pred_error_results, model = full_names[counter],
                                stringsAsFactors = FALSE)
    names(from_this_mod)[1] <- error_type

    result <- rbind(result, from_this_mod)
  }


  result
}

# returns a vector of predictions or likelihoods
kfold_trial <- function(model, k=10, type) {
  # Grab the data and the call from the model.
  data <- get_training_data(model)
  # For cross validation, we don't want the constructed terms
  constructed <- grep("\\(.*\\)", names(data))
  if (length(constructed) > 0) data[[constructed]] <- NULL # get rid of them


  fit_call <- construct_fitting_call(model, data_name = "training")
  # construct the groups for the k-fold divisions
  groups <- sample(rep(1:k, length.out = nrow(data) ))
  # Create a holder for the result
  # output <- evaluate_model(model, data = data, type = type)
  output <- rep(NA_real_, nrow(data))

  for (group in 1:k) {
    training <- data[group != groups, , drop = FALSE ]

    testing  <- data[group == groups, , drop = FALSE ]

    this_model <- try(eval(fit_call), silent = TRUE)
    if ( ! inherits(this_model, "try-error")) {
      tmp <- model_pe(this_model, testdata = testing, error_type = type)

      output[group == groups] <- tmp
    }
  }

  res = mean(output, na.rm = TRUE)
  names(res) <- names(tmp)[1] # name the result after the type of error actually used

  res
}

# returns a vector of predictions or likelihoods
kfold_block <- function(model, k=10, type) {
  # Grab the data and the call from the model.
  data <- data_from_model(model)
  # For cross validation, we don't want the constructed terms
  constructed <- grep("\\(.*\\)", names(data))
  if (length(constructed) > 0) data[[constructed]] <- NULL # get rid of them


  fit_call <- construct_fitting_call(model, data_name = "training")
  # construct the groups for the k-fold block divisions
  groups <- ceiling(seq(0.00001, k, length = nrow(data))) # e.g. 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, ...

  # Create a holder for the result
  # output <- evaluate_model(model, data = data, type = type)
  output <- rep(NA_real_, k)

  for (group in 1:k) {
    training <- data[group != groups, , drop = FALSE ]

    testing  <- data[group == groups, , drop = FALSE ]

    this_model <- try(eval(fit_call), silent = TRUE)
    if ( ! inherits(this_model, "try-error")) {
      tmp <- model_pe(this_model, testdata = testing, error_type = type)
      output[group] <- tmp
    }
  }

  res = data_frame(x = output)
  names(res) <- names(tmp)[1] # name the result after the type of error actually used

  res
}

# helper for reference levels
# get an appropriate set of levels
n_levels <- function(values, n) {
  var_name <- substitute(values)
  n <- pmax(ceiling(abs(n)), 1)
  unique_vals <- unique(values)
  if (n == Inf) { # flag for "all levels". But don't go crazy if variable is quantitative
    res <- if (is.numeric(values)) { # enough to make a nice plot
      if (length(unique_vals) < 10) unique_vals
      else seq(min(values, na.rm = TRUE),
               max(values, na.rm = TRUE), length = 100)
    } else {
      as.character(unique_vals) # all categorical levels
    }
    return(res)
  }

  # finite number of categorical levels
  if ( ! is.numeric(values)) {
    level_names <- names(sort(table(values), decreasing = TRUE))
    return( level_names[1:pmin(n, length(level_names))] )
  }

  # finite number of numerical levels
  if (is.numeric(values)) {
    med <- stats::median(values, na.rm = TRUE)
    if (n == 1) {
      return(signif(med, 2))
    } else {
      res <- pretty(quantile(values, c(.1, .9), na.rm = TRUE), n-1)
      if (n == 2) res <- res[c(1, length(res))]
      if (length(res) > n) res <- res[-1]
      if (length(res) > n) res <- res[-length(res)]
      return(res)
    }
    # outliers <- has_outlier(values)
    # order_of_magnitude <- 0
    # common_digits <- range(log10(abs(unique_vals[unique_vals != 0])))
    # if (1 > diff(common_digits) && sign(min(unique_vals)) == sign(max(unique_vals))) {
    #   order_of_magnitude <- sign(max(unique_vals)) * signif(10^mean(common_digits), floor(-log10(diff(common_digits))))
    # }
    # to_two_digits <- signif(values - order_of_magnitude, 2L)
    #
    # trim <- ifelse(n < 10, .1, ifelse(n > 100, .01, 0.05))
    # # if no outliers, do the whole range
    # if ( ! any(outliers)) where <- seq(0, 1, length = n)
    # else if (all(outliers)) where <- seq(trim, 1-trim, length = n)
    # else if (outliers[1]) where <- seq(trim, 1, length = n)
    # else where <- seq(0, 1-trim, length = n)
    #
    # candidate1 <- quantile(to_two_digits, where, type = 3, na.rm = TRUE)
    #
    # return(unique(candidate1 + order_of_magnitude))
  }

  stop("\"", var_name, "\" is neither numerical nor categorical. Can't figure out typical levels.")

}

# look for pretty extreme outliers
# return logicals: is the minimum an outlier? is the maximum an outlier?
has_outlier <- function(values, whisker = 3) {
  box <- as.numeric(quantile(values, probs = c(.25, .75), na.rm = TRUE))
  c(min(values, na.rm=TRUE) < box[1] - diff(box) * whisker,
    max(values, na.rm=TRUE) > box[2] + diff(box) * whisker )

}


get_step = function(ref_vals, change_var, data, step = NULL, from = NULL) {
  if (is.null(step)) { # Need to set the step
    vals <- data[[change_var]]

    if (is.numeric(vals)){
      step <- pretty(c(.5, 1.5) * sd(vals, na.rm = TRUE), 2)[2]
    } else {
      if ( ! is.null(from))
        vals <- vals[ ! vals %in% from]
      else {
        vals <- vals[ ! vals %in% ref_vals[[change_var]]]
        if (length(vals) == 0) vals <- data[[change_var]]
      }
      step <- names(sort(table(vals), decreasing = TRUE))[1]
    }
  }
  step
}

# separate ... into the components that match explanatory variables ($at)
# and those that don't ($extras)

handle_dots_as_variables <- function(model, ...) {
  xvars <- base::union(explanatory_vars(model), names(get_training_data(model)))
  All <- list(...)
  res <- list()
  res$at <- All[names(All) %in% xvars]
  res$extras <- All[ ! names(All) %in% xvars]

  res
}


