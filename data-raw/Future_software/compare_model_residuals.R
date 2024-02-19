#' Compare models based on the size of their residuals
#'
#' Compare a set of models, all with the same response variable, using either
#' RMS residuals or the sum-of-square residuals
#'
#' @param source A data frame
#' @param \ldots One or more model specifications, all with the same response variable.
#' @param measure Either of `"RMS"` (the default) or `"SS"` (sum of squares)
#' @param n Sample size from DAG.
#' @param testing Character string, either `"in-sample"` or `"out-of-sample"`
#'
#'
#' @examples
#' compare_model_residuals(mtcars,
#'   mpg ~ 1, mpg ~ hp, mpg ~ hp + wt,
#'   measure="SS")
#'   # This won't work until cross-validation is set up
#' # compare_model_residuals(sample(sim_07, n=100),
#'   # c ~ 1, c ~ a, c ~ a + b, c ~ a + b + d,
#'   # measure="SS", testing="out-of-sample")

#' @export
compare_model_residuals <- function(source, ...,  n=500,
                                    measure = c("RMS", "SS", "R2"),
                                    testing = c("in-sample", "out-of-sample")) {
  # collect the models
  models <- list(...)
  testing <- match.arg(testing)
  measure <- match.arg(measure) # make sure it's one of the allowed possibilities
  # make sure they all have the same response variable
  responses <- unique(unlist(lapply(models, function(x) all.names(x[[2]]))))
  if (length(responses) > 1) stop("All model specifications must have the same response variable.")
  else response <- as.name(responses) # convert to a name so it can be inserted in the formula
  if (inherits(source, "datasim")) {
    Training <- sample(source, n=n)
    if (testing == "in-sample") Testing <- Training
    else Testing <- sample(source, n=2*n) # a bit bigger, because we can
  } else {
    if (testing != "in-sample") {
      message("Cross-validation being used.")
      stop("Need to implement cross-validation.")
    }

    Training <- Testing <- source
  }
  res <- numeric(length(models))
  for (k in 1:length(models)) {
    mod <- Training |> model_train(models[[k]])
    Mod_evaluated <- model_eval(mod, data = Testing)
    if (measure == "RMS") {
      res[k] <- sqrt(mean(Mod_evaluated$.resid^2, na.rm=TRUE))
    } else if (measure == "SS") {
      res[k] <- sum(Mod_evaluated$.resid^2, na.rm=TRUE)
    } else if (measure == "R2") {
      res[k] <- stats::var(Mod_evaluated$.output, na.rm=TRUE) /
                   stats::var(Mod_evaluated[, 1], na.rm=TRUE)
    } else
        stop("Should never get here! Report this message to package maintainer.")
  }
  return(res)
}
