#' Run the left side of the pipeline multiple times.
#'
#' DRAFT DRAFT DRAFT This is intended as a pipeline friendly replacement for mosaic::do().
#' Write a pipeline to perform some calculation whose result can
#' be coerced into one line of a data frame. Add `trials(size=3)` to the end
#' of the pipeline in order to repeat the calculation multiple times. Typically,
#' each trial involves some random component, but another (or an additional)
#' capability is to parameterize the pipeline expression by including some
#' unbound variable in it, e.g. `lambda`. Then call `trials(lambda=c(10,20))` to
#' repeat the calculation for each of the elements of the named parameter.
#'
#' @param .ex (Not user-facing.) The left side of the pipeline.
#' @param size The number of times to run the trial.
#' \ldots Values for any unbound parameter in the left side of the pipeline.
#'
#' @examples
#' mean(rnorm(10)) |> trials(size=3)
#' mean(rnorm(lambda)) |> trials(lambda=c(1, 100, 10000))
#' mean(rnorm(lambda)) |> trials(size=5, lambda=c(1, 100, 10000))
#' sample(mtcars, size=lambda, replace=TRUE) |> select(mpg, hp) |> model_train(mpg ~ shuffle(hp)) |> trials(size=3, lambda=c(10, 20, 40))
#'
#' @export
trials <- function(.ex, size = 1, ...) {
  # capture the left-hand side of pipeline without evaluating it
  left_side <- substitute(.ex)
  # if <left_size> is a name, then the %>% pipe is being used. No good
  if (is.name(left_side)) stop("Cannot use %>% pipe for input to trials(). Use |> instead.")
  # make sure size value is legitimate
  if (size <= 0 || size != round(size))
    stop("<size=> must be a positive integer.")

  # create a function to hold the left-hand side of the pipeline
  fun <- function() {}
  body(fun) <- substitute(.ex)
  params <- c(list(.trial = 1:size), list(...))
  # data frame with all combinations of 1:size and the parameters (if any)
  param_vals <- expand.grid(params, stringsAsFactors = TRUE)
  # set arguments for the function
  formals(fun) <- as.list(param_vals[1, , drop = FALSE])
  # Accumulator for output
  Res <- list()

  # Iterate over all the combinations of 1:size and the parameters
  for (k in 1:nrow(param_vals)) {
    vals <- param_vals[k, , drop = FALSE]
    output <- do.call(fun, vals) |> mosaic:::cull_for_do()
    Res[[k]] <- cbind(vals, output)
  }

  # Condense accumulator into a data frame
  bind_rows(Res)

}
