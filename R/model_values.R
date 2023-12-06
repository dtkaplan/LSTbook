#' Construct a model and return the model values
#'
#' One-stop shopping to fit a model and return the model output on the training data.
#'
#' @param data A data frame containing the training data. When used with `mutate()`, `data` will
#' hold the model specification, instead of `tilde`.
#' @param tilde A model specification in the form of a tilde expression
#' @param family The type of model architecture: `"linear"`, `"prob"`, or `"counts"`
#'
#' @details This is intended to be used ONLY WITHIN `mutate()`
#'
#' @returns A **vector** (not a data frame) of the model evaluated on the training data.
#' This is intended mainly for use within `mutate()`, so that a general model can be used in
#' the place of simple reduction verbs like `mean()`, `median()`
#'
#' @examples
#' mtcars |> mutate(mpg_mod = model_values(mpg ~ hp + wt)) |> select(hp, wt, mpg_mod) |> head()
#'
#' @export
model_values <- function(data, tilde, family=c("linear", "prob", "counts")) {
  family <- match.arg(family)
  # Figure out if we are inside `mutate()`
  in_mutate <- try(any(grepl("mutate", deparse(sys.calls()))))
  if (inherits(in_mutate, "try-error")) in_mutate <- FALSE
  if (in_mutate) {
    tilde <- data # we don't need the data argument
    vnames <- all.vars(tilde)
    E <- parent.frame()$.top_env
    missing <- vnames[!all(vnames %in% names(E))]
    if (length(missing) !=  0) {
      msg = paste("Modeling variable(s)",
                  paste0("'", missing, "'", collapse=" & "),
                  "not in data frame.")
      stop(msg)
    }
    df_str <- paste("tibble(",
                    paste(vnames, "=", vnames, collapse=", "),
                    ")")
    data <- eval(parse(text=df_str), envir=E)
  } else {
    data <- data
  }

  model <-
    if (family=="linear") lm(tilde, data=data, na.action=na.exclude)
    else if (family=="prob") glm(tilde, data=data, family="binomial", na.action=na.exclude)
    else if (family=="counts") glm(tilde, data=data, family="poisson", na.action=na.exclude)
    else stop("Model type not recognized.")

  model |> predict(type="response") |> as.vector()

}
