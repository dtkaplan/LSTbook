#' Construct a classifier function from a formula
#'
#' A classifier function takes a data set as input and adds a new column showing
#' how each row would be classified. The classification itself is just a calculation
#' based on the values in that row. To create the classifier function you provide a formula
#' description of the calculation
#'
#' @param tilde A tilde expression. The left-hand side is the name for the new column,
#' the right-hand side is an expression written in terms of the names in the data frame
#' that will be handed to the classifier function constructed by `make_classifier()`.
#'
#' @returns A function that takes a data frame as input and returns the same data frame
#' but with a new column added that gives the result of the classifier calculation. The new
#' column will have levels "+" or "-".
#'
#' @examples
#' # If the sum of a and b is positive, the classification will be +.
#' cfun <- make_classifier(output ~ (a + b) > 0)
#' @export
make_classifier <- function(tilde) {
  out_name <- all.vars(tilde[[2]]) # the left-hand side of the tilde sets the name of the output
  calculation <- tilde[[3]] # just the right side matters
  ftext <- glue::glue("function(data) {{
    data['{out_name}'] <-
    ifelse(eval(quote({deparse(calculation)}), envir=data), '+', '-')

    return(data)
  }}")

  eval(parse(text = ftext)) # return the function defined by ftext
}
