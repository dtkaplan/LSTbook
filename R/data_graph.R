#' One-step data graphics
#'
#' A simple introduction to data graphics with up to three variables.
#'
#' @param D a data frame
#' @param tilde tilde expression specifying `y ~ x` or `y ~ x + color`
#' @param annot Statistical annotation (one of "none", "violin", "box", "both")
#' @param seed (optional) random seed for jittering
#' @param jitter If `TRUE`, jitter categorical and zero-one variables
#' @param cgrad vector of two color names to set color gradient for continuous color
#' variables.
#'
#' @examples
#' mtcars |> data_graph(mpg ~ wt + cyl)
#' mtcars |> mutate(cyl=as.factor(cyl)) |> data_graph(mpg ~ wt + cyl, annot="both")
#' @export
#'
data_graph <- function(D, tilde, ..., data=NULL, seed=101,
                       annot = c("none", "violin", "box", "both"), jitter=TRUE,
                       cgrad = c("yellow", "darkblue")) {
  annot <- match.arg(annot)
  vars <- all.vars(tilde)
  if (length(vars) > 5)
    stop("data_graphics() can handle at most five variables, you have",
         length(vars), ".")
  if (is.null(data)) {
    if (inherits(D, "ggplot")) {
      # Grab the data from the incoming plot
      data <- D$data
    } else if (inherits(D, "data.frame")) {
      data <- D
      D <- NULL # this must be the first layer of the plot
    } else if (inherits(D, "glm")) {
      data <- data.from.model(D) # Grab the data from the model
      model <- D
      D <- NULL
      warning("data_graphics() not yet set up to take a model as an argument.")
    }
  }

  # Make sure tilde expression and data are compatible
  valid_columns <- vars %in% names(data)
  if (! all(valid_columns) ) {
    message <- paste("Tilde expression includes variables not in data frame:",
                     paste("<", vars[!valid_columns], ">", collapse=", "))
    stop(message)
  }
  # Pull out the variables to be used in the plot
  if (length(vars) >= 1) y_data <- data[[vars[1]]]
  else stop("No variables in tilde expression.")

  xstring <- " "
  if (length(vars) == 1) { # no x variable
    vars[2] <- ".none."
    data[[vars[2]]] <- x_data <- rep(xstring, times=length(y_data))
  } else {
    x_data <- data[[vars[2]]]
  }

  color_data  <- if (length(vars) > 2) data[[vars[3]]] else NULL
  color_discrete <- inherits(color_data, "zero_one") || !is.numeric(color_data)

  if (jitter) { # automatic jittering
    x_jitter <- NULL # default jittering amount
    if (is.numeric(x_data) && !inherits(x_data, "zero_one")) x_jitter=0
    y_jitter <- NULL # default jittering amount
    if (is.numeric(y_data) && !inherits(y_data, "zero_one")) y_jitter=0
  }


  space_formula <- as.formula(paste(vars[1], "~", vars[2]))
  args <- c(list(D, space_formula, data=data, width=x_jitter, height=y_jitter),
            list(...),
            inherit=FALSE)
  if (is.null(D)) args[[1]] <- NULL # eliminate from the argument list
  if (!is.null(color_data)) args$color <- as.formula(paste("~", vars[3]))

  Res <- do.call(gf_jitter, args)
  # Eliminate x-axis label and ticks if not needed.
  if (all(x_data == xstring)) Res <- Res + scale_x_discrete() + xlab("")

  # Add palette if color mapped to a variable
  if ("color" %in% names(args) && inherits(args$color, "formula")) {
    if (color_discrete) {
      Res <- Res +
        scale_color_manual(values=discrete_palette(length(unique(color_data))))
    } else {
      Res <- Res +
        scale_color_gradient(low = cgrad[1], high = cgrad[2])
    }
  }

  # Add a violin if called for
  if (annot %in% c("violin", "both"))
      Res <- Res |> gf_violin(fill="blue", color=NA, alpha=.2)
  if (annot %in% c("box", "both"))
          Res <- Res |> gf_boxplot(color="blue", fill=NA)

  Res

}

discrete_palette <- function(n) {
  fixed <- list(
    c("#1f78b4"),
    c('#a6cee3','#1f78b4'),
    c('#a6cee3','#1f78b4','#b2df8a'),
    c('#a6cee3','#1f78b4','#b2df8a','#33a02c'),
    c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99'),
    c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c'),
    c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f'),
    c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00'),
    c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6'),
    c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')

    )
  if (n <= length(fixed)) fixed[[n]]
  else if (n <= 12) brewer.pal(n, name="Paired")
  else if (n <= 32) c(brewer.pal(12, name="Paired"),
                      brewer.pal(12, name="Set3"),
                      brewer.pal(8, name="Accent)"))[1:n]
  else stop("Can't provide more than 32 discrete colors.")
}
