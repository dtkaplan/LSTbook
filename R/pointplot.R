#' One-step data graphics
#'
#' `pointplot()` makes it easy to construct an informative basic graph of a
#' data frame. "Making it easy" means that the user only needs to specify
#' two things: 1) the data frame to be used and 2) a tilde expression with the response variable on the left and up to
#' three explanatory variables on the right. The response variable is mapped to
#' the vertical axis while the first explanatory variable defines
#' the horizontal axis. The second explanatory variable (if any) maps to color,
#' the third (if any) defines facets. Quantitative variables used for color or faceting
#' are cut into categorical variables, so color and facets will always be discrete.
#'
#' When an x- or y- variables is categorical, jittering is automatically applied.
#'
#' Using `annot = "model"` will annotate the data with the graph of a
#' model --- shown as confidence intervals/bands --- corresponding to
#' the tilde expression. `annot = "violin"` will annotate with a violin plot.
#'
#' If you want to use the same explanatory variable for color and faceting
#' (this might have pedagogical purposes) merely repeat the name of the color variable
#' in the faceting position, e.g. `mpg ~ hp + cyl + cyl`.
#'
#' @param D a data frame
#' @param tilde tilde expression specifying `y ~ x` or `y ~ x + color`
#' @param annot Statistical annotation (one of "none", "violin", "model")
#' @param model_alpha Transparency for the model annotation
#' @param palette Depending on taste and visual capabilities, some people might
#' prefer to alter the color scheme. There are 8 palettes available: `"A"` through `"H"`.
#' @param seed (optional) random seed for jittering
#' @param jitter Options for turning on jitter: one of `"default"`, `"both"`, `"none"`, `"x"`, `"y"`. By default,
#' By default, categorical variables are jittered.
#' @param bw bandwidth for violin plot
#' @param level confidence level to use (0.95)
#' @param ... Graphical options for the data points, e.g. alpha, size
#'
#' @examples
#' Galton |> pointplot(height ~ mother + sex + father, annot="model", model_alpha=1)
#' mtcars |> pointplot(mpg ~ wt + cyl)
#' mtcars |> pointplot(mpg ~ wt + cyl + hp, annot="model")
#' @export
pointplot <- function(D, tilde, ..., data=NULL, seed=101,
                       annot = c("none", "violin", "model"),
                       jitter = c("default", "none", "all", "x", "y"),
                       model_alpha = 0.2, palette=LETTERS[1:8], bw = NULL, level=0.95) {
  annot <- match.arg(annot)
  palette <- match.arg(palette)
  jitter <- match.arg(jitter)
  set.seed(seed)

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
      warning(as.character(match.call()[[1]]), " not yet set up to take a model as an argument.")
    }
  }

  data <- data_from_tilde(data, tilde)
  y_data <- data[[1]]
  x_data <- data[[2]]
  x_is_discrete <- continuous_or_discrete(x_data) == "discrete"
  y_is_discrete <- continuous_or_discrete(y_data) == "discrete"
  vars <- names(data)
  if (length(vars) > 4)
    stop("data_graphics() can handle at most four variables, you have",
         length(vars), ".")

  # Convert color and faceting (columns 3 and 4) to discrete values

  if (ncol(data) > 2) {
    columns <- 3:ncol(data)
    for (k in columns) {
      if  (continuous_or_discrete(data[[k]]) == "continuous") {
        if (length(unique(data[[k]])) < 5) {
          data[[k]] <- as.factor(data[[k]])
        } else {
          data[[k]] <- ntiles(signif(data[[k]],2), 3, format="interval")
        }
      }
    }
  }

  default_jitter <- 0.15 # default jitter size for categorical variables

  if (jitter == "default") {
    x_jitter <- ifelse(is.numeric(x_data) && !inherits(x_data, "zero_one") && length(unique(x_data)) != 1, 0, default_jitter)
    y_jitter <- ifelse(is.numeric(y_data) && !inherits(y_data, "zero_one") && length(unique(y_data)) != 1, 0, default_jitter)

  } else if (jitter == "none") {
    x_jitter <- y_jitter <- 0
  } else {
  # Use geom_jitter's default jitter amount
    x_jitter <- y_jitter <- default_jitter # just to define the variables
    if (jitter %in% c("x", "all")) x_jitter <- NULL
    if (jitter %in% c("y", "all")) y_jitter <- NULL
  }




  # Color if there is a non-trivial third column
  show_color <- ncol(data) > 2 && length(unique(data[[3]])) > 1
  if (!show_color) {
    Res <- ggplot(data) +
      geom_jitter(aes(y=.data[[vars[1]]], x=.data[[vars[[2]]]]),
                  width=x_jitter, height=y_jitter, ...)
  }  else {
    Res <- ggplot(data) +
      geom_jitter(aes(y=.data[[vars[1]]], x=.data[[vars[[2]]]], color=.data[[vars[3]]]),
                  width=x_jitter, height=y_jitter, ...)
  }


  # Eliminate x-axis label and ticks if trivial x-variable
  if (length(unique(x_data)) == 1 && x_data[1] == 1)
    Res <- Res + scale_x_discrete() + xlab("")

  # Add a violin if called for
  if (annot %in% c("violin")) {
    if (!is.null(bw)) {
      Res <- Res + geom_violin(fill="blue", color=NA, alpha=model_alpha,
                               aes(y=.data[[vars[1]]], x=.data[[vars[2]]]), bw = bw)
    } else { # can't always pass bw=NULL to geom_violin.
      Res <- Res + geom_violin(fill="blue", color=NA, alpha=model_alpha,
                               aes(y=.data[[vars[1]]], x=.data[[vars[2]]]))
    }
      warning("x-axis variable is numerical, so only one violin drawn for all rows.
              Perhaps you want to use ntiles() or factor() on that variable?")
  }


  if (annot == "model") {
    # calls_to_names() rejiggers the model formula so that it contains
    # references to the <names> of the already transformed data.
    mod_vals <- simple_mod_eval(calls_to_names(tilde), data, level=level)

    if (y_is_discrete && length(levels(data[[1]])) > 2) {
      warning("No modeling available for categorical vars with 3 or more levels.")
    }
    if (show_color) {
      if (x_is_discrete) {
        Res <- Res +
          geom_linerange(data=mod_vals,
                        aes(x=.data[[vars[2]]],
                            ymin=.lwr, ymax=.upr,
                            color=.data[[vars[3]]]),
                        alpha = model_alpha, size=4,
                        position="dodge") +
          guides(fill="none")
      } else {
        Res <- Res +
          geom_ribbon(data=mod_vals,
                      aes(x=.data[[vars[2]]],
                          ymin=.lwr, ymax=.upr,
                          # color = .data[[vars[3]]],
                          fill=.data[[vars[3]]]),
                      alpha = model_alpha) +
          guides(fill="none")
      }
    } else {
      if (x_is_discrete) {
        Res <- Res +
          geom_linerange(data=mod_vals,
                        aes(x=.data[[vars[2]]],
                            ymin=.lwr, ymax=.upr),
                        size = 4,
                        color="blue", alpha=model_alpha)
      } else {
        Res <- Res +
          geom_ribbon(data=mod_vals,
                      aes(x=.data[[vars[2]]],
                          ymin=.lwr, ymax=.upr),
                      color=NA,
                      fill="blue", alpha=model_alpha)
      }
    }
  }

  if (show_color) {
    Res <- Res +
      scale_color_viridis_d(option=palette, begin=0, end=0.75)

    # A BUG workaround?
    if (length(unique(data[[3]])) > 2) {
      Res <- Res +
        scale_fill_viridis_d(option=palette, begin=0.0, end=0.75)
    } else {
      Res <- Res +
        scale_fill_viridis_d(option=palette, begin=0.75, end=0.0)
    }
    # The reversal of <begin> and <end> in the fill_viridis_d() command
    # compensates for geom_jitter() and geom_ribbon() making different choices
    # for color and fill respectively. I know that sounds crazy!
  }

  if (ncol(data) > 3) { # facet by the fourth variable
    Res <- Res + facet_grid(as.formula(glue::glue(". ~ `{vars[4]}`")),
                     labeller = label_both)
  }

  # turn off horizontal axis if there are no explanatory variables
  if (vars[[2]] == 1 && all(data[[2]] == 1)){
    Res + theme_update(axis.ticks.x = element_blank(),
                       axis.text.x = element_blank())

  }

  if (inherits(data[[1]], "zero_one")) {
    Res <- label_zero_one(Res)
  }

  Res
}


# Train and evaluate the model, with evaluation only
# on a skeleton.
simple_mod_eval <- function(tilde, data, family=NULL, level=0.95) {
  M <- if (!is.null(family)) model_train(data, tilde, family=family)
  else model_train(data, tilde)
  Dskel <- expand.grid(data_skeleton(data, tilde))
  Meval <- model_eval(M, data=Dskel, interval="confidence", level=level) |> select(.output, .lwr, .upr)
  # Rename the skeleton values to correspond to the names in <data>
  D <- data_from_tilde(Dskel, tilde[[3]])

  bind_cols(D, Meval) # renamed skeleton and model outputs.
}

# Replace the innards of a tilde expression with the names of variables
# from data_from_tilde
calls_to_names <- function(tilde) {
  if (length(tilde) == 1) return(tilde)
  if (deparse(tilde[[1]]) %in% c("~", "+", "-", "*")) {
    for (k in 2:length(tilde)) {
      tilde[[k]] <- calls_to_names(tilde[[k]])
    }
  }
  else {
    str <- deparse(tilde)
    if (grepl("(ns|poly|rand)\\(", str)) {
      # handle ns() or poly() or rand(), by passing the
      # whole expression. This is because simple_mod_eval() doesn't
      # translate the multi-column modeling transformations so
      # it has to be done by the modeling software.
      return(tilde)
    }
    return(as.name(deparse(tilde)))
  }
  tilde
}

# Left over from an earlier choice of colors.
# discrete_palette <- function(n) {
#   fixed <- list(
#     c("#1f78b4"),
#     c('#a6cee3','#1f78b4'),
#     c('#a6cee3','#1f78b4','#b2df8a'),
#     c('#a6cee3','#1f78b4','#b2df8a','#33a02c'),
#     c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99'),
#     c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c'),
#     c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f'),
#     c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00'),
#     c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6'),
#     c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')
#
#     )
#   if (n <= length(fixed)) fixed[[n]]
#   else if (n <= 12) brewer.pal(n, name="Paired")
#   else if (n <= 32) c(brewer.pal(12, name="Paired"),
#                       brewer.pal(12, name="Set3"),
#                       brewer.pal(8, name="Accent)"))[1:n]
#   else stop("Can't provide more than 32 discrete colors.")
# }
