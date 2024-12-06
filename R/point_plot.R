#' One-step data graphics
#'
#' `point_plot()` makes it easy to construct an informative basic graph of a
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
#' @seealso `add_plot_labels` to add labels to the plot (without needing the ggplot2 + pipe)
#'
#' @param D a data frame
#' @param tilde tilde expression specifying `y ~ x` or `y ~ x + color`
#' @param annot Statistical annotation (one of "none", "violin", "model", "bw")
#' @param point_ink Opacity of ink for the data points
#' @param model_ink Opacity of ink for the model annotation
#' @param palette Depending on taste and visual capabilities, some people might
#' prefer to alter the color scheme. There are 8 palettes available: `"A"` through `"H"`.
#' @param seed (optional) random seed for jittering
#' @param jitter Options for turning on jitter: one of `"default"`, `"both"`, `"none"`, `"x"`, `"y"`. By default,
#' By default, categorical variables are jittered.
#' @param bw bandwidth for violin plot
#' @param level confidence level to use (0.95)
#' @param interval the type of interval: default `"confidence"`. Others: `"none"` or `"prediction"`
#' @param nx Number of places to evaluate any x-axis quantitative vars. Default 50. Use higher
#' if graph isn't smooth enough.
#' @param ncategorical Number of levels of the categorical variable to retain
#' when displaying model annotations. Default: Choose the 3 most populated levels.
#' @param model_family Override the default model type. See `model_train()`
#' @param \ldots Graphical options for the data points, labels, e.g. size
#'
#' @returns A ggplot graphics object
#'
#' @examples
#' mosaicData::Galton |> point_plot(height ~ mother + sex + father, annot="model", model_ink=1)
#' mtcars |> point_plot(mpg ~ wt + cyl)
#' mtcars |> point_plot(mpg ~ wt + cyl + hp, annot="model")
#' @export
point_plot <- function(D, tilde, ..., seed=101,
                       annot = c("none", "violin", "model", "bw"),
                       jitter = c("default", "none", "all", "x", "y"),
                       interval = c("confidence", "none", "prediction"),
                       point_ink = 0.5,
                       model_ink = 0.4, palette=LETTERS[1:8], bw = NULL, level=0.95,
                       nx = 50, ncategorical = 3, model_family = NULL) {
  annot <- match.arg(annot)
  interval <- match.arg(interval)
  palette <- match.arg(palette)
  jitter <- match.arg(jitter)
  set.seed(seed)
  # tilde_vars <- all.vars(tilde, unique = FALSE) # get list of variables (with repeats, if any)


  if (inherits(D, "ggplot")) {
    # Grab the data from the incoming plot
    data <- D$data
  } else if (inherits(D, "data.frame")) {
    data <- D
    D <- NULL # this will be the first layer of the plot
  } else if (inherits(D, "glm")) {
    data <- get_training_data(D) # Grab the data from the model
    model <- D
    D <- NULL # this will be the first layer of the plot
  }

  data <- data_from_tilde(data, tilde)

  vars <- names(data)
  if (length(vars) > 4)
    stop("data_graphics() can handle at most four variables, you have",
         length(vars), ".")
  # Handle case where response variable is the same as one of the explanatory vars.
  if (vars[1] %in% vars[-1]) {
    vars[1] <- paste0(vars[1], ".")
    names(data)[1] <- vars[1]
  }

  y_data <- data[[1]]
  x_data <- data[[2]]
  x_is_discrete <- continuous_or_discrete(x_data) == "discrete"
  y_is_discrete <- continuous_or_discrete(y_data) == "discrete"


  # Convert color and faceting (columns 3 and 4) to discrete values

  if (ncol(data) > 2) {
    columns <- 3:ncol(data)
    # See if there are any NA in the color and faceting variables
    na_present <- rep(FALSE, nrow(data))
    for (k in columns) {
      # Update na_present for each column
      na_present <- na_present | is.na(data[[k]])
      if  (continuous_or_discrete(data[[k]]) == "continuous") {
        if (length(unique(data[[k]])) < 5) {
          data[[k]] <- factor(data[[k]], ordered = TRUE)
        } else {
          data[[k]] <- ntiles(signif(data[[k]],2), 3, format="interval")
        }
      } else {
        # make sure that color and fill aesthetics are aligned
        data[[k]] <- factor(data[[k]], ordered = TRUE)
      }
    }
    # Eliminate the rows with the NAs in color or faceting
    data <- data |> filter(!na_present)
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
    Res <- ggplot2::ggplot(data) +
      ggplot2::geom_jitter(aes(y=.data[[vars[1]]], x=.data[[vars[[2]]]]),
                  width=x_jitter, height=y_jitter, alpha = point_ink, ...)
  }  else {
    Res <- ggplot2::ggplot(data) +
      ggplot2::geom_jitter(aes(y=.data[[vars[1]]], x=.data[[vars[[2]]]], color=.data[[vars[3]]]),
                  width=x_jitter, height=y_jitter, alpha = point_ink, ...)
  }


  # Eliminate x-axis label and ticks if trivial x-variable
  if (length(unique(x_data)) == 1 && x_data[1] == 1)
    Res <- Res +  scale_x_discrete() + xlab("")

  # Add a violin if called for
  if (annot %in% c("violin")) {
    if (!is.null(bw)) {
      Res <- Res + ggplot2::geom_violin(fill="blue", color=NA, alpha=model_ink,
                               aes(y=.data[[vars[1]]], x=.data[[vars[2]]]), bw = bw)
    } else { # can't always pass bw=NULL to geom_violin.
      Res <- Res + ggplot2::geom_violin(fill="blue", color=NA, alpha=model_ink,
                               aes(y=.data[[vars[1]]], x=.data[[vars[2]]]))
    }
    if (!x_is_discrete) {
    warning("x-axis variable is numerical, so only one violin drawn for all rows.
              Perhaps you want to use ntiles() or factor() on that variable?")
    }
  }

  # Add a box-and-whisker plot if called for
  if (annot %in% c("bw")) {
    if (!is.null(bw)) {
      Res <- Res + ggplot2::geom_boxplot(fill=NA, color="blue", alpha=model_ink,
                               aes(y=.data[[vars[1]]], x=.data[[vars[2]]]), bw = bw)
    } else { # can't always pass bw=NULL to geom_violin.
      Res <- Res + ggplot2::geom_boxplot(fill=NA, color="blue", alpha=model_ink,
                               aes(y=.data[[vars[1]]], x=.data[[vars[2]]]))
    }
    if (!x_is_discrete) {
      warning("x-axis variable is numerical, so only one box-and-whiskers is drawn for all rows.
              Perhaps you want to use ntiles() or factor() on that variable?")
    }
  }


  if (annot == "model") {
    # calls_to_names() rejiggers the model formula so that it contains
    # references to the <names> of the already transformed data.
    mod_vals <- simple_mod_eval(calls_to_names(tilde), data,
                                level = level, itype = interval,
                                nx = nx, ncategorical, family = model_family)

    # special case for categorical response variable
    if (!inherits(y_data, "zero_one") && y_is_discrete) {
      if (length(unique(data[[1]])) == 2) {
        # Need to add 1 to model output levels, which are in [0,1],
        # so that they will graph on the same
        # axis as the categorical response values which are numerically 1 and 2.
        mod_vals$.output <- mod_vals$.output + 1.
        mod_vals$.lwr <- mod_vals$.lwr + 1.
        mod_vals$.upr <- mod_vals$.upr + 1.
      }
      if (length(levels(data[[1]])) > 2) {
          warning("No modeling available for categorical vars with 3 or more levels.")
      }
    }
    if (show_color) {
      if (x_is_discrete) {
        if (model_ink < 0.8) model_ink <- model_ink + 0.2 # enhance visibility
        annot_width <- 6
        nlevels <- length(unique(mod_vals[[vars[2]]]))
        annot_width <- annot_width / ifelse(nlevels > 4, sqrt(nlevels), 1)
        Res <- Res +
          geom_linerange(data=mod_vals,
                        aes(x=.data[[vars[2]]],
                            ymin=.data$.lwr, ymax=.data$.upr, # for CRAN CMD check
                            color=.data[[vars[3]]]),
                        alpha = model_ink, linewidth = annot_width,
                        position=position_dodge(width = 0.5)) +
          guides(fill="none")
      } else {
        Res <- Res +
          geom_ribbon(data=mod_vals,
                      aes(x=.data[[vars[2]]],
                          ymin=.data$.lwr, ymax=.data$.upr,
                          # color = .data[[vars[3]]],
                          fill=.data[[vars[3]]]),
                      alpha = model_ink) +
          guides(fill="none")
      }
    } else {
      if (x_is_discrete) {
        Res <- Res +
          geom_linerange(data=mod_vals,
                        aes(x=.data[[vars[2]]],
                            ymin=.data$.lwr, ymax=.data$.upr),
                        size = 4,
                        color="blue", alpha=model_ink)
      } else {
        Res <- Res +
          geom_ribbon(data=mod_vals,
                      aes(x=.data[[vars[2]]],
                          ymin=.data$.lwr, ymax=.data$.upr),
                      color=NA,
                      fill="blue", alpha=model_ink)
      }
    }
  }

  if (show_color) {
    Res <- Res +
      scale_color_viridis_d(option=palette, begin=0, end=0.75) +
      scale_fill_viridis_d(option=palette, begin=0.0, end=0.75)

  }

  if (ncol(data) > 3) { # facet by the fourth variable
    Res <- Res + facet_grid(stats::as.formula(glue::glue(". ~ `{vars[4]}`")),
                     labeller = label_both)
  }

  # turn off horizontal axis if there are no explanatory variables
  if (vars[[2]] == 1 && all(data[[2]] == 1, na.rm=TRUE)){
    Res + theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank())

  }

  if (inherits(data[[1]], "zero_one")) {
    Res <- label_zero_one(Res)
  }

  Res
}

#' Convenience function for adding labels to point_plot or others without needing
#' the ggplot2 + pipe.
#' @param P A ggplot2 object, for instance as made with `point_plot()` or `model_plot()`
#' @param \ldots Label items (e.g. `x = "hello"`) as in ggplot2::labs
#' @param color Name for color legend (works for `point_plot()`)
#'
#' @returns A ggplot graphics object
#'
#' @examples
#' mtcars |> point_plot(mpg ~ hp + cyl) |>
#'   add_plot_labels(x = "The X axis", y = "Vertical", color = "# cylinders")
#' @export
add_plot_labels <- function(P, ..., color=NULL) {
  P <- P + labs(...)
  if (is.null(color)) P
  else P + suppressMessages(scale_colour_discrete(name=color))
}

#'
#'
# Train and evaluate the model, with evaluation only
# on a skeleton.
simple_mod_eval <- function(tilde, data, family=NULL, level=0.95, itype="confidence", nx=50, ncategorical = 3) {
  M <- if (!is.null(family)) model_train(data, tilde, family = family)
  else model_train(data, tilde)
  # Construct a skeleton of explanatory variable values
  if (length(all.vars(rlang::f_rhs(tilde))) == 0) {
    # there are no explanatory variables, that is, the tilde is y ~ 1
    Dskel <- tibble::tibble(1)
  } else {
    Dskel <- expand.grid(data_skeleton(data, tilde, ncont=nx, nlevels = ncategorical))
  }
  Meval <- model_eval(M, data = Dskel, interval = itype, level = level) |>
    dplyr::select(".output", ".lwr", ".upr")
  # Rename the skeleton values to correspond to the names in <data>
  D <- data_from_tilde(Dskel, tilde[[3]])

  dplyr::bind_cols(D, Meval) # renamed skeleton and model outputs.
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

