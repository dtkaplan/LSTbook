#' Graph a model function
#'
#' Every model has an implicit function whose output is the response variable and which has one
#' or more explanatory variables. (Exceptionally, there might be no explanatory variables as in
#' `response ~ 1`.) One of the explanatory variables can be mapped to the horizontal axis; this can
#' be either quantitative or categorical. The remaining explanatory variables will be mapped to color,
#' facet-horizontal, and facet-vertical. For visual clarity, any quantitative
#' variables among these remaining variables must be coerced
#' to categorical, corresponding to a discrete set of colors and a discrete set of facets.
#'
#' @param mod A model object, made with `model_train()`, `lm()`, or `glm()`
#' @param interval The type of interval to draw (default: confidence)
#' @param level The confidence or prediction level for the interval
#' @param nlevels Integer. When quantitative variables need to be converted to
#' factors for color or facetting, how many levels in those factors.
#' @param palette One of "A" through "F" giving some control for people who don't like or can't see the default palette
#' @param model_ink The density of ink used to draw the model. ("alpha" for those in the know.)
#'
#' @export
model_plot <- function(mod, nlevels = 3,
                       interval = c("confidence", "prediction", "none"),
                       level=0.95,
                       palette=LETTERS[1:8],
                       model_ink=0.7) {
  interval = match.arg(interval)
  palette  = match.arg(palette)
  evars <- explanatory_vars(mod)
  response_name <- as.character(deparse(response_var(mod)))

  Skeleton <- coerce_model_for_graph(mod, nlevels = nlevels,
                                      interval = interval,
                                      level = level)
  # Get rid of response and residual if they are in the Skeleton
  if (".response" %in% names(Skeleton))
    Skeleton <- Skeleton |> dplyr::select(-.response, -.resid)
  # Add a placeholder explanatory variable if model has none.
  if (names(Skeleton)[1] == ".output")
    Skeleton <- cbind(tibble(All = "all"), Skeleton)

  # Determine aesthetics
  aes_mappings <- list(x = as.name(names(Skeleton)[1]))
  aes_settings <- list(alpha = model_ink)
  if (interval != "none") {
    # Draw intervals as bands
    aes_mappings$ymin = as.name(".lwr")
    aes_mappings$ymax = as.name(".upr")
    if (names(Skeleton)[2] %in% evars) {
      # There's a color variable. Use fill aesthetic for bands
      aes_mappings$fill <- as.name(names(Skeleton)[2])
      aes_mappings$color <- as.name(names(Skeleton)[2])
    } else {
      aes_settings$fill <- "blue"
      aes_settings$color <- "blue"
    }
  } else {
    # Draw model as a line
    aes_mappings$ymin = as.name(".output")
    aes_mappings$ymax = as.name(".output")
    if (names(Skeleton)[2] %in% evars) {
      # There's a color variable
      aes_mappings$color <- aes_mappings$fill <- as.name(names(Skeleton)[2])
    } else {
      aes_settings$color <- aes_settings$fill <- "blue"
    }
  }

  # Determine the faceting variables (if any)
  facet1 <- ifelse(names(Skeleton)[3] %in% evars, names(Skeleton)[3], NA_character_)
  facet2 <- ifelse(names(Skeleton)[4] %in% evars, names(Skeleton)[4], NA_character_)

  # Two major situations:
  # 1. First explanatory variable numeric, ribbons
  # 2. First explanatory variable not numeric, use error bars
  the_geom <- ifelse(is.numeric(Skeleton[[1]]), ggplot2::geom_ribbon, ggplot2::geom_linerange)
  # Fatten up lines if x-axis is categorical
  aes_settings$linewidth <- ifelse(is.numeric(Skeleton[[1]]), 0, 3)
  P <- ggplot2::ggplot(Skeleton, do.call(aes, aes_mappings))

  P <- P + do.call(the_geom, aes_settings)

  # Facet the plot, if appropriate
  if (!is.na(facet1)) {
    if (is.na(facet2)) {
      facet_formula <- stats::as.formula(paste("~", facet1))
      P <- P + facet_wrap(facet_formula, labeller = "label_both")
    } else  {
      facet_formula <- stats::as.formula(paste(facet1, "~", facet2))
      P <- P + facet_grid(facet_formula, labeller = "label_both")
    }
  }

  P <- P + scale_color_viridis_d(option=palette, begin=0, end=0.75) +
    scale_fill_viridis_d(option=palette, begin=0, end=0.75)

  if (length(evars[1]) == 0) {
    P <- P + theme_update(axis.ticks.x = element_blank(),
                       axis.text.x = element_blank())
  }

  P + ylab(paste("Model output:", response_name ))


}

# helper function
coerce_model_for_graph <- function(mod, nlevels = 5,
                                   interval=c("none", "prediction", "confidence"),
                                   level=0.95) {
  interval=match.arg(interval)
  evars <- explanatory_vars(mod)


  # Produce an explanatory skeleton with only a handful of levels for quantitative variables.
  # (model_skeleton() treats the first explanatory variable differently, with potentially
  # many levels)


  Skeleton <- if (length(evars) == 0 ) {
    tibble(All = "all") # When there are no explanatory variables, return a dummy skeleton
  } else {
    model_skeleton(mod, ncont = nlevels)
  }
  Skeleton <- model_eval(mod, Skeleton, interval = interval, level = level)

  # Convert quantitative explanatory variables to categorical
  # (except for the first explanatory variable)

  for (var in evars[-1]) {
    if (is.numeric(Skeleton[[var]])) {
      the_levels <- sort(unique(Skeleton[[var]]))
      Skeleton[[var]] <- factor(Skeleton[[var]], levels = the_levels, ordered = TRUE)
    }

  }
  Skeleton
}
