#' Convert a model or a data frame to a skeleton
#'
#' Only explanatory variables are included in the result.
#'
#' @param mod A fitted model **or** a tilde expression describing a model structure, e.g. `outcome ~ vara+varb`.
#' @param data a data frame. Relevant only when `mod` is a tilde expression
#' @param ncont minimum number of levels at which to represent continuous variables. (More levels
#' may be added to "prettify" the choices. See [pretty()].)
#' @param nfirstcont Like `ncont`, but for the first explanatory variable if it is categorical. This variable is mapped
#' to the horizontal axis and so should have many levels to produce a smooth graph. (Default: 50)
#'
#' @export
model_skeleton <- function(mod, data=NULL, ncont=3, nfirstcont=50) {
  if (is.null(data)) data <- get_training_data(mod)

  if (inherits(mod, "formula")) {
    if (length(formula) == 2) explan_names <- all.vars(formula)
    else explan_names <- all.vars(formula[[3]])
  } else {
    explan_names <- all.vars(formula_from_mod(mod)[[3]])
  }
  Skeleton <- list()

  for (var in explan_names) {
    values <- stats::na.omit(data[[var]])
    type <- continuous_or_discrete(values)
    if (type == "continuous") {
      # For the first explanatory variable, pull out `nfirstcont` levels.
      # For others, only `ncont` levels.
      points <- if (length(unique(values)) <= ncont + 1) values
                else pretty(values,
                            n = ifelse(var == explan_names[1],
                                       nfirstcont, ncont - 1),
                                       bounds = TRUE,
                                       min.n = pmax(0, ncont - 3))
      # If no actual negative values, don't allow any in the levels
      if (min(values) >= 0) points <- points[points >= 0]
    } else {
      if (inherits(values, "zero_one")) points <- unique.zero_one(values)
      else points <- unique(values)
    }
    Skeleton[[var]] <- points
  }

  expand.grid(Skeleton)
}


data_skeleton <- function(data, tilde, spreadn=NULL, ..., ncont=10, nlevels=3) {
  # find the names of the explanatory variables in an order implied by the formula
  explan_names <- all.vars(tilde)
  # strip out the response variable, if any
  if (length(tilde) == 3) explan_names <- explan_names[-1]

  # How many variables are to have multiple values in the skeleton
  if (is.null(spreadn)) spreadn <- min(c(length(explan_names), 4))
  by_hand <- list(...)

  if (!all(explan_names %in% names(data))) stop("<tilde> has names not in the data.")

  # Placeholders
  Vals <- list()

  # is the first explanatory variable continuous or discrete?
  first_vals <- data[[explan_names[1]]]
  first_type <- continuous_or_discrete(first_vals)
  Vals[[explan_names[[1]]]] <- get_typical(data[[explan_names[1]]],
                                           type = "continuous", ncont = ncont,
                                           nlevels = nlevels)
  if (spreadn > 1) {
    for (k in 2:spreadn) {
      Vals[[explan_names[[k]]]] <-
        get_typical(data[[explan_names[k]]], type = "discrete",
                    ncont = ncont, nlevels = nlevels)

    }
  }

  if (length(explan_names) > spreadn) {
    for (k in (spreadn+1):length(explan_names)) {
      Vals[[explan_names[[k]]]] <- get_typical(data[[explan_names[k]]], type="discrete", nlevels=1, ncont=1)
    }
  }

  if (length(by_hand) > 0) {
    for (k in 1:length(by_hand)) {
      Vals[[names(by_hand)[k]]] <- by_hand[[k]]
    }
  }

  Vals # all combinations of the levels

}

#'
continuous_or_discrete <- function(vals) {
  ifelse(inherits(vals, c("character", "logical", "factor", "zero_one")),
         "discrete",
         "continuous")
}

#'
get_typical <- function(vals, type=c("continuous", "discrete"), ncont=10, nlevels=3) {
  type <- match.arg(type)
  # special cases
  if (inherits(vals, "zero_one")) return(unique.zero_one(vals))
  if (inherits(vals, "logical")) return(unique(vals))

  val_type <- continuous_or_discrete(vals)
  if (val_type=="continuous") {
    if (ncont==1) return(stats::median(vals, na.rm=TRUE))

    n <- ifelse(type=="continuous", ncont, nlevels)
    breaks <- compromise_breaks(vals, n = n)
    return(breaks)
  } else {
    # if an ordered factor, give all the levels
    if (is.ordered(vals)) return(unique(vals))
    # pull out the nlevels most populated levels

    tmp <- table(vals)
    biggest <- order(tmp, decreasing=TRUE)[1:nlevels]
    biggest <- biggest[!is.na(biggest)]
    names(tmp)[biggest]
  }
}

#'
compromise_breaks <- function(vals, n) {
  breaks = stats::quantile(vals, seq(0, 1, length=n), na.rm=TRUE)
  # evenly spread throughout the value range
  breaks2 <- seq(min(vals, na.rm = TRUE),
                 max(vals, na.rm = TRUE),
                 length=n)

  # compromise
  as.numeric((breaks + breaks2)/2)
}
