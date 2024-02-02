#' Zero-one transformation for categorical variable
#'
#' A convenience function for handling categorical response variables.
#' Ordinarily, ggplot2 maps categorical levels to numerical values 1, 2, ....
#' Such numerical mapping is inappropriate for logistic modeling, where we want the levels to
#' be on a probability scale.
#'
#' @param x a categorical variable
#' @param one character string specifying the level that gets mapped to 1.
#'
#' @examples
#' mosaicData::Whickham |> point_plot(zero_one(outcome, one="Alive") ~ age + smoker, annot = "model")
#'
#' @export
zero_one <- function(x, one) {
  U <- unique(x)
  if (missing(one)) one <- U[1]
  if (length(U) > 2) U <- c(one, "other")

  if (!one %in% U) {
    stop("Specified level for <one> not one of the levels of the variable.")
  } else {
    U <- c(as.character(one), as.character(U[U!=one])) # as.character() to avoid factors becoming numerical levels
  }
  res <- rep(0, length(x))
  res[x == one] <- 1
  class(res) <- c(class(res), "zero_one")
  attr(res, "levels") <- rev(U)
  res
}


#' @rdname zero_one
#' @param P A ggplot2 object made by `model_plot()` or `point_plot()`
#' @examples
#' mosaicData::Whickham |>
#'   point_plot(zero_one(outcome, one = "Alive") ~ age + smoker, annot = "model") |>
#'   label_zero_one() +
#'   ylab("Prob. alive at 20-year follow-up")
#' @export
label_zero_one <- function(P) {
  # vertical axis data in P$data[[1]]
  YesNo <- P$data[[1]]
  if (!inherits(YesNo, "zero_one")) return(P)
  else {
    P + scale_y_continuous(breaks=c(0, 0.5, 1),
                              sec.axis=sec_axis(trans = ~ .,
                                                breaks=c(0,1), labels=levels(YesNo)))
  }
}

#' @export
unique.zero_one <- function(x, incomparables=FALSE, ...) {
  levels <- attr(x, "levels")
  vals <- unique.default(x)
  attr(vals, "levels") <- levels
  class(vals) <- c(class(vals), "zero_one")

  vals
}
