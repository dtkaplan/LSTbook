#' Zero-one transformation for categorical variable
#'
#' A convenience function for handling categorical response variables.
#' Ordinarily, ggplot2 maps categorical levels to numerical values 1, 2, ....
#' This is inappropriate for logistic modeling, where we want the levels to
#' be on a probability scale.
#'
#' @param x a categorical variable
#' @param one character string specifying the level that gets mapped to 1.
#'
#' @examples
#' mod <- mod_train(zero_one(outcome) ~ age + smoker, data = Whickham, type="linear")
#' gf_jitter(zero_one(outcome) ~ age, color=~smoker, data = Whickham) %>% label_zero_one()
#'
#' @export
zero_one <- function(x, one) {
  U <- unique(x)
  if (length(U) > 2) U <- c(U[1], "others")
  if (missing(one)) one <- U[1]
  if (! one %in% U) stop("Specified level for one not one of the levels.")

  res <- rep(0, length(x))
  res[x == one] <- 1
  class(res) <- c(class(res), "zero_one")
  attr(res, "levels") <- rev(U)
  res
}

#' not clear how to use this. I want an easy way to label the vertical axis of graphs
#' where the y-axis is a zero-one variable.
#'
#' @examples
#' P <- gf_jitter(zero_one(outcome) ~ age, color=~smoker, data = Whickham) %>% label_zero_one() %>% gf_labs(y="Smoker")
#' mod <- mod_train(zero_one(outcome) ~ age + smoker, data = Whickham)
#' fun <- makeFun(mod)
#' P
#' P %>% mosaicCalc::slice_plot(fun(age, smoker="Yes") ~ age)


#' need to write documentation for the graphics command `label_zero_one()`
#' @export
label_zero_one <- function(P) {
  YesNo <- rlang::eval_tidy(P$mapping$y, data = P$data)
  if (!inherits(YesNo, "zero_one")) return(P)
  else P + scale_y_continuous(breaks=waiver(),
                              sec.axis=sec_axis(trans = ~ .,
                                                breaks=c(0,1), labels=levels(YesNo)))
}

#' @export
unique.zero_one <- function(x, incomparables=FALSE, ...) {
  levels <- attr(x, "levels")
  vals <- unique.default(x)
  attr(vals, "levels") <- levels
  class(vals) <- c(class(vals), "zero_one")

  vals
}
