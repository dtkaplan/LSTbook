#' Add a slope "rose" to a plot.
#'
#' To guide a reader in quantifying the slope of components of an x-y graph, a "slope rose" is helpful.
#' Several radiating lines are drawn, each marked with a numerical slope. A suitable choice of slopes
#' is made automatically, based on the x- and y- scale of the plot.
#'
#' @note Use the pipe operator to send a previously made plot to have a rose added. Don't use the `{ggplot2}`
#' `+` connector.
#'
#'
#' @param P a ggplot2 object made by the ggformula package
#' @param x the x-position of the rose. This will be assigned automatically if `x` isn't specified.
#' @param y the y-position of the rose, just like `x`.
#' @param scale the size of the rose as a fraction of the plot area covered (default 1/4)
#' @param color text string (e.g. `"blue"`) for the rose
#' @param keepers whether to show `"both"` positive and negative slopes or just show the `"pos"` or the `"neg"`
#'
#' @examples
#' gf_point(mpg ~ hp, data=mtcars) |> add_slope_rose()
#' gf_point(wt ~ hp, data=mtcars) |> add_slope_rose(keepers="pos", color="blue", x=100, scale=.5 )
#'
#'
#' @export
add_slope_rose <-
function(P, x=NULL, y=NULL, scale=1/4, color="red", keepers=c("both", "pos", "neg")) {
  keepers <- match.arg(keepers)
  xy <- layer_scales(P)
  xrange <- xy$x$range$range
  yrange <- xy$y$range$range
  if (is.null(x)) x <- mean(xrange)
  if (is.null(y)) y <- mean(yrange)
  width <- diff(xrange)*scale
  height <- diff(yrange)*scale
  center <- height / width
  nice_slopes <- pretty(extendrange(c(-center, center),f=.1 ), n=6) %>% setdiff(0)
  if (keepers == "pos") nice_slopes <- nice_slopes[nice_slopes > 0]
  if (keepers == "neg") nice_slopes <- nice_slopes[nice_slopes < 0]

  nice_x <- rep(width, length(nice_slopes))
  nice_y <- nice_slopes*nice_x

  Lines <- tibble(xend=nice_x+x, yend=nice_y + y, x=x, y=y, label=as.character(nice_slopes))

  P + geom_segment(data=Lines, aes(x=x, y=y, xend=xend, yend=yend), color=color) + geom_label(data=Lines, aes(x=xend, y=yend, label=label), hjust=0, color=color)


}
