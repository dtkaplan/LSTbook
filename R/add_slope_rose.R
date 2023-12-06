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
#' @param P a ggplot2 object made by the ggplot2 or ggformula packages
#' @param x the x-position of the rose. This will be assigned automatically if `x` isn't specified.
#' @param y the y-position of the rose, just like `x`.
#' @param width for rulers, the distance between tick marks (in native units, where
#' categories are separated by a distance of 1.)
#' @param ticks Integers, typically `0:5`, that label the ticks.
#' @param scale the size of the rose as a fraction of the plot area covered (default 1/4)
#' @param color text string (e.g. `"blue"`) for the rose
#' @param keepers whether to show `"both"` positive and negative slopes or just show the `"pos"` or the `"neg"`
#'
#' @details For the ruler, x gives the position of the root of the ruler, with
#' the rest of the ruler moving off to the left. (For vertically oriented rulers, use
#' a negative width.)
#'
#' @examples
#' mtcars |> pointplot(mpg ~ hp, annot="model") |> add_slope_rose()
#' mtcars |> pointplot(wt ~ hp) |> add_slope_rose(keepers="pos", color="blue", x=100, scale=.5 )
#'
#' @rdname statistical_annotations
#' @export
add_slope_rose <- function(P, x=NULL, y=NULL, scale=1/4,
                           color="red",
                           keepers=c("both", "pos", "neg")) {
  keepers <- match.arg(keepers)
  xy <- layer_scales(P)
  xrange <- xy$x$range$range
  yrange <- xy$y$range$range
  if (is.null(x)) x <- mean(xrange)
  if (is.null(y)) y <- mean(yrange)
  width <- diff(xrange)*scale
  height <- diff(yrange)*scale
  center <- height / width
  # define this function to avoid dependency on grDevices
  extendrange <- function (x, r = range(x, na.rm = TRUE), f = 0.05)
  {
    if (!missing(r) && length(r) != 2)
      stop("'r' must be a \"range\", hence of length 2")
    f <- if (length(f) == 1L)
      c(-f, f)
    else c(-f[1L], f[2L])
    r + f * diff(r)
  }
  nice_slopes <- pretty(extendrange(c(-center, center),f=.1 ), n=6) %>% setdiff(0)
  if (keepers == "pos") nice_slopes <- nice_slopes[nice_slopes > 0]
  if (keepers == "neg") nice_slopes <- nice_slopes[nice_slopes < 0]

  nice_x <- rep(width, length(nice_slopes))
  nice_y <- nice_slopes*nice_x

  Lines <- tibble(xend=nice_x+x, yend=nice_y + y, x=x, y=y, label=as.character(nice_slopes))

  P + geom_segment(data=Lines, aes(x=x, y=y, xend=xend, yend=yend), color=color) + geom_label(data=Lines, aes(x=xend, y=yend, label=label), hjust=0, color=color)


}


#' Add a ruler (typically on top of a violin plot)
#' @rdname statistical_annotations
#' @param \ldots additional graphical parameters, e.g. `color = "blue"`
#' @export
add_violin_ruler <-
  function(P, x=NULL, y=NULL,
           width=1/10, ticks=seq(0,1,by=0.1),
           ...) {
    xy <- layer_scales(P)
    xrange <- xy$x$range$range
    if (!is.numeric(xrange)) {
      # deal with a categorical variable on x axis
      xrange <- c(0, length(xrange)) + 0.5
    }
    yrange <- xy$y$range$range
    if (is.null(x)) x <- mean(xrange)
    if (is.null(y)) y <- mean(yrange)
    height <- diff(yrange)

    Ruler <- tibble(
      xticks = x - width*(seq_along(ticks)-1),
      ybase = y,
      ymid = y + height/20,
      ytop = y + height/15,
      labels = as.character(sort(ticks))
    )

    P + geom_path(aes(x=xticks, y=ybase), data=Ruler, ...) +
      geom_segment(aes(x=xticks, xend=xticks, y=ybase, yend=ymid), data=Ruler, ...) +
      geom_text(aes(x=xticks, y=ytop, label=labels), size=3,
                vjust=0, data=Ruler, ...)

  }

