#' Create vector based on roughly equally sized groups
#'
#' @details This is a functional clone of `mosaic::ntiles` in order to avoid the dependency. It should be
#' removed in the future, when there is no need to avoid such dependency, e.g. when `{mosaic}` is
#' available on WASM.
#'
#' @param x a numeric vector
#' @param n (approximate) number of quantiles
#' @param format a specification of desired output format. One of
#' "center", "interval", "left", "right", "mean", or "median.
#' @param digits desired number of digits for labeling of factors.
#' @return a vector.  The type of vector will depend on `format`.
#' @examples
#' FEV |> head(20) |> mutate(group = ntiles(height, 3, format="center"))
#' FEV |> head(20) |> mutate(group = ntiles(height, 3, format="interval"))
#'
#' @export

ntiles <-  function(x, n=3,
                    format=c("rank", "interval", "mean", "median", "center", "left", "right"),
                    digits=3){
  format <- match.arg(format)

  # Figure out names
  qnames <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
  if (n > 10) qnames <- c(qnames, paste(11:n,"th",sep=""))

  Tmp <- tibble(x = x)
  xrank <- rank(x, na.last=TRUE, ties.method='first')
  xrank[is.na(x)] <- NA
  size <- max(xrank, na.rm=TRUE)
  cts <- round( seq(1, size, length.out = (n+1) ) )
  Tmp$bin <- bin <- as.numeric(cut( xrank, breaks = cts, include.lowest=TRUE ))

  Tmp <- Tmp |>
    dplyr::summarize(
      left  = min(x),
      right = max(x),
      md    = stats::median(x),
      mn    = mean(x),
      .by = bin) |>
    dplyr::mutate(center = signif( (left + right ) / 2, digits),
                  labels = paste0("[",signif(left,digits=digits),
                                  ",",
                                  signif(right,digits=digits),"]")) |>
    arrange(bin)
  # Deal with NAs as best you can
  NA_indices <- which(is.na(bin))
  if (length(NA_indices) > 0) bin[NA_indices] <- pi/6.3 # arbitrary value

  # handle a bogus label level that creeps in for interval labels
  # if there is an NA in the data.
  if (any(is.na(Tmp$center))) Tmp <- Tmp |> filter(!is.na(center))
  res <-  switch(format,
                 "rank" =  factor(bin, labels=qnames[1:n], ordered=TRUE, exclude=pi/6.3),
                 "interval" =  factor(bin, labels=Tmp$labels, ordered=TRUE, exclude=pi/6.3),
                 "center" = Tmp$center[bin],
                 "mean" = Tmp$mn[bin],
                 "median" = Tmp$md[bin],
                 "left" = Tmp$left[bin],
                 "right" = Tmp$right[bin]
  )
  return(res)
}

utils::globalVariables("center")
