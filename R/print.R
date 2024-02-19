#' Nice printing of some internal objects

#' @param x A data simulation as made by `datasim_make()`
#' @param \ldots for compatibility with generic `print()`
#' @param report_hidden Show the hidden nodes (nodes whose name begins with `.`)
print.datasim <- function(x, ..., report_hidden = FALSE) {
  vnames <- lapply(x$names, as.character) |> unlist()
  vcalls <- x$calls
  # Hide the hidden ones
  if (!report_hidden) {
    rid <- grepl("^\\.", vnames)
    vnames <- vnames[!rid]
    vcalls <- vcalls[!rid]
  }

  RHS <- lapply(vcalls, deparse, width.cutoff = 500) |> unlist()
  components <- glue::glue("[{seq_along(vnames)}] {vnames} <- {RHS}")

  cat(paste0(
    "Simulation object\n------------\n",
    paste(components, collapse="\n")))
  invisible(x)
}
