# For the user's convenience, attach packages that are often used in the textbook

pkgs_to_attach <- c("ggplot2", "dplyr")
optional_packages <- c("mosaicData", "moderndive",
                       "palmerpenguins")
# if an optional package is installed, .onLoad() will attach it.
add_to_attach <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE))
    pkgs_to_attach <<- c(pkgs_to_attach, pkg)
}

foobar <- sapply(optional_packages, add_to_attach)

#################
.onLoad <- function(libname, pkgname) {

  pks <- invisible(suppressPackageStartupMessages(
    sapply(pkgs_to_attach, requireNamespace, quietly = TRUE)
  ))
  for (p in pkgs_to_attach) {
    if (! is_attached(p)) suppressPackageStartupMessages(attachNamespace(p))
  }
}
################
is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

################
# to avoid notes in CRAN checks
utils::globalVariables(c("data", "pf", "xend", "yend",
                         "label", "xticks", "ybase", "ymid",
                         "ytop", ".lwr", ".output", ".resid",
                         "na.exclude", "left", "right"))

####
