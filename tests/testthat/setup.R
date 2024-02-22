# Helper function for snapshots
gg_in_tmp_png <- function(plot_code, width = 4, height = 4) {
  path <- tempfile(fileext = ".png")
  ggsave(path, plot=plot_code, width = width, height = height)

  path
}
