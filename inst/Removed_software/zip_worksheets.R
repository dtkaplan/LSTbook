#' Zip the worksheet files
#'
#' This utility enables multiple worksheets on Posit.cloud to be downloaded
#' all at once in zip format
#'
#' @export
zip_worksheets <- function() {
  file_names <- dir(pattern="^Worksheet-.*Rmd$")
  student_name <- readLines(file_names[1], n=5L)
  student_name <- student_name[grepl("^author:", student_name)] |>
    gsub("author: *", "", x=_) |> gsub("[\" ]", "", x=_) |>
    gsub("@.*$", "", x=_) |> gsub("\\.", "_", x=_)
  dir.create(student_name)
  file.copy(file_names, student_name)
  zip_file_name <- glue::glue("{student_name}.zip", zip="zip")
  zip(zip_file_name, paste0(student_name, "/", file_names))
  unlink(student_name, recursive=TRUE)
}
