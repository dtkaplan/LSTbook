#' Pull in a worksheet template from the Math 300Z repository.
#'
#' @param lesson integer: lesson number
#' @param force if `TRUE`, override the check for existence of the file.
#'
#' @export
get_lesson_worksheet <- function(lesson=19, force=FALSE) {
  if (length(lesson) != 1 || lesson != round(lesson) || lesson < 19 || lesson > 38)
    stop("lesson must be an integer 19 through 38.")
  file_name <- glue::glue("Worksheet-{lesson}.Rmd")

  if (!force && file_name %in% dir())
    stop(glue::glue("Worksheet for Lesson {lesson} already exists. To over-write it, use the `force=TRUE` argument."))

  URL <- paste0("https://raw.githubusercontent.com/dtkaplan/Math-300Z/main/Worksheets/", file_name)
  # for debugging:
  # URL <- paste0("/Users/kaplan/Math-300Z-project/Math-300Z/Worksheets/", file_name)

  content <- readLines(URL)
  # Fix the author line
  userID <- rstudioapi::userIdentity()
  userID <- gsub("@.*$", "", userID)
  content <- sub("Jane Doe", rstudioapi::userIdentity(), content)
  # Replace the content-ful answer blocks with a mute "ANSWER: "
  content <- fix_answers(content)

  writeLines(content, con=file_name)

  message("Created ", file_name, ". You can open it in the editor.")


}
#' @rdname get_lesson_worksheet
#' @export
get_teaching_notes <- function(lesson=19, force=FALSE) {
  if (length(lesson) != 1 || lesson != round(lesson) || lesson < 19 || lesson > 38)
    stop("lesson must be an integer 19 through 38.")
  file_name <- glue::glue("Teaching-notes-{lesson}.Rmd")

  if (!force && file_name %in% dir())
    stop(glue::glue("Teaching notes file for Lesson {lesson} already exists. To over-write it, use the `force=TRUE` argument."))

  URL <- glue::glue("https://raw.githubusercontent.com/dtkaplan/Math-300Z/main/Day-by-day/Lesson-{lesson}/Teaching-notes-{lesson}.qmd")

  content <- readLines(URL)

  writeLines(content, con=file_name)

  message("Created ", file_name, ". You can open it in the editor.")

}

#' Replace the answer blocks with a simple "ANSWER:"
fix_answers <- function(content, spacer="@") {
  one_line <- paste0(content, collapse=spacer)
  search_pattern <- "\\:\\:\\: \\{.callout-note.*?\\:\\:\\:" # non-greedy match
  new_contents <- gsub(search_pattern, "ANSWER: ", one_line)

  strsplit(new_contents, "@", fixed=TRUE) |> unlist()
}

