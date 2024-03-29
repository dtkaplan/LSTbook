#' Grades at a small college
#'
#' These are the actual grades for 400+ individual students in the courses they took
#' at a small, liberal-arts college in the midwest US. All the students graduated in 2006.
#' Each row corresponds to a single student in a single course. The data have been de-identified by translating the student ID, the instructor
#' ID, and the name of the department. Typically a graduating student has taken about 32 courses.
#' As another form of de-identification, only half of the courses each student, selected randomly,
#' are included. Only courses with 10 or more students enrolled were included.
#'
#' @docType data
#' @name College_grades
#' @usage data(College_grades)
#'
#' @keywords datasets
#'
#' @source The data were helpfully provided by the registrar of the college with the proviso
#' that the de-identification steps outlined above be performed.
#'
#' @format A data frame with 6146 Grades for 443 students.
#'
#'     -grade The letter grade for the student in this course: A is the highest.
#'     - sessionID An identifier for the course taken. Courses
#'     offered multiple times in one semester or across semesters have individual IDs.
#'     -sid The student ID
#'     -dept The department in which the course was offered. 100 is entry-level,
#'     200 sophomore-level, 300 junior-level, 400 senior-level.
#'     -enroll Student enrollment in the course. This includes students who are not
#'     part of this sample.
#'     -iid Instructor ID
#'     -gradepoint A translation of the letter grade into a numerical scale. 4 is high.
#'     Some letter grades are not counted in a student's gradepoint average. These have \code{NA} for
#'     the gradepoint.
#'
"College_grades"
