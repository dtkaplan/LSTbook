#' Sample from a college registrar's database
#'
#' Grade data from students at a liberal arts college. IDs of students,
#' professors, and departments have been dis-identified.
#'
#'
#'
#' @docType data
#'
#' @format Three data frames
#' - Sessions: ID for a class session, that is, a course in a semester
#'     - sessionID: Unique identifier for the session
#'     - iid: Unique identifier for the instructor
#'     - enroll: Total enrollment in the session (note: includes students who didn't make it into the sample in `Grades`)
#'     - dept: Unique identifier for the department
#'     - level: Instruction evel of the course 100, 200, 300, 400. Roughly: first-year, sophomore, junior, senior
#'     - sem: The semester in which the session was held.
#' - Grades: A 50% random sample of student-by-student grades in those Sessions
#'     - sid: Unique identifier or the student.
#'     - grade: Letter grade: A, A-, B+ and so on,
#'     - sessionID: The course session for the grade, as in the Sessions data frame
#' - Gradepoint: Letter to numerical conversion (per college policy)
#'     - grade: Letter grade: A, A-, and so on
#'     - gradepoint: Numerical equivalent
#'
#' @source
#' Used with permission by the college's registrar.
#'
#' @name Registrar
"Sessions"

#' @rdname Registrar
"Grades"

#' @rdname Registrar
"Gradepoint"
