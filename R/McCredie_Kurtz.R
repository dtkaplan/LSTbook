#' "Big Five" personality ratings for college first-year students
#'
#' Abstract from the research paper: Five-factor personality ratings were provided by undergraduate freshmen,
#' their parents, and their college peers as predictors of cumulative GPA
#' upon graduation. Conscientiousness ratings were significant
#' predictors of GPA by all three raters; peer ratings of
#' Conscientiousness were the only significant predictor of GPA when
#' self-, parent-, and peer-ratings of Conscientiousness
#' were examined simultaneously.
#' College major was a moderator of this relationship,
#' with self- and parent-ratings of Conscientiousness correlating
#' more strongly with GPA among Social Science majors and
#' parent-ratings of Conscientiousness correlating less
#' strongly with GPA among Science majors.
#' These findings replicate existing research regarding the
#' validity of informant ratings as predictors of behavioral outcomes
#' such as academic performance, while emphasizing the importance of
#' including multiple informants from various life contexts.

#' @details
#' The five personality factors are:
#' 1. extraversion: sociability
#' 2. neuroticism: sadness or emotional instability
#' 3. openness to experience
#' 4. agreeableness: kindness
#' 5. conscientiousness: thoughtfulness
#'
#' @format For simplicity, only the mother's and father's ratings for the student are given.
#' The variable name indicates whose rating and on what scale, e.g. `m_extra` is the
#' mothers rating on the extraversion scale. Other variables are:
#' - subjid: Unique ID for the student
#' - age: The student's age when the ratings were collected
#' - GPA: The student's eventual 4-year grade-point average
#' - sex: The student's sex
#' - field: What field the student ended up studying
#'
#' @usage data(McCredie_Kurtz)
#'
#' @docType data
#'
#' @references
#' Morgan N. McCredie and John E. Kurtz (2020) "Prospective prediction of academic performance in college using self- and informant-rated personality traits" *Journal of Research in Personality* **85**
#'
#' @source
#' McCredie_Kurtz_Open_Data.sav comes from <https://data.mendeley.com/datasets/rn2bpp6f37/1> and is described in <https://www.sciencedirect.com/science/article/abs/pii/S009265661930131X>
"McCredie_Kurtz"
