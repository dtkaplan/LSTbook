#' Pima Indians Diabetes Database
#'
#' "The population for this study was the Pima Indian population near
#' Phoenix, Arizona. That population has been under continuous
#' study since 1965 by the National Institute of Diabetes and
#' Digestive and Kidney Diseases because of its high incidence rate
#' of diabetes. Each community resident over 5 years of age
#' was asked to undergo a standardized examination every two years,
#' which included an oral glucose tolerance test. Diabetes was
#' diagnosed according to World Health Organization Criteria;
#' that is, if the 2 hour post-load plasma glucose was at least 200
#' mg/dl (11.1 mmol/l) at any survey examination or if the Indian
#' Health Service Hospital serving the community found a glucose
#' concentration of at least 200 mg/dl during the course of routine
#' medical care." --- quoted from the reference below. The data were published by Kaggle for a machine-learning competition whose goal was
#' to develop a prediction function for diabetes.
#'
#' @usage data(PIDD)
#'
#' @docType data
#'
#' @format 768 rows, each of which is a woman 21 years or older. There are 9 variables:
#' - `age` of the woman
#' - `pregnancies`: number of previous pregnancies
#' - `glucose`: glucose level
#' - `BP`: systolic blood pressure
#' - `skin_thickness`:
#' - `insulin`:
#' - `bmi`: Body mass index
#' - `pedigree`: "Diabetes Pedigree Function"
#' - `diabetes`: Did the patient develop diabetes during a 5-year follow-up?
#'
#' @references Smith, J.W., Everhart, J.E., Dickson, W.C., Knowler, W.C., & Johannes, R.S. (1988)
#' "Using the ADAP learning algorithm to forecast the onset of diabetes mellitus"
#' *Proceedings of the Symposium on Computer Applications and Medical Care*
#'
#' @source [Kaggle](https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database/)
"PIDD"
