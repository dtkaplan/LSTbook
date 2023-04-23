#' Data from the Framingham heart study
#'
#' When it launched in 1948 the original goal of The Framingham Heart Study (FHS) launched in 1948
#' with the goal of identifying risk factors for cardiovascular disease. FHS had over 14,000 people from three generations,
#' including the original participants, their children, and their grandchildren. These data represent
#' 4238 Framingham subjects and were published by Kaggle for a machine-learning competition. The goal of
#' the competition was to predict `TenYearCHD` from the other factors.
#'
#' @usage data(Framingham)
#'
#' @docType data
#'
#' @format 4238 rows, each of which is a FHS subject. There are 16 variables:
#' - `male` 1 if male, 0 if female
#' - `age` of the patient
#' - `education` highest level achieved: some HS, HS grad/GED, some college/vocational school, college graduate
#' - `currentSmoker`: whether or not the patient is a current smoker
#' - `cigsPerDay`: the number of cigarettes that the person smoked on average in one day.
#' - `BPMeds`: whether or not the patient was on blood pressure medication
#' - `prevalentStroke`: whether or not the patient had previously had a stroke
#' - `prevalentHyp`: whether or not the patient was hypertensive
#' - `diabetes`: whether or not the patient had diabetes
#' - `totChol`: total cholesterol level
#' - `sysBP`: systolic blood pressure
#' - `diaBP`: diastolic blood pressure
#' - `BMI`: Body Mass Index
#' - `heartRate`: heart rate
#' - `glucose`: glucose level
#' - `TenYearCHD`: Did the patient develop congestive heart disease during a 10 year follow-up? (1=Yes)
#'
#' @references  Description of [FHS by the National Heart, Lung, and Blood Institute](https://www.nhlbi.nih.gov/science/framingham-heart-study-fhs)
#'
#' @source [Kaggle](https://www.kaggle.com/datasets/dileep070/heart-disease-prediction-using-logistic-regression) and [Github repository](https://github.com/GauravPadawe/Framingham-Heart-Study)
"Framingham"
