#' Records on births in the US in 2022
#'
#' These data come from the Centers for Disease Controls "public use file" recording
#' all 3,699,040 (known) births in the US in 2022. `Births2022` is a random
#' sample of size 20,000 from the comprehensive file
#'
#' @docType data
#'
#' @format  A data frame with 20,000 observations on the following 38 variables.
#' - `month`: 1-12
#' - `dow`: Day of week: Sun, Mon, Tues, ...
#' - `place`: hospital, home, clinic, etc.
#' - `paternity`: is paternity acknowledged. Y, N, and X. X stands for "not applicable"
#' which is shorthand for the mother is married (consequently the husband is presumed
#' to be the father).
#' - `meduc`: mother's educational level. <8 is 8th grade or less, HSG+ means
#' high school plus some college but no degree.
#' - `feduc`: father's education. Same coding as `meduc`.
#' - `married`: Is the mother married?
#' - `mage`: mother's age
#' - `fage`: father's age
#' - `total_kids`: how many total births to mother including this one.
#' - `interval`: months since last birth (if applicable).
#' - `prenatal_start`: Which trimester did the mother start prenatal care?
#' - `prenatal_visits`: How many total prenatal care visits.
#' - `mheight`: Mother's height in inches
#' - `wt_pre`: Mother's weight in pounds before pregnancy
#' - `wt_delivery`: Mother's weight in pounds at delivery
#' - `diabetes_pre`: Did the mother have diabetes before pregnancy
#' - `diabetes_gest`: Did the mother develop gestational diabetes
#' - `hbp_pre`: Did the mother have high blood pressure before pregancy
#' - `hbp_gest`: Did the mother develop high blood pressure during pregnancy
#' - `eclampsia`: Did the mother develop eclampsia
#' - `induction`: Was labor induced?
#' - `augmentation`: Was the uterus stimulated to increase frequency, duration, and
#' intensity of contractions.
#' - `anesthesia`: Was the mother given anesthesia?
#' - `presentation`: Baby's presentation at birth (e.g. cephalic or breech)
#' - `method`: method of delivery (vaginal or C-section)
#' - `trial_at_labor`: For mother's who delivered by C-section, was there an
#' attempt at labor.
#' - `attendant`: MD, DO, midwife, other
#' - `payment`: How was the bill paid?
#' - `apgar5`, `apgar10`: APGAR scores (0-10) at five and ten minutes after birth.
#' - `plurality`: singletons, twins, triplets, quadruplets (as an integer 1-4)
#' - `sex`: Baby's sex
#' - `duration`: Duration of gestation, in weeks by "obstretric estimate."
#' - `menses`: Last normal menses month: 1-12 (Jan-Dec)
#' - `weight`: Baby's weight (in grams)
#' - `living`: Baby living at time of birth report
#' - `breastfed`: Baby breastfed at time of discharge
#'
#' @keywords datasets
#'
#' @source US Centers for Disease Control "Natality Public Use File" <https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Births>
#'
#' @references "User Guide to the 2022 Natality Public Use File" <https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/DVS/natality/UserGuide2022.pdf>
"Births2022"

