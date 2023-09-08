# Construct the Natality2022 files

# Data from https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Births
# 2022 data file: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/natality/Nat2022us.zip


# helper function to translate from raw codes to legible values
uncode <- function(var, codes, unknown) {
  if (length(names(codes)) > 0) {
    res <- rep(NA, length(var))
    for (k in names(codes)) {
      res[var == k] <- codes[[k]]
    }
    res
  }
  ifelse(var == unknown, NA, codes[var])
}


library(readr)
Raw_positions <- tibble::tribble(
  ~ start, ~ end, ~ name,
  9, 12, "year",
  13, 14, "month",
#  15, 16, "hour",
#  17, 18, "minute",
  23, 23, "dow",
  32, 32, "place",
  75, 76, "mage",
  119, 119, "paternity",
  120, 120, "married",
  124, 125, "meduc",
  147, 148, "fage",
  163, 164, "feduc",
  182, 183, "total_kids",
  201, 202, "interval", # 88 is 1st live birth
  227, 227, "prenatal_start",
  238, 239, "prenatal_visits",
  # 253, 254, "cig0",
  # 255, 256, "cig1",
  # 257, 258, "cig2",
  # 259, 260, "cig3",
  # 270, 271, "cig_reported",
  280, 281, "mheight",
  292, 294, "wt_pre",
  299, 301, "wt_delivery",
  313, 313, "diabetes_pre",
  314, 314, "diabetes_gest",
  315, 315, "hbp_pre",
  316, 316, "hbp_gest",
  317, 317, "eclampsia",
  383, 383, "induction",
  384, 384, "augmentation",
  388, 388, "anesthesia",
  401, 401, "presentation",
  402, 402, "method",
  403, 403, "trial_at_labor",
  433, 433, "attendant",
  436, 436, "payment",
  444, 445, "apgar5",
  448, 449, "apgar10",
  454, 454, "plurality",
  475, 475, "sex",
  477, 478, "menses",
  490, 491, "duration",
  504, 507, "weight",
  568, 568, "living",
  569, 569, "breastfed"
)

Raw <- read_fwf("inst/Natality2022/Nat2022us.zip",
         fwf_positions(Raw_positions$start,
                       Raw_positions$end,
                       Raw_positions$name),
         n_max = 1000000)

set.seed(999)
Raw <- Raw |> sample_n(20000)

Births2022 <- Raw[c("month", "dow")]

dow_codes <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
interval_codes <- c("00" = "plural", "88" = "first", "01"='4-11',
                    "02" = "12-17", "03" = "18-23", "04" = "24-35",
                    "05" = "36-47", "06" = "48-59", "07" = "60-71",
                    "08" = "72+")
paternity_codes <- c(Y = "Y", X = "Y", N="N")
place_codes <- c("hospital", "freestanding", "home (intended)", "home (unintended)",
                 "home (unknown)", "clinic", "other")
educ_codes <- c("<8", "<12", "HS", "HS+", "Assoc", "Bachelors", "Masters", "Prof")

Births2022$dow <- uncode(Raw$dow, dow_codes, 99)
Births2022$dow <- factor(Births2022$dow, dow_codes, ordered = TRUE)

Births2022$place <- uncode(Raw$place, place_codes, 9)


Births2022$paternity <- uncode(Raw$paternity, paternity_codes, "U")
Births2022$meduc <- uncode(Raw$meduc, educ_codes, 9)
Births2022$meduc <- factor(Births2022$meduc, levels=educ_codes, ordered=TRUE)
Births2022$feduc <- uncode(Raw$feduc, educ_codes, 9)
Births2022$feduc <- factor(Births2022$feduc, levels=educ_codes, ordered=TRUE)
Births2022$married <- uncode(Raw$married, c("married", "not_married"), "bogus")
Births2022$married[is.na(Births2022$married)] <- "NA"
Births2022$fage <- ifelse(Raw$fage == 99, NA, as.numeric(Raw$fage))
Births2022$mage <- ifelse(Raw$mage == 99, NA, as.numeric(Raw$mage))
Births2022$total_kids <- uncode(Raw$total_kids, 1:8, 9)

Births2022$interval <- uncode(Raw$interval, interval_codes, "99")
Births2022$interval <- factor(Births2022$interval, levels=interval_codes, ordered = TRUE)
trimesters <- c("first", "second", "third", "none")
Births2022$prenatal_start <- uncode(Raw$prenatal_start, trimesters, 5)
Births2022$prenatal_start <- factor(Births2022$prenatal_start, trimesters, ordered=TRUE)
Births2022$prenatal_visits <- ifelse(Raw$prenatal_visits==99, NA, as.numeric(Raw$prenatal_visits))
Births2022$mheight <- ifelse(Raw$mheight == 99, NA, as.numeric(Raw$mheight))
Births2022$wt_pre <-  ifelse(Raw$wt_pre == 999, NA, as.numeric(Raw$wt_pre))
Births2022$wt_delivery <- ifelse(Raw$wt_delivery==999, NA, as.numeric(Raw$wt_delivery))
Births2022$diabetes_pre <- ifelse(Raw$diabetes_pre == "U", NA, Raw$diabetes_pre)
Births2022$diabetes_gest <- ifelse(Raw$diabetes_gest == "U", NA, Raw$diabetes_gest)
Births2022$hbp_pre <- ifelse(Raw$hbp_pre == "U", NA, Raw$hbp_pre)
Births2022$hbp_gest <- ifelse(Raw$hbp_gest == "U", NA, Raw$hbp_gest)
Births2022$eclampsia <- ifelse(Raw$eclampsia == "U", NA, Raw$eclampsia)
Births2022$induction <- ifelse(Raw$induction == "U", NA, Raw$induction)
Births2022$augmentation <- ifelse(Raw$augmentation == "U", NA, Raw$augmentation)
Births2022$anesthesia <- ifelse(Raw$anesthesia == "U", NA, Raw$anesthesia)
Births2022$presentation <- uncode(Raw$presentation, c("cephalic", "breech", "other"), 9)
Births2022$method <- uncode(Raw$method,
                                    c("spontaneous", "forceps", "vacuum", "cesarean"), 9)
Births2022$trial_at_labor <- ifelse(Raw$trial_at_labor == "U", NA, Raw$trial_at_labor)
Births2022$attendant <- uncode(Raw$attendant,
                                       c("MD", "DO", "midwife", "midwife", "other"), 9)
Births2022$payment <- uncode(Raw$payment,
                                     c("medicaid", "private", "self_pay", "other"), 9)
Births2022$apgar5 <-  ifelse(Raw$apgar5  == 99, NA, as.numeric(Raw$apgar5))
Births2022$apgar10 <- ifelse(Raw$apgar10 == 99, NA, as.numeric(Raw$apgar10))
Births2022$plurality <- as.integer(Raw$plurality)
Births2022$sex <- Raw$sex
Births2022$duration <- ifelse(Raw$duration==99, NA, as.numeric(Raw$duration))
Births2022$menses <- ifelse(Raw$menses == 99, NA, as.numeric(Raw$menses))
Births2022$weight <- ifelse(Raw$weight==9999, NA, as.numeric(Raw$weight))
Births2022$living <- ifelse(Raw$living=="U", NA, Raw$living)
Births2022$breastfed <- ifelse(Raw$breastfed=="U", NA, Raw$breastfed)



save(Births2022, file="data/Births2022.rda", compress = TRUE)


# Compare place and method. Explain.
# Compare place and anesthesia. Explain.
# Find most common conception month. Adjust for length of month.
# Sex versus mother's age. What's the effect size on sex of age.
