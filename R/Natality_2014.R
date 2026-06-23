#' Medical info on each birth in the US in 2014
#'
#' The Centers for Disease Control collects data on all births registered
#' in the US (50 states + DC). \code{Natality_2014}, contains a random sample of
#' 10,000 rows and 55 variables related to potential risk factors and outcomes from the CDC data.
#'
#' Variable names are the same as in the original CDC file.
#'
#' Variables have been re-coded to translate markers like 99 into `NA` and to
#' self-document the levels of the variables.
#'
#' Note that the data include only registered births. Fetal deaths are not included.
#' For reference, there were about 24,000 fetal deaths (20 weeks and thereafter) in the US in 2013. (See \url{http://www.cdc.gov/nchs/data/nvsr/nvsr64/nvsr64_08.pdf}.)
#' This corresponds to about 0.6% of the births listed in these data. The CDC also
#' reports about 16,000 infant deaths before age 28 days and another 7500 deaths before age 1 year.
#'
#' @details
#' The full set of 3.99 million cases from the CDC are not available
#' via this `{LSTbook}` package. You can install the "dtkaplan/natality2014" package from
#' GitHub. You will need a laptop or server-based version of R to do this. COMMENT
#'
#' @docType data
#' @name Natality_2014
#'
#' @keywords datasets
#'
#' @format A data frame with a random sample of size 100000 from the complete CDC set of 3,998,175 cases, each of which is a birth in the US in 2014.
#' \describe{
#'   \item{mager}{Mother's age at date of birth}
#'   \item{fagecomb}{Father's age at date of birth}
#'   \item{ubfacil}{Type of facility where the birth took place}
#'   \item{urf_diab}{Mother had diabetes before pregnancy}
#'   \item{urf_chyper}{Mother had chronic hypertension before pregnancy}
#'   \item{urf_phyper}{Mother developed hypertension during pregnancy}
#'   \item{urf_eclam}{Mother developed eclampsia}
#'   \item{uop_induc}{Was labor induced?}
#'   \item{uld_breech}{Did baby present as breech? See \code{me_pres}.}
#'   \item{ilive}{Whether the baby was alive at the time of the report.}
#'   \item{ab_aven1}{Baby put on mechanical ventilator immediately}
#'   \item{ab_aven6}{Baby still on ventilator after 6 hours}
#'   \item{ab_nicu}{Baby transferred to intensive care unit.}
#'   \item{ab_surf}{Surfactant administered to newborn}
#'   \item{ab_anti}{Antibiotics administered to newborn}
#'   \item{ab_seiz}{Newborn had seizure}
#'   \item{dbwt}{Baby's weight (gm)}
#'   \item{combgest}{Length of gestation}
#'   \item{sex}{Baby's sex}
#'   \item{dplural}{Plurality of birth: 2 = twins, 3 = triplets, ...}
#'   \item{apgar5}{APGAR score at 5 minutes}
#'   \item{apgar10}{APGAR score at 10 minutes}
#'   \item{mtran}{Mother transferred. No other description given.}
#'   \item{pay}{Source of payment for delivery}
#'   \item{mm_mtr}{Mother received blood transfusion}
#'   \item{mm_plac}{Perineal laceraction}
#'   \item{mm_rupt}{Ruptured uterus}
#'   \item{mm_uhyst}{Unplanned hysterectomy}
#'   \item{mm_aicu}{Mother transferred to intensive care unit}
#'   \item{me_pres}{Presentation of baby on delivery. See \code{uld_breech}. It's not clear why "other" corresponds to breech.}
#'   \item{me_rout}{Method of delivery}
#'   \item{ld_indl}{Labor induced}
#'   \item{pwgt_r}{Mother's weight before pregnancy}
#'   \item{dwgt_r}{Mother's weight at delivery}
#'   \item{m_ht_in}{Mother's height}
#'   \item{cig_0}{Number of cigarettes smoked daily before pregnancy}
#'   \item{cig_1}{Number of cigarettes smoked daily during first trimester}
#'   \item{cig_2}{Number of cigarettes smoked daily during second trimester}
#'   \item{cig_3}{Number of cigarettes smoked daily during third trimester}
#'   \item{wic}{Enrolled in Women, Infants, and Children (WIC) program for supplemental nutrition.}
#'   \item{precare}{Month started in prenatal care. 15 means never started.}
#'   \item{previs}{Number of visits to prenatal care.}
#'   \item{priorlive}{Number of previous live births.}
#'   \item{priordead}{Number of previous births where baby died.}
#'   \item{priorterm}{Number of terminations of pregnancy before this birth.}
#' }

#'
#' @examples
#' table(Natality_2014$ilive)
"Natality_2014"
