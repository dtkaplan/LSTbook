# Produce the Forced expiratory volume dataset

fev = read.table(file = "./fev.dat.txt")

colnames(fev) = c("age", "FEV", "height", "sex", "smoker")

fev %>%
  mutate(
    sex = ifelse(sex==1, "M", "F"),
    smoker = ifelse(smoker==1, "smoker", "not")
  ) -> fev

FEV = tibble::as_tibble(fev)

# save(FEV, file="../data/FEV.rda")
