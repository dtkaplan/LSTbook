test_that("Can handle four variables", {
  mod <- glm(TenYearCHD ~ age + sex + diabetes + BPMeds, data=Framingham, family=binomial)
  model_plot(mod, show_data=FALSE)

})
