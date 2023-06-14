test_that("Can handle four variables", {
  mod <- glm(TenYearCHD ~ age + sex + diabetes + BPMeds, data=Framingham, family=binomial)
  model_plot(mod, show_data=FALSE)

})

test_that("Puts numbers at end of line segments when color variable is continuous.", {
  mod <- mtcars |> model_train(mpg ~ wt + hp)
  model_plot(mod)
})
