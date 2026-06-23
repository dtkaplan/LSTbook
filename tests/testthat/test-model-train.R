test_that("model_train() sets attributes appropriately", {
  m <- mtcars |> model_train(mpg ~ hp * wt)
  expect_true(all(c("explan_names", "training_data") %in% names(attributes(m))))
})


test_that("explanatory var names are found in output from model_train", {
  knots <- 1:2
  m <- mtcars |> model_train(mpg ~ splines::bs(hp, knots = knots) * wt)
  expect_true(all(LSTbook:::explanatory_vars(m) %in% c("hp", "wt")))
  # but this doesn't work if lm() is used instead of model_train()
  m2 <- lm(mpg ~ splines::bs(hp, knots = knots) * wt, data = mtcars)
  expect_false(all(LSTbook:::explanatory_vars(m2) %in% c("hp", "wt")))
})

