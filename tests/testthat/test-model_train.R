test_that("automatic model family selection works", {
  mod <- Birdkeepers |> model_train(LC ~ YR)
  expect_true(mod$family$family == "binomial")
  mod <- Birdkeepers |> model_train(YR ~ LC)
  expect_true(inherits(mod, "lm") && !inherits(mod, "glm"))
})


test_that("lm (or gaussian) works", {
  baseline <- lm(mpg ~ hp + cyl, data=mtcars)
  to_test  <- mtcars |> model_train(mpg ~ hp + cyl)
  expect_identical(coef(baseline), coef(to_test))
  expect_identical(conf_interval(baseline), conf_interval(to_test))
})

