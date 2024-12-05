test_that("automatic model family selection works", {
  mod <- Birdkeepers |> model_train(LC ~ YR)
  expect_true(mod$family$family == "binomial")
  mod <- Birdkeepers |> model_train(YR ~ LC)
  expect_true(inherits(mod, "lm") && !inherits(mod, "glm"))
})


test_that("lm (or gaussian) works", {
  baseline <- lm(mpg ~ hp + cyl, data=mtcars)
  to_test  <- mtcars |> model_train(mpg ~ hp + cyl)
  expect_identical(stats::coef(baseline), stats::coef(to_test))
  expect_identical(conf_interval(baseline), conf_interval(to_test))
})

test_that("Zero-one transform of response in tilde expression works to trigger logistic regression", {
  mod <- Whickham |> model_train(zero_one(outcome, one="Dead") ~ smoker + age)
  expect_true(inherits(mod, "glm"))
})


