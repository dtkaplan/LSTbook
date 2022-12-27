
test_that("model_eval() repeats input values and adds columns named .output, .lwr, .upr", {
  mod <- lm(mpg ~ wt + hp, data = mtcars)
  Tmp <- model_eval(mod, wt = c(2, 4), hp=c(100,200,300))
  expect_equal(c("wt", "hp", ".output", ".lwr", ".upr"), names(Tmp))
})

test_that("model_eval() handles ... inputs to assemble an input data set", {
  mod <- lm(mpg ~ wt + hp, data = mtcars)
  Tmp <- model_eval(mod, wt = c(2, 4), hp=c(100,200,300))
  expect_equal(nrow(Tmp), 6)
})

test_that("model_eval() uses training data as input if no other is specified", {
  mod <- lm(mpg ~ wt + hp, data = mtcars)
  Tmp <- model_eval(mod)
  expect_equal(nrow(Tmp), nrow(mtcars))
})

test_that("model_eval() generates a skeleton of inputs when requested", {
  mod <- lm(mpg ~ wt + hp, data = mtcars)
  Tmp <- model_eval(mod, skeleton = TRUE, ncont=5)
  expect_true(length(unique(Tmp$wt)) >= 5)
  expect_true(length(unique(Tmp$hp)) >= 5)
  expect_true(nrow(Tmp) > 25)
})
