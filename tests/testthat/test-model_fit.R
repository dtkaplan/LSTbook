test_that("lm (or gaussian) works", {
  baseline <- lm(mpg ~ hp + cyl, data=mtcars)
  to_test  <- mtcars |> model_fit(mpg ~ hp + cyl)
  expect_identical(coef(baseline), coef(to_test))
  expect_identical(conf_interval(baseline), conf_interval(to_test))
})

test_that("wrangle=TRUE argument causes a dataframe to be returned", {
  simple <- Framingham |> model_fit(age ~ education, wrangle=TRUE)

  one  <- mtcars |> model_fit(mpg ~ hp + cyl, wrangle=TRUE)
  expect_true(is.data.frame(one))
  logistic <- Framingham |>
    model_fit(TenYearCHD ~ age + sex, family="binomial", wrangle=TRUE)
  expect_true(is.data.frame(logistic))
})
