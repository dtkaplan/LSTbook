# test_that("model_values function works outside of mutate", {
# need to run this test manually, since test_that() disrupts the stack
  # tmp <- mtcars |> model_values(mpg ~ hp + wt, family="linear")
  # tmp2 <- lm(mpg ~ hp + wt, data=mtcars) |> predict()
  # expect_true(max(abs(as.numeric(tmp) - as.numeric(tmp2))) < 0.0001)
  # expect_true(TRUE)
# })

test_that("model_values function works INSIDE mutate", {
  Tmp <- mtcars |> mutate(mvals = model_values(mpg ~ hp + wt, family="linear"))
  tmp2 <- lm(mpg ~ hp + wt, data=mtcars) |> predict() |> as.vector()
  expect_identical(Tmp$mvals, as.numeric(tmp2))
  })

test_that("works in spite of missing values", {
  mtcars$hp[5:10] <- NA
  mtcars$mpg[7:12] <- NA
  Tmp <- mtcars |> mutate(mvals = model_values(mpg ~ hp + wt, family="linear"))
  expect_equal(nrow(Tmp), nrow(mtcars))
  })
