test_that("model_values function works outside of mutate", {
  tmp <- model_values(mtcars, mpg ~ hp + wt, family="lm")
  tmp2 <- lm(mpg ~ hp + wt, data=mtcars) |> predict()
  expect_identical(tmp, tmp2)
})

test_that("model_values function works INSIDE mutate", {
  Tmp <- mtcars |> mutate(mvals = model_values(mpg ~ hp + wt, family="lm"))
  tmp2 <- lm(mpg ~ hp + wt, data=mtcars) |> predict() |> as.vector()
  expect_identical(Tmp$mvals, tmp2)
  })

test_that("works in spite of missing values", {
  mtcars$hp[5:10] <- NA
  mtcars$mpg[7:12] <- NA
  Tmp <- mtcars |> mutate(mvals = model_values(mpg ~ hp + wt, family="lm"))
  expect_equal(nrow(Tmp), nrow(mtcars))
  })
