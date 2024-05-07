test_that("mix_with() achieves the requested R2 and variance", {
  Sim <- datasim_make(
    x <- rnorm(n),
    y <- mix_with(x, R2 = 0.73, var = 2.1, exact = TRUE)
  )
  Dat <- Sim |> take_sample(n = 10000)
  expect_equal(var(Dat$y), 2.1)
  modr2 <- Dat |> model_train(y ~ x) |> R2()
  expect_equal(modr2$Rsquared, 0.73)
})

test_that("mix_with() throws appropriate warnings and errors.", {
  Sim <- datasim_make(
    x <- rnorm(n),
    y <- mix_with(x, R2 = -0.73, var = 2.1, exact = TRUE)
  )
  expect_error(Sim |> take_sample(n = 10000))
  Sim <- datasim_make(
    x <- rnorm(n),
    y <- mix_with(x, R2 = 0.73, var = -2.1, exact = TRUE)
  )
  expect_warning(Sim |> take_sample(n = 10000))
})
