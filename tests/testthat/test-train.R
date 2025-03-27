test_that("environment is preserved against name conflicts (per issue 14)", {
  k <- 5
  tmp <-data.frame(x = k) |> trials(3)
  expect_equal(nrow(tmp), 3)
  expect_true("x" %in% names(tmp))
  expect_equal(sum(tmp$x), 15)
})

test_that("iteration is done over other arguments to trials()", {
  ntrials <- 4
  k <- 10
  tmp <- data.frame(x = k, y = j^2) |> trials(4, j = 1:2)
  expect_equal(nrow(tmp), ntrials*2)
  expect_equal(sum(tmp$j), 3*ntrials)
})
