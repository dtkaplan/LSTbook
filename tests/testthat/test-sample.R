test_that("take_sample() works without a `.by` argument", {
  one <- mtcars |> take_sample(n=1)
  expect_equal(1, nrow(one))
})

test_that("take_sample() works with a character string `.by` argument", {
  one <- take_sample(mtcars, n=1, .by = "cyl")
  expect_equal(3, nrow(one))
  two <- take_sample(mtcars, n=1, .by = c("cyl", "vs"))
  expect_equal(5, nrow(two))
})

test_that("take_sample() works with unquoted names in `.by` argument", {
  one <- take_sample(mtcars, n=1, .by = cyl)
  expect_equal(3, nrow(one))
  two <- take_sample(mtcars, n=1, .by = c(cyl, vs))
  expect_equal(5, nrow(two))
})


test_that("take_sample() works with datasim object", {
  one <- take_sample(sim_01, n=3)
  expect_equal(3, nrow(one))
})


