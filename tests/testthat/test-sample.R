test_that("sample() works without a `.by` argument", {
  one <- sample(mtcars, n=1)
  expect_equal(1, nrow(one))
})

test_that("sample() works with a character string `.by` argument", {
  one <- sample(mtcars, n=1, .by = "cyl")
  expect_equal(3, nrow(one))
  two <- sample(mtcars, n=1, .by = c("cyl", "vs"))
  expect_equal(5, nrow(two))
})

test_that("sample() works with unquoted names in `.by` argument", {
  one <- sample(mtcars, n=1, .by = cyl)
  expect_equal(3, nrow(one))
  two <- sample(mtcars, n=1, .by = c(cyl, vs))
  expect_equal(5, nrow(two))
})

test_that("size overrides n in sample()", {
  one <- sample(mtcars, size=10)
  expect_equal(10, nrow(one))
  two <- sample(mtcars, n = 3, size = 7)
  expect_equal(7, nrow(two))
})

test_that("sample() works with datasim object", {
  one <- sample(dag01, n=3)
  expect_equal(3, nrow(one))
})

test_that("deprecation of `size` argument works", {
  expect_warning(sample(mtcars, n=3, size=5))
  expect_warning(sample(dag01, n=3, size=5))
  sample(1:10, n = 3)
})
