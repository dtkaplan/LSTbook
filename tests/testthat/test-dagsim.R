test_that("default sample size is 5", {
  Foo <- sample(dag_make(x ~ exo(.3), y ~ x + exo(.3)))
  expect_equal(nrow(Foo), 5)
  expect_equal(names(Foo), c("x", "y"))
})

test_that("size parameter is used", {
  Foo <- sample(dag_make(x ~ exo(.5), y ~ x + exo(.5)), size=10000)
  expect_equal(nrow(Foo), 10000)
  expect_equal(sum(is.na(Foo$y)), 0)
})

test_that("exo() generates noise of the right magnitude", {
  Foo <- sample(dag_make(x ~ exo(.5), y ~ x + exo(.5)), size=10000)
  expect_lt(abs(sd(Foo$x) - 0.5), 0.1)
  expect_lt(abs(sd(Foo$y) - sqrt(0.5)), 0.1)
})

test_that("unif() generates the uniform-distribution.", {
  Foo <- sample(dag_make(x ~ unif(min=-2, max=2), prob ~ punif(x, min=-2, max=2)), size=10000)
  expect_gt(max(Foo$prob), 0.99)
  expect_lt(min(Foo$prob), 0.01)
  expect_lt(abs(mean(Foo$prob) - 0.5), 0.02)
})

test_that("tdist() generates the t-distribution.", {
  Foo <- sample(dag_make(x ~ tdist(df=1), prob ~ pt(x, df=1)), size=10000)
  expect_gt(max(Foo$prob), 0.99)
  expect_lt(min(Foo$prob), 0.01)
  expect_lt(abs(mean(Foo$prob) - 0.5), 0.02)
})

test_that("seq() generates a sequence", {
  Foo <- sample(dag_make(x ~ seq()), size=100)
  expect_equal(Foo$x, 1:100)
})

test_that("roll() draws a series of values from a set of levels.", {
  Foo <- sample(dag_make(x ~ roll(1:6)), size=5000)
  expect_gt(mean(Foo$x == 6), 1/8)
  expect_gt(mean(Foo$x == 5), 1/8)
  expect_gt(mean(Foo$x == 4), 1/8)
  expect_gt(mean(Foo$x == 3), 1/8)
  expect_gt(mean(Foo$x == 2), 1/8)
  expect_gt(mean(Foo$x == 1), 1/8)
})

test_that("each() runs the expression independently for each row", {
  Foo <- sample(dag_make(x ~ each(sum(runif(2)))), size=1000)
  # Should generate a triangular distribution between 0 and 2
  expect_lt(mean(Foo$x < 0.5), 1/6) # about 1/8 of samples should be here
  expect_lt(mean(Foo$x > 1.5), 1/6)
})

test_that("Constant patterns are replicated to have nrow", {
  Foo <- sample(dag_make(x ~ 3), size=10)
  expect_equal(nrow(Foo), 10)
  Foo2 <- sample(dag_make(x ~ 1:3), size=10)
  expect_equal(sum(Foo2$x == 1), 4)
})

test_that("Names starting with dots don't appear in the output.", {
  Foo <- sample(dag_make(.genes ~ exo(), x ~ .genes + exo(), y ~ .genes + exo()))
  expect_equal(names(Foo), c("x", "y") )
})

