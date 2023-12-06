test_that("default sample size is 5", {
  Foo <- sample(datasim_make(x <- exo(n, .3), y <- x + exo(n, .3)))
  expect_equal(nrow(Foo), 5)
  expect_equal(names(Foo), c("x", "y"))
})

test_that("n parameter is used", {
  Foo <- sample(datasim_make(x ~ exo(n, sd = 0.5), y ~ x + exo(n, sd = 0.5)), n=10000)
  expect_equal(nrow(Foo), 10000)
  expect_equal(sum(is.na(Foo$y)), 0)
})

test_that("exo() generates noise of the right magnitude", {
  Foo <- sample(datasim_make(x ~ exo(n, sd = 0.5),
                             y ~ x + exo(n, sd = 0.5)),
                n = 10000)
  expect_lt(abs(sd(Foo$x) - 0.5), 0.1)
  expect_lt(abs(sd(Foo$y) - sqrt(0.5)), 0.1)
})

test_that("runif() generates the uniform-distribution.", {
  Foo <- sample(datasim_make(x ~ runif(n, min = -2, max = 2),
                             prob ~ punif(x, min = -2, max = 2)),
                n=10000)
  expect_gt(max(Foo$prob), 0.99)
  expect_lt(min(Foo$prob), 0.01)
  expect_lt(abs(mean(Foo$prob) - 0.5), 0.02)
})

test_that("tdist() generates the t-distribution.", {
  Foo <- sample(datasim_make(x ~ rt(n, df = 1), prob ~ pt(x, df = 1)), n=10000)
  expect_gt(max(Foo$prob), 0.99)
  expect_lt(min(Foo$prob), 0.01)
  expect_lt(abs(mean(Foo$prob) - 0.5), 0.02)
})

test_that("seq() generates a sequence", {
  Foo <- sample(datasim_make(x ~ seq(n)), n=100)
  expect_equal(Foo$x, 1:100)
})

test_that("roll() draws a series of values from a set of levels.", {
  Foo <- sample(datasim_make(x ~ sample(1:6, n=n, replace = TRUE)), n=5000)
  expect_gt(mean(Foo$x == 6), 1/8)
  expect_gt(mean(Foo$x == 5), 1/8)
  expect_gt(mean(Foo$x == 4), 1/8)
  expect_gt(mean(Foo$x == 3), 1/8)
  expect_gt(mean(Foo$x == 2), 1/8)
  expect_gt(mean(Foo$x == 1), 1/8)
})

test_that("replicate() runs the expression independently for each row", {
  Foo <- sample(datasim_make(x <- replicate(n, sum(runif(2)))), n=1000)
  # Should generate a triangular distribution between 0 and 2
  expect_lt(mean(Foo$x < 0.5), 1/6) # about 1/8 of samples should be here
  expect_lt(mean(Foo$x > 1.5), 1/6)
})

test_that("Constant patterns are replicated to have nrow", {
  Foo <- sample(datasim_make(x ~ rep(3, n)), n=10)
  expect_equal(nrow(Foo), 10)
  Foo2 <- sample(datasim_make(x <- 3), n=10)
  expect_equal(nrow(Foo), 10)
  Foo3 <- sample(datasim_make(x ~ rep(1:3, length.out=n)), n=10)
  expect_equal(sum(Foo3$x == 1), 4)
  Foo4 <- sample(datasim_make(x <- 1:3), n=10)
  expect_equal(sum(Foo4$x == 1), 4)
})

test_that("Names starting with dots don't appear in the output.", {
  Foo <- sample(datasim_make(.genes ~ exo(n), x ~ .genes + exo(n), y ~ .genes + exo(n)))
  expect_equal(names(Foo), c("x", "y") )
})

test_that("block_by() generates a new categorical variable with correct levels", {
  Sim <- datasim_make(x <- exo(n), y <- block_by(x))
  Foo <- sample(Sim, n = 100)
  expect_equal(sum(Foo$y == "treatment"), 50)
  Goo <- Foo |> arrange(x) |> mutate(row = row_number()) |> summarize(m=mean(row), .by = y)
  expect_true(all(Goo$m - 50 < 1))
  Sim2 <- datasim_make(x <- exo(n),
                       treatment <- block_by(x, levels = c("a", "b", "c")),
                       block <- block_by(x, levels=c("a", "b", "c"), show_block=TRUE),
                       z <- cat2value(treatment, a=1, b=2, c=3))
  Foo <- sample(Sim2, n = 102) |> # sample size is multiple of number of blocking levels
    summarize(total = sum(z), .by = block)
  # There should be an a, b, c in each block, so z will be 6 in each block
  expect_equal(sum(Foo$total-6), 0)
})

test_that("categorical generates levels with right probabilities", {
  Sim <- datasim_make(g <- categorical(n, a=1, b=2, c=3, d=4, exact=TRUE))
  Foo <- sample(Sim, n=10000)

  expect_true(all(table(Foo$g) == c(1000, 2000, 3000, 4000)))
  set.seed(101)
  Sim <- datasim_make(g <- categorical(n=n, a=1, b=2, c=3, d=4, exact=FALSE))
  Foo <- sample(Sim, n=10000)
  expect_true(all(table(Foo$g) == c(983, 1997, 2989, 4031)))
})

test_that("bernoulli() can use log odds", {
  Sim <- datasim_make(x <- exo(n), z <- bernoulli(logodds = x))
  Foo <- sample(Sim, n=1000)
  expect_true(sum(Foo$z == 0) - 5000 < 200) # about equal numbers of ones and zeros
})
