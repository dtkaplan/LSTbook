test_that("Can generate categorical variables with multiple levels.", {
  SIM <- datasim_make(x ~ categorical(n, c("one", "two", "three")))
  DF <- sample(SIM, n=99)
  expect_true(all(DF$x %in% c("one", "two", "three")))
})

test_that("Relative probabilities work with categorical generation.", {
  D <- datasim_make(x ~ categorical(n, c(one=1, two=2, three=3)))
  S <- sample(D, n=6000)
  # There should be about one-third as many "one"s and "three"s
  # The following are just rough checks but are pretty safe
  expect_true(sum(S$x=="one") < 1500)
  expect_true(sum(S$x=="three") > 2500)
})

test_that("Relative probabilities work with categorical generation.", {
  D <- datasim_make(x ~ categorical(n, c("one", "two", "three"), exact=TRUE))
  S <- sample(D, n=6000)
  # There should be about one-third as many "one"s and "three"s
  # The following are just rough checks but are pretty safe
  expect_true(sum(S$x=="one") == 2000)
  expect_true(sum(S$x=="three") == 2000)
})

test_that("rounding works", {
  D <- datasim_make(x ~ round(runif(n, 0,10)))
  S <- sample(D, n = 100)
  expect_equal(S$x, round(S$x))
})

test_that("Translation from categorical to numerical works.", {
          D <- datasim_make(
            g ~ categorical(n, c("A", "B", "C")),
            y ~ cat2value(g, A=1, B=2, C=10)
            )
          Samp <- sample(D, n=100)
          Summary <- Samp |> dplyr::summarize(m = mean(y), .by = g) |> dplyr::arrange(g)
          expect_equal(Summary$m, c(1,2,10))
         } )
