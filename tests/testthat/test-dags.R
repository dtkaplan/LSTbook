test_that("Can generate categorical variables with multiple levels.", {
  D <- dag_make(x ~ categorical(c("one", "two", "three")))
  S <- sample(D, size=99)
  expect_true(all(S$x %in% c("one", "two", "three")))

})

test_that("Relative probabilities work with categorical generation.", {
  D <- dag_make(x ~ categorical(c("one", "two", "three"), even=FALSE, probs=c(1,2,3)))
  S <- sample(D, size=6000)
  # There should be about one-third as many "one"s and "three"s
  # The following are just rough checks but are pretty safe
  expect_true(sum(S$x=="one") < 1500)
  expect_true(sum(S$x=="three") > 2500)

})

test_that("Relative probabilities work with categorical generation.", {
  D <- dag_make(x ~ categorical(c("one", "two", "three"), exact=TRUE))
  S <- sample(D, size=6000)
  # There should be about one-third as many "one"s and "three"s
  # The following are just rough checks but are pretty safe
  expect_true(sum(S$x=="one") == 2000)
  expect_true(sum(S$x=="three") == 2000)
})

test_that("rounding works", {
  D <- dag_make(x ~ round(unif(0,10)))
  S <- sample(D, size=100)
  expect_equal(S$x, round(S$x))
})

test_that("Translation from categorical to numerical works.", {
          D <- dag_make(
            g ~ categorical(levels=c("A", "B", "C")),
            y ~ value(g, c("A", "B", "C"), c(1,2,10))
            )
          Samp <- sample(D, size=100)
          expect_equal(df_stats(y ~ g, data=Samp, mean)$mean, c(1,2,10))


         } )
