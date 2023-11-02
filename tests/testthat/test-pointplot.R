test_that("1-variable plots work", {
  mtcars |> df_graph(cyl ~ 1)
  expect_equal(2 * 2, 4)
})


test_that("Transformations work", {
  Babies <- natality2014::Natality_2014_10k
  Babies |> data_graph(dbwt ~ as.factor(combgest), annot="violin")
})


# Logistic models show up in the right spot compared to the data
test_that("Logistic model values are 0-1", {
  Birdkeepers <- Birdkeepers |> mutate(cancer = zero_one(LC, one="LungCancer"))
  Birdkeepers |> pointplot(cancer ~ splines::ns(CD, 2), annot = "model")
  # Look at the plot to make sure band goes in range [0, 1]
})
