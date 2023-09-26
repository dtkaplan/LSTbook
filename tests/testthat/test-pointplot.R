test_that("1-variable plots work", {
  mtcars |> df_graph(cyl ~ 1)
  expect_equal(2 * 2, 4)
})


test_that("Transformations work", {
  Babies <- natality2014::Natality_2014_10k
  Babies |> data_graph(dbwt ~ as.factor(combgest), annot="violin")
})
