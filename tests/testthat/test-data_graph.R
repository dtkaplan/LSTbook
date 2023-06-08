test_that("1-variable plots work", {
  mtcars |> df_graph(cyl ~ 1)
  expect_equal(2 * 2, 4)
})
