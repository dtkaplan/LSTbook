test_that("1-variable plots work", {
  P <- mtcars |> point_plot(cyl ~ 1)
  expect_snapshot_file(gg_in_tmp_png(P), "1-var-plot.png")
})

test_that("Categorical responses trigger logistic regression", {
  P <- Birdkeepers |> point_plot(LC ~ YR, annot = "model")
  # Check that model function curves between NoCancer and Lung Cancer
  expect_snapshot_file(gg_in_tmp_png(P), "bird-logistic-plot.png")
})

# Logistic models show up in the right spot compared to the data
test_that("Logistic model values are 0-1", {
  Birdkeepers <- Birdkeepers |> mutate(cancer = zero_one(LC, one="LungCancer"))
  Birdkeepers |> point_plot(cancer ~ splines::ns(CD, 2), annot = "model") -> P
  # Look at the plot to make sure band goes in range [0, 1]
  expect_snapshot_file(gg_in_tmp_png(P), "logistic-fun-plot.png")
})

test_that("NAs in color or faceting variables cause the corresponding row to be deleted.", {
  P <- Hill_racing |> point_plot(time ~ distance + climb + climb)
  expect_snapshot_file(suppressWarnings(gg_in_tmp_png(P)), "color-and-facet-na.png")
})

