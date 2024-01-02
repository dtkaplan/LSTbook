test_that("1-variable plots work", {
  P <- mtcars |> point_plot(cyl ~ 1)
  expect_snapshot_file(ggsave("1-var-plot.png", P))
})


test_that("Transformations work", {
  Babies <- natality2014::Natality_2014_10k |> select(combgest, dbwt) |> stats::na.omit()
  Babies |> point_plot(dbwt ~ ntiles(combgest, n=3), annot="violin", point_ink = 0.01) -> P
  # look for 4 violins, including one  for NA
  expect_snapshot_file(ggsave("transform-plot.png", P))
})

test_that("Categorical responses trigger logistic regression", {
  P <- Birdkeepers |> point_plot(LC ~ YR, annot = "model")
  # Check that model function curves between NoCancer and Lung Cancer
  expect_snapshot_file(ggsave("bird-logistic-plot.png"))
})

# Logistic models show up in the right spot compared to the data
test_that("Logistic model values are 0-1", {
  Birdkeepers <- Birdkeepers |> mutate(cancer = zero_one(LC, one="LungCancer"))
  Birdkeepers |> point_plot(cancer ~ splines::ns(CD, 2), annot = "model") -> P
  # Look at the plot to make sure band goes in range [0, 1]
  expect_snapshot_file(ggsave("logistic-fun-plot.png", P))
})

test_that("BUG IS FIXED", {
  Clock_auction |>
    point_plot(price ~ bidders + splines::ns(age,2), annot = "model")

})
