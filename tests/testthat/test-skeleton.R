test_that("model_skeleton() generates a skeleton of inputs when requested", {
  mod <- mtcars |> model_train(mpg ~ wt + hp)
  Tmp <- model_skeleton(mod, ncont=5)
  expect_true(length(unique(Tmp$wt)) >= 5)
  expect_true(length(unique(Tmp$hp)) >= 5)
  expect_true(nrow(Tmp) > 25)
  expect_true(ncol(Tmp) == 2)
})

test_that("model_skeleton() gets untransformed explanatory variables", {
  mod <- mosaicData::Galton |> model_train(height ~ splines::ns(father, 4) * poly(mother,3) * sex)
  Tmp <- model_skeleton(mod)
  expect_true(all(names(Tmp) %in% c("father", "mother", "sex")))
})

test_that("typical values preserve order of factor levels", {
  tmp <- LSTbook:::get_typical(Penguins$species, type = "discrete")
  expect_true(all(levels(tmp) == levels(Penguins$species)))
})
