test_that("Prediction error works", {
  mod <- mosaicData::Galton |> model_train(height ~ splines::ns(father,3)*poly(mother, 2)*sex)
  result <- suppressWarnings(model_pe(mod))
  expect_true( abs(result - 4.44231) < 0.0001 )
  # testdata feature works
  result2 <- model_pe(mod, testdata = head(mosaicData::Galton), error_type = "default")
  expect_true( abs(result2 - 0.41091) < 0.0001)
  })

# test_that("CV works", {
#   mod <- mosaicData::Galton |> model_train(height ~ father*mother*sex)
#   result <- suppressWarnings(model_cv(mod))
#   expect_true( abs(result - 4.44231) < 0.0001 )
# })

