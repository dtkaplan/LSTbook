test_that("model_skeleton() generates a skeleton of inputs when requested", {
  mod <- lm(mpg ~ wt + hp, data = mtcars)
  Tmp <- model_skeleton(mod, ncont=5)
  expect_true(length(unique(Tmp$wt)) >= 5)
  expect_true(length(unique(Tmp$hp)) >= 5)
  expect_true(nrow(Tmp) > 25)
  expect_true(ncol(Tmp) == 2)
})

# These tests are a legacy from the {mosaicModel} package
#
# test_that("continuous_or_discrete() works", {
#   expect_equal(continuous_or_discrete(Hill_racing$distance), "continuous")
#   expect_equal(continuous_or_discrete(Hill_racing$sex), "discrete")
#   expect_equal(continuous_or_discrete(Hill_racing$distance > 5), "discrete")
#   expect_equal(continuous_or_discrete(zero_one(Hill_racing$sex)), "discrete")
# })
#
# test_that("get_typical() works for continuous variables", {
#   expect_equal(get_typical(Hill_racing$distance, type="discrete", nlevels=3), c(3.5, 12.75, 22.00))
#   expect_equal(
#     length(get_typical(Hill_racing$distance, type="continuous", ncont=25)), 25)
# })
#
# test_that("get_typical() works for booleans", {
#   expect_equal(get_typical(Hill_racing$distance > 10), c(FALSE, TRUE))
# })
#
# test_that("get_typical() works for zero_one variables", {
#   expect_equal(get_typical(zero_one(Hill_racing$sex)),
#                unique(zero_one(Hill_racing$sex)))
# })
#
# test_that("get_typical() works for character-string variables", {
#   expect_equal(get_typical(Hill_racing$name, nlevels=4),
#                c("Abi Ven Heerver", "Adam Gatens", "Agustí Roc", "Al Hart" ))
# })

