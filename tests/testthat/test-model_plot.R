test_that("Can handle four variables", {
  mod <- Framingham |>
    model_train(TenYearCHD ~ age + sex + diabetes + BPMeds,
             family="binomial")
  P <- model_plot(mod)
  expect_snapshot(ggsave("four-facets.png", P))
})

