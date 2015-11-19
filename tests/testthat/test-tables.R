context("EC 50 accuracy tests")

data("dummydata")

test_that("EC 50 results are correct", {
  expected <- structure(list(
    Estimate.10 = c(0.000660138648614638, 0.000953981208502841),
    SE.10 = c(0.000875010870868628, 0.00117863678191537),
    Estimate.50 = c(0.0924087889397188,  0.0828767375276147),
    SE.50 = c(0.0471388603040392, 0.0409852652965899),
    Estimate.90 = c(12.935743561184, 7.19988356374494),
    SE.90 = c(9.7676676460528,  5.06079482886767)),
    .Names = c("Estimate.10", "SE.10", "Estimate.50", "SE.50", "Estimate.90", "SE.90"),
  row.names = c("NEW", "PI-07-002"), class = "data.frame")

  res <- EC_table(dummydata, form = response ~ dose, plot = FALSE)
  res_mod <- EC_table(dummydata, form = response ~ dose, plot = FALSE, result = "model")
  res_sum <- EC_table(dummydata, form = response ~ dose, plot = FALSE, result = "summary")

  expect_equivalent(res[2:1, ], expected)
  expect_that(res, is_a("data.frame"))
  expect_that(res_mod[[1]], is_a("drc"))
  expect_that(res_sum[[1]], is_a("summary.drc"))
})

test_that("EC_table needs a formula to work", {
  expect_error(EC_table(dummydata))
})
