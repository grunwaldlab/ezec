context("EC 50 accuracy tests")

expected <- structure(list(
  sample = c("Sample1", "Sample2"),
  Estimate.10 = c(0.000953981208502841, 0.000660138648614638),
  SE.10 = c(0.00117863678191537, 0.000875010870868628),
  Estimate.50 = c(0.0828767375276147, 0.0924087889397188),
  SE.50 = c(0.0409852652965899, 0.0471388603040392),
  Estimate.90 = c(7.19988356374494, 12.935743561184),
  SE.90 = c(5.06079482886767, 9.7676676460528)),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -2L),
  .Names = c("sample", "Estimate.10", "SE.10", "Estimate.50",
             "SE.50", "Estimate.90", "SE.90"))

data("dummydata")

test_that("EC 50 results are correct", {
  skip_on_cran()
  res <- EC_table(dummydata, form = response ~ dose) # plot
  res_mod <- EC_table(dummydata, form = response ~ dose, plot = FALSE,
                      result = "model")
  res_sum <- EC_table(dummydata, form = response ~ dose, plot = FALSE,
                      result = "summary")

  expect_equivalent(res %>% data.frame, expected %>% data.frame)
  expect_that(res, is_a("data.frame"))
  expect_that(res, is_a("tbl_df"))
  expect_that(res_mod[[1]], is_a("drc"))
  expect_that(res_sum[[1]], is_a("summary.drc"))
})

test_that("EC_table needs a formula to work", {
  expect_error(EC_table(dummydata))
})

test_that("EC_table throws an error with incorrect formulae", {
  expect_error(EC_table(dummydata, wrong ~ formula))
})

test_that("EC_table can read files", {
  tmp <- tempfile()
  write.table(dummydata, file = tmp, row.names = FALSE, col.names = TRUE,
              sep = ",")
  res <- EC_table(tmp, form = response ~ dose, sep = ",")
  expect_equivalent(res %>% data.frame, expected %>% data.frame)
})

test_that("Insufficient data is handled well", {
  whadat <- dummydata %>%
    filter(ID == "Sample2") %>%
    mutate(ID = "wha", response = 0L) %>%
    bind_rows(dummydata)
  expect_output(whares <- EC_table(whadat, form = response ~ dose, response = 50, model = "LL.4"), "Not evaluated: wha")
  expect_equal(dim(whares), c(3L, 3L))
})
