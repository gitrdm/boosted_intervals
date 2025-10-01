test_that("finance vignette core runs", {
  if (!"boostedintervals" %in% loadedNamespaces()) pkgload::load_all()
  P <- units_interval(1000, 1000, unit = "1")
  r <- units_interval(0.03, 0.05, unit = "1")
  fv <- P * (1 + r)^5
  expect_s3_class(fv, "units_interval")
  nb <- as_numeric_bounds(fv)
  expect_equal(ncol(nb), 2L)
})
