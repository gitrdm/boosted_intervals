test_that("pharmacokinetics vignette core runs", {
  if (!"boostedintervals" %in% loadedNamespaces()) pkgload::load_all()
  dose <- units_interval(units::set_units(100, "mg"), units::set_units(102, "mg"))
  V    <- units_interval(units::set_units(5, "L"), units::set_units(5.2, "L"))
  C <- dose / V
  expect_s3_class(C, "units_interval")
  nb <- as_numeric_bounds(C, unit = "mg/L")
  expect_equal(ncol(nb), 2L)
})
