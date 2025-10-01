test_that("mechanical vignette core runs", {
  if (!"boostedintervals" %in% loadedNamespaces()) pkgload::load_all()
  shaft <- units_interval(units::set_units(9.98, "mm"), units::set_units(9.995, "mm"))
  hole  <- units_interval(units::set_units(10.0, "mm"), units::set_units(10.02, "mm"))
  clearance <- hole - shaft
  expect_s3_class(clearance, "units_interval")
  expect_true(any(possible(clearance, units::set_units(0, "mm"), ">")))
})
