test_that("physics vignette core runs", {
  if (!"boostedintervals" %in% loadedNamespaces()) pkgload::load_all()
  mass <- units_interval(units::set_units(1.0, "kg"), units::set_units(1.02, "kg"))
  vel  <- units_interval(units::set_units(10.0, "m/s"), units::set_units(10.2, "m/s"))
  energy <- 0.5 * mass * vel^2
  expect_s3_class(energy, "units_interval")
  nb <- as_numeric_bounds(energy, unit = "J")
  expect_true(is.matrix(nb) || is.data.frame(nb))
  expect_equal(ncol(nb), 2L)
})
