test_that("geoscience vignette core runs", {
  if (!"boostedintervals" %in% loadedNamespaces()) pkgload::load_all()
  site_a <- units_interval(units::set_units(120.0, "m"), units::set_units(120.5, "m"))
  site_b <- units_interval(units::set_units(150.2, "m"), units::set_units(150.8, "m"))
  dist <- abs(site_b - site_a)
  expect_s3_class(dist, "units_interval")
  expect_true(is.numeric(units::drop_units(midpoint(dist))))
})
