test_that("nth_root handles unit exponents like m^3 -> m", {
  cube <- units_interval(
    set_units(8, "m^3"),
    set_units(27, "m^3")
  )

  root3 <- nth_root(cube, 3)
  expect_equal(units::deparse_unit(lower_bounds(root3)), "m")
  expect_equal(units::deparse_unit(upper_bounds(root3)), "m")
  expect_equal(units::drop_units(lower_bounds(root3)), 2)
  expect_equal(units::drop_units(upper_bounds(root3)), 3)
})
