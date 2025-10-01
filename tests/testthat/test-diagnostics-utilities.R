test_that("possibility and certainty helpers reflect interval relations", {
  a <- units_interval(
    units::set_units(c(0, 2), "m"),
    units::set_units(c(0.5, 4), "m")
  )
  b <- units_interval(
    units::set_units(c(1, 1), "m"),
    units::set_units(c(2, 3), "m")
  )

  expect_identical(possible(a, b, "<"), c(TRUE, TRUE))
  expect_identical(certain(a, b, "<"), c(TRUE, FALSE))
  expect_identical(verify(a, b, "<"), c(TRUE, NA))

  expect_identical(possible(a, b, "gt"), c(FALSE, TRUE))
  expect_identical(certain(a, b, ">"), c(FALSE, FALSE))
  expect_identical(verify(a, b, ">"), c(FALSE, NA))
})

test_that("verify returns NA for undecidable or invalid relations", {
  x <- units_interval(units::set_units(NA_real_, "m"), units::set_units(NA_real_, "m"))
  y <- units_interval(units::set_units(0, "m"), units::set_units(1, "m"))
  expect_true(is.na(verify(x, y, "==")))
})

test_that("contract shrinks intervals and flags over-contraction", {
  i <- units_interval(units::set_units(0, "s"), units::set_units(10, "s"))

  contracted_abs <- contract(i, absolute = units::set_units(1, "s"))
  expect_equal(lower_bounds(contracted_abs), units::set_units(1, "s"))
  expect_equal(upper_bounds(contracted_abs), units::set_units(9, "s"))

  contracted_rel <- contract(i, relative = 0.1)
  expect_equal(lower_bounds(contracted_rel), units::set_units(1, "s"))
  expect_equal(upper_bounds(contracted_rel), units::set_units(9, "s"))

  over_contracted <- contract(i, absolute = units::set_units(6, "s"))
  expect_true(all(is.na(lower_bounds(over_contracted))))
  expect_true(all(is.na(upper_bounds(over_contracted))))
})

test_that("as_numeric_bounds emits numeric matrices with optional conversion", {
  x <- units_interval(
    units::set_units(c(0, 1), "m"),
    units::set_units(c(2, 3), "m")
  )

  native <- as_numeric_bounds(x)
  expect_identical(dim(native), c(2L, 2L))
  expect_identical(colnames(native), c("lower", "upper"))
  expect_equal(native[, "lower"], c(0, 1))
  expect_equal(native[, "upper"], c(2, 3))

  converted <- as_numeric_bounds(x, unit = "cm")
  expect_equal(converted[, "lower"], c(0, 100))
  expect_equal(converted[, "upper"], c(200, 300))
})
