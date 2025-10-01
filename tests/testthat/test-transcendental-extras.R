test_that("exp2 computes base-2 exponentials for dimensionless intervals", {
  interval <- units_interval(-1, 2, unit = "1")
  res <- exp2(interval)

  expect_equal(units::deparse_unit(lower_bounds(res)), "1")
  expect_equal(units::drop_units(lower_bounds(res)), 0.5, tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(res)), 4, tolerance = 1e-12)
})

test_that("pow2 is an alias for exp2", {
  interval <- units_interval(0, 3, unit = "1")
  expect_equal(pow2(interval), exp2(interval))
})

test_that("exp2 rejects non-dimensionless intervals", {
  length_interval <- units_interval(set_units(1, "m"), set_units(2, "m"))
  expect_error(exp2(length_interval))
})

test_that("sqrt1pm1 matches direct interval evaluation", {
  interval <- units_interval(-0.5, 0.5, unit = "1")
  res <- sqrt1pm1(interval)

  expected_lower <- sqrt(1 - 0.5) - 1
  expected_upper <- sqrt(1 + 0.5) - 1

  expect_equal(units::drop_units(lower_bounds(res)), expected_lower, tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(res)), expected_upper, tolerance = 1e-12)
})

test_that("sqrt1pm1 errors when interval dips below -1", {
  bad <- units_interval(-1.5, -1.2, unit = "1")
  expect_error(sqrt1pm1(bad), "below -1")
})

test_that("hypot returns Euclidean norms with unit alignment", {
  leg1 <- units_interval(set_units(c(3, 6), "m"), set_units(c(4, 8), "m"))
  leg2 <- units_interval(set_units(c(4, 8), "m"), set_units(c(5, 9), "m"))
  res <- hypot(leg1, leg2)

  expect_equal(units::deparse_unit(lower_bounds(res)), "m")
  expect_equal(units::drop_units(lower_bounds(res))[1], 5, tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(res))[1], sqrt(41), tolerance = 1e-12)
  expect_equal(units::drop_units(lower_bounds(res))[2], 10, tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(res))[2], sqrt(145), tolerance = 1e-12)
})

test_that("hypot accepts numeric and recycles lengths", {
  interval <- units_interval(set_units(3, "m"), set_units(3, "m"))
  numeric_leg <- c(4, 5)
  res <- hypot(interval, numeric_leg)

  expect_equal(length(res), 2L)
  expect_equal(units::drop_units(lower_bounds(res)), c(5, sqrt(34)), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(res)), units::drop_units(lower_bounds(res)), tolerance = 1e-12)
})

test_that("hypot reduces to abs() for a single argument", {
  interval <- units_interval(set_units(-3, "m"), set_units(4, "m"))
  expect_equal(hypot(interval), abs(interval))
})

test_that("hypot errors when units are incompatible", {
  leg_m <- units_interval(set_units(3, "m"), set_units(4, "m"))
  leg_s <- units_interval(set_units(4, "s"), set_units(5, "s"))
  expect_error(hypot(leg_m, leg_s))
})

test_that("hypot handles more than two arguments", {
  a <- units_interval(set_units(1, "m"), set_units(2, "m"))
  b <- units_interval(set_units(2, "m"), set_units(3, "m"))
  c <- units_interval(set_units(3, "m"), set_units(4, "m"))
  res <- hypot(a, b, c)

  expect_equal(units::drop_units(lower_bounds(res)), sqrt(1^2 + 2^2 + 3^2), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(res)), sqrt(2^2 + 3^2 + 4^2), tolerance = 1e-12)
})
