test_that("abs produces tight non-negative intervals", {
  x <- units_interval(
    set_units(c(-2, -1), "m"),
    set_units(c(-0.5, 3), "m")
  )

  res <- abs(x)

  expect_equal(
    lower_bounds(res),
    set_units(c(0.5, 0), "m")
  )
  expect_equal(
    upper_bounds(res),
    set_units(c(2, 3), "m")
  )
})

test_that("sqrt returns principal root with adjusted units", {
  squared <- units_interval(
    set_units(c(4, 9), "m^2"),
    set_units(c(9, 25), "m^2")
  )

  res <- sqrt(squared)

  expect_equal(
    lower_bounds(res),
    set_units(c(2, 3), "m")
  )
  expect_equal(
    upper_bounds(res),
    set_units(c(3, 5), "m")
  )
})

test_that("sqrt errors on negative support", {
  neg <- units_interval(
    set_units(-1, "m^2"),
    set_units(4, "m^2")
  )
  expect_error(sqrt(neg), "negative lower bound")
})

test_that("integer powers align with units", {
  length_ival <- units_interval(
    set_units(2, "m"),
    set_units(3, "m")
  )

  area <- length_ival ^ 2
  expect_equal(lower_bounds(area), set_units(4, "m^2"))
  expect_equal(upper_bounds(area), set_units(9, "m^2"))

  inverse <- length_ival ^ (-1)
  expect_equal(
    lower_bounds(inverse),
    set_units(1 / 3, "1/m")
  )
  expect_equal(
    upper_bounds(inverse),
    set_units(0.5, "1/m")
  )

  constant <- length_ival ^ 0
  expect_equal(
    units::deparse_unit(lower_bounds(constant)),
    "1"
  )
  expect_equal(
    units::drop_units(lower_bounds(constant)),
    1
  )
  expect_equal(
    units::drop_units(upper_bounds(constant)),
    1
  )
})

test_that("power edge cases report informative errors", {
  centered <- units_interval(
    set_units(-1, "m"),
    set_units(1, "m")
  )
  expect_error(centered ^ -1, "Negative exponents")
  expect_error(centered ^ 0, "0\\^0")

  expect_error(units_interval(1, 2, unit = "m") ^ 1.5, "integer exponents")
})

test_that("exp/log/log10 operate on dimensionless intervals", {
  dimless <- units_interval(0, 1, unit = "1")

  exp_res <- exp(dimless)
  expect_equal(units::deparse_unit(lower_bounds(exp_res)), "1")
  expect_equal(lower_bounds(exp_res), units::set_units(exp(0), "1"))
  expect_equal(upper_bounds(exp_res), units::set_units(exp(1), "1"))

  log_res <- log(units_interval(1, exp(1), unit = "1"))
  expect_equal(units::deparse_unit(lower_bounds(log_res)), "1")
  expect_equal(lower_bounds(log_res), units::set_units(0, "1"))
  expect_equal(upper_bounds(log_res), units::set_units(1, "1"))

  expect_equal(
    lower_bounds(log10(units_interval(1, 10, unit = "1"))),
    units::set_units(0, "1"))
  expect_equal(
    upper_bounds(log10(units_interval(1, 10, unit = "1"))),
    units::set_units(1, "1"))

  expect_error(log(units_interval(0, 2, unit = "1")), "undefined")
  expect_error(log10(units_interval(-1, 2, unit = "1")), "undefined")
})

test_that("transcendental Math functions enforce unit requirements", {
  length_interval <- units_interval(set_units(0, "m"), set_units(1, "m"))
  expect_error(exp(length_interval), "dimensionless intervals")
  expect_error(log(length_interval), "dimensionless intervals")
  expect_error(sin(length_interval), "convertible to radians")
})

test_that("sin and cos accept radians and convertible units", {
  angle_rad <- units_interval(0, pi / 6, unit = "rad")

  sin_res <- sin(angle_rad)
  expect_equal(units::deparse_unit(lower_bounds(sin_res)), "1")
  expect_equal(
    units::drop_units(lower_bounds(sin_res)),
    c(0)
  )
  expect_equal(
    units::drop_units(upper_bounds(sin_res)),
    sin(pi / 6),
    tolerance = 1e-12
  )

  cos_res <- cos(angle_rad)
  expect_equal(units::deparse_unit(lower_bounds(cos_res)), "1")
  expect_equal(
    units::drop_units(lower_bounds(cos_res)),
    cos(pi / 6),
    tolerance = 1e-12
  )
  expect_equal(
    units::drop_units(upper_bounds(cos_res)),
    1,
    tolerance = 1e-12
  )

  angle_deg <- convert_units(angle_rad, "degree")
  sin_deg <- sin(angle_deg)
  cos_deg <- cos(angle_deg)
  expect_equal(units::drop_units(lower_bounds(sin_deg)), units::drop_units(lower_bounds(sin_res)))
  expect_equal(units::drop_units(upper_bounds(sin_deg)), units::drop_units(upper_bounds(sin_res)))
  expect_equal(units::drop_units(lower_bounds(cos_deg)), units::drop_units(lower_bounds(cos_res)))
  expect_equal(units::drop_units(upper_bounds(cos_deg)), units::drop_units(upper_bounds(cos_res)))
})

test_that("tan produces bounded results away from singularities", {
  angle <- units_interval(0, pi / 4, unit = "rad")
  tan_res <- tan(angle)
  expect_equal(units::deparse_unit(lower_bounds(tan_res)), "1")
  expect_equal(units::drop_units(lower_bounds(tan_res)), 0, tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(tan_res)), tan(pi / 4), tolerance = 1e-12)
})

test_that("inverse trig functions return radians", {
  dimless <- units_interval(-0.5, 0.5, unit = "1")

  asin_res <- asin(dimless)
  expect_equal(units::deparse_unit(lower_bounds(asin_res)), "rad")
  expect_equal(units::drop_units(lower_bounds(asin_res)), asin(-0.5), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(asin_res)), asin(0.5), tolerance = 1e-12)

  acos_res <- acos(dimless)
  expect_equal(units::deparse_unit(lower_bounds(acos_res)), "rad")
  expect_equal(units::drop_units(upper_bounds(acos_res)), acos(-0.5), tolerance = 1e-12)

  atan_res <- atan(units_interval(-1, 1, unit = "1"))
  expect_equal(units::deparse_unit(lower_bounds(atan_res)), "rad")
  expect_equal(units::drop_units(lower_bounds(atan_res)), atan(-1), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(atan_res)), atan(1), tolerance = 1e-12)
})

test_that("inverse trig and hyperbolic domains enforced", {
  expect_error(asin(units_interval(-1.5, -1.2, unit = "1")), "within [-1, 1]", fixed = TRUE)
  expect_error(acos(units_interval(0.5, 1.5, unit = "1")), "within [-1, 1]", fixed = TRUE)
  expect_error(acosh(units_interval(0.5, 2, unit = "1")), "lower bound >= 1")
  expect_error(atanh(units_interval(-1, 0.5, unit = "1")), "within (-1, 1)", fixed = TRUE)
  expect_error(atanh(units_interval(-0.5, 1, unit = "1")), "within (-1, 1)", fixed = TRUE)
})

test_that("log family applies domain guards", {
  expect_error(log(units_interval(-1, 1, unit = "1")), "at or below zero", fixed = TRUE)
  expect_error(log2(units_interval(0, 1, unit = "1")), "at or below zero", fixed = TRUE)
  expect_error(log1p(units_interval(-2, -1, unit = "1")), "at or below -1", fixed = TRUE)
})

test_that("exp and log variants operate on dimensionless intervals", {
  dimless <- units_interval(-0.25, 0.5, unit = "1")

  expm1_res <- expm1(dimless)
  expect_equal(units::deparse_unit(lower_bounds(expm1_res)), "1")
  expect_equal(units::drop_units(lower_bounds(expm1_res)), exp(-0.25) - 1, tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(expm1_res)), exp(0.5) - 1, tolerance = 1e-12)

  log1p_res <- log1p(units_interval(0, 0.5, unit = "1"))
  expect_equal(units::deparse_unit(lower_bounds(log1p_res)), "1")
  expect_equal(units::drop_units(lower_bounds(log1p_res)), log1p(0), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(log1p_res)), log1p(0.5), tolerance = 1e-12)

  log2_res <- log2(units_interval(1, 8, unit = "1"))
  expect_equal(units::deparse_unit(lower_bounds(log2_res)), "1")
  expect_equal(units::drop_units(lower_bounds(log2_res)), log2(1), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(log2_res)), log2(8), tolerance = 1e-12)
})

test_that("hyperbolic functions propagate dimensionless input", {
  dimless <- units_interval(-1, 1, unit = "1")

  expect_equal(
    units::drop_units(lower_bounds(sinh(dimless))),
    sinh(-1),
    tolerance = 1e-12
  )
  expect_equal(
    units::drop_units(upper_bounds(sinh(dimless))),
    sinh(1),
    tolerance = 1e-12
  )

  cosh_res <- cosh(dimless)
  expect_equal(units::deparse_unit(lower_bounds(cosh_res)), "1")
  expect_equal(units::drop_units(lower_bounds(cosh_res)), 1, tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(cosh_res)), cosh(1), tolerance = 1e-12)

  tanh_res <- tanh(dimless)
  expect_equal(units::drop_units(lower_bounds(tanh_res)), tanh(-1), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(tanh_res)), tanh(1), tolerance = 1e-12)

  asinh_res <- asinh(dimless)
  expect_equal(units::drop_units(lower_bounds(asinh_res)), asinh(-1), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(asinh_res)), asinh(1), tolerance = 1e-12)

  atanh_res <- atanh(units_interval(-0.5, 0.5, unit = "1"))
  expect_equal(units::drop_units(lower_bounds(atanh_res)), atanh(-0.5), tolerance = 1e-12)
  expect_equal(units::drop_units(upper_bounds(atanh_res)), atanh(0.5), tolerance = 1e-12)
})
