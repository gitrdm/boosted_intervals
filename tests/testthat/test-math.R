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

test_that("interval diagnostics report containment and emptiness", {
  intervals <- units_interval(c(-1, 2), c(1, 3), unit = "m")
  expect_identical(zero_in(intervals), c(TRUE, FALSE))

  empty <- interval_intersection(intervals[1], units_interval(10, 12, unit = "m"))
  expect_true(is_empty(empty))
  expect_identical(is_empty(intervals), c(FALSE, FALSE))

  outer <- units_interval(c(0, 5), c(10, 9), unit = "m")
  inner <- units_interval(c(2, 6), c(4, 7), unit = "m")
  expect_identical(is_subset(outer, inner), c(TRUE, TRUE))
  expect_identical(is_proper_subset(outer, inner), c(TRUE, TRUE))
  expect_identical(is_subset(inner, outer), c(FALSE, FALSE))
  expect_identical(is_superset(outer, inner), c(TRUE, TRUE))
  expect_identical(is_proper_superset(outer, inner), c(TRUE, TRUE))
  expect_identical(is_superset(inner, outer), c(FALSE, FALSE))

  whole <- whole_interval(like = outer)
  expect_identical(is_whole(whole), rep(TRUE, length(whole)))
  expect_false(any(is_whole(intervals)))
})

test_that("radius, mag, mig, and distance match analytic expectations", {
  x <- units_interval(-4, -1, unit = "s")
  expect_equal(radius(x), units::set_units(rep(1.5, length(x)), "s"))
  expect_equal(norm(x), units::set_units(rep(4, length(x)), "s"))
  expect_equal(mag(x), units::set_units(rep(4, length(x)), "s"))
  expect_equal(mig(x), units::set_units(rep(1, length(x)), "s"))

  spans_zero <- units_interval(-2, 3, unit = "s")
  expect_equal(norm(spans_zero), units::set_units(3, "s"))
  expect_equal(mig(spans_zero), units::set_units(0, "s"))

  a <- units_interval(0, 1, unit = "m")
  b <- units_interval(2, 4, unit = "m")
  expect_equal(distance(a, b), units::set_units(1, "m"))
  expect_equal(distance(b, a), units::set_units(1, "m"))
  expect_equal(distance(a, a), units::set_units(0, "m"))

  overlap <- units_interval(0.5, 1.5, unit = "m")
  expect_equal(distance(a, overlap), units::set_units(0, "m"))
})

test_that("median returns unit-aware representative points", {
  intervals <- units_interval(
    set_units(c(-4, 10), "m"),
    set_units(c(2, 12), "m")
  )
  med <- stats::median(intervals)
  expect_s3_class(med, "units")
  expect_equal(units::deparse_unit(med), "m")
  expect_equal(units::drop_units(med), c(-1, 11))

  empty <- empty_interval(like = intervals)
  expect_true(all(is.na(stats::median(empty))))
  expect_error(stats::median(intervals, na.rm = TRUE), "na.rm is not supported")
})

test_that("bisect splits intervals at the Boost median", {
  input <- units_interval(
    set_units(c(0, -2), "m"),
    set_units(c(4, 2), "m")
  )
  halves <- bisect(input)
  expect_named(halves, c("left", "right"))
  expect_true(all(halves$left$lower <= halves$left$upper))
  expect_true(all(halves$right$lower <= halves$right$upper))

  expect_equal(units::drop_units(lower_bounds(halves$left)), c(0, -2))
  expect_equal(units::drop_units(upper_bounds(halves$left)), c(2, 0))
  expect_equal(units::drop_units(lower_bounds(halves$right)), c(2, 0))
  expect_equal(units::drop_units(upper_bounds(halves$right)), c(4, 2))

  empty <- empty_interval(unit = "m")
  halves_empty <- bisect(empty)
  expect_true(all(is.na(lower_bounds(halves_empty$left))))
  expect_true(all(is.na(upper_bounds(halves_empty$right))))
})

test_that("inflate combines absolute and relative widening", {
  base <- units_interval(
    set_units(c(-1, 2), "m"),
    set_units(c(1, 3), "m")
  )

  inflated_abs <- inflate(base, absolute = set_units(0.5, "m"))
  expect_equal(units::drop_units(lower_bounds(inflated_abs)), c(-1.5, 1.5))
  expect_equal(units::drop_units(upper_bounds(inflated_abs)), c(1.5, 3.5))

  inflated_rel <- inflate(base, relative = 0.1)
  expected_lower <- c(-1, 2) - c(1, 3) * 0.1
  expected_upper <- c(1, 3) + c(1, 3) * 0.1
  expect_equal(units::drop_units(lower_bounds(inflated_rel)), expected_lower)
  expect_equal(units::drop_units(upper_bounds(inflated_rel)), expected_upper)

  inflated_both <- inflate(base, absolute = set_units(0.5, "m"), relative = 0.1)
  expect_equal(
    units::drop_units(lower_bounds(inflated_both)),
    c(-1.6, 1.2)
  )
  expect_equal(
    units::drop_units(upper_bounds(inflated_both)),
    c(1.6, 3.8)
  )

  expect_error(inflate(base, absolute = set_units(-0.1, "m")), "non-negative")
  expect_error(inflate(base, relative = -0.1), "non-negative")
  expect_error(inflate(base, relative = set_units(1, "m")), "dimensionless")
})

test_that("pow_interval supports fractional exponents with units", {
  squared <- units_interval(set_units(1, "m^2"), set_units(16, "m^2"))
  root <- pow_interval(squared, 0.5)
  expect_equal(units::deparse_unit(lower_bounds(root)), "m")
  expect_equal(units::drop_units(lower_bounds(root)), 1)
  expect_equal(units::drop_units(upper_bounds(root)), 4)

  expect_error(pow_interval(units_interval(-1, 1, unit = "1"), -0.25), "spanning zero")
})

test_that("pow_scalar_interval raises scalars to interval exponents", {
  exponent <- units_interval(set_units(1, "1"), set_units(2, "1"))
  res <- pow_scalar_interval(2, exponent)
  expect_equal(units::deparse_unit(lower_bounds(res)), "1")
  expect_equal(units::drop_units(lower_bounds(res)), 2^1)
  expect_equal(units::drop_units(upper_bounds(res)), 2^2)
})

test_that("nth_root returns expected enclosures", {
  cube <- units_interval(set_units(-8, "m^3"), set_units(27, "m^3"))
  root3 <- nth_root(cube, 3)
  expect_equal(units::deparse_unit(lower_bounds(root3)), "m")
  expect_equal(units::drop_units(lower_bounds(root3)), -2)
  expect_equal(units::drop_units(upper_bounds(root3)), 3)

  negatives <- units_interval(set_units(-4, "1"), set_units(-1, "1"))
  even_root <- nth_root(negatives, 2)
  expect_true(all(is.na(lower_bounds(even_root))))
})

test_that("pow1p matches pow(1 + x, p) - 1", {
  x <- units_interval(set_units(-0.1, "1"), set_units(0.1, "1"))
  res <- pow1p(x, 3)

  one <- units_interval(set_units(1, "1"), set_units(1, "1"))
  expected <- pow_interval(one + x, 3) - one

  expect_equal(lower_bounds(res), lower_bounds(expected))
  expect_equal(upper_bounds(res), upper_bounds(expected))
})

test_that("hull aggregates multiple intervals", {
  a <- units_interval(set_units(0, "m"), set_units(1, "m"))
  b <- units_interval(set_units(0.5, "m"), set_units(2, "m"))
  c <- units_interval(set_units(-1, "m"), set_units(0, "m"))

  combined <- hull(a, b, c)
  expect_equal(units::drop_units(lower_bounds(combined)), rep(-1, length(combined)))
  expect_equal(units::drop_units(upper_bounds(combined)), rep(2, length(combined)))

  empty <- empty_interval(unit = "m")
  expect_equal(hull(a, empty, na.rm = TRUE), hull(a))
  expect_true(all(is.na(lower_bounds(hull(a, empty)))))

  numeric_hull <- hull(1, units_interval(set_units(0, "1"), set_units(2, "1")), unit = "1")
  expect_equal(units::drop_units(lower_bounds(numeric_hull)), 0)
  expect_equal(units::drop_units(upper_bounds(numeric_hull)), 2)
})
