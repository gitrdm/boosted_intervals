test_that("successor and predecessor operate on numerics", {
  x <- c(0, 1, -1)
  up <- successor(x)
  down <- predecessor(up)
  expect_false(any(up == x))
  expect_identical(down, x)

  double_step <- successor(x, steps = 2L)
  expect_true(all(double_step > up | is.na(double_step)))
})

test_that("next_value and prior_value advance numerics predictably", {
  x <- c(0, 1, -1, NA_real_)
  next_vals <- next_value(x)
  expect_true(all(next_vals[!is.na(x)] > x[!is.na(x)]))
  expect_true(all(is.na(next_vals[is.na(x)])))

  back <- prior_value(next_vals)
  expect_identical(back, x)

  double_step <- next_value(x, steps = 2L)
  expect_true(all(double_step[!is.na(x)] > next_vals[!is.na(x)]))
})

test_that("successor and predecessor respect units", {
  x <- units::set_units(c(1, 2), "m")
  next_vals <- successor(x)
  expect_s3_class(next_vals, "units")
  expect_identical(units(next_vals), units(x))
  expect_true(all(next_vals > x))

  prev_vals <- predecessor(next_vals)
  expect_equal(prev_vals, x)
})

test_that("next_value and prior_value preserve units", {
  x <- units::set_units(c(1, 2), "m")
  next_vals <- next_value(x)
  expect_s3_class(next_vals, "units")
  expect_identical(units(next_vals), units(x))
  expect_true(all(next_vals > x))

  back <- prior_value(next_vals)
  expect_equal(back, x)

  expect_error(next_value(units_interval(x, x)), "next_value\\(\\) expects numeric or units input")
  expect_error(prior_value(units_interval(x, x)), "prior_value\\(\\) expects numeric or units input")
})

test_that("interval successor and predecessor adjust single bounds", {
  interval <- units_interval(units::set_units(1, "m"), units::set_units(2, "m"))
  next_interval_only <- successor(interval)
  expect_equal(lower_bounds(next_interval_only), lower_bounds(interval))
  expect_true(all(upper_bounds(next_interval_only) > upper_bounds(interval)))

  prev_interval_only <- predecessor(next_interval_only)
  expect_equal(prev_interval_only, interval)
})

test_that("directed interval helpers move both bounds", {
  interval <- units_interval(units::set_units(1, "m"), units::set_units(2, "m"))
  next_all <- next_interval(interval)
  prior_all <- prior_interval(interval)

  expect_true(all(upper_bounds(next_all) > upper_bounds(interval)))
  expect_true(all(lower_bounds(next_all) > lower_bounds(interval)))

  expect_true(all(upper_bounds(prior_all) < upper_bounds(interval)))
  expect_true(all(lower_bounds(prior_all) < lower_bounds(interval)))

  expect_equal(prior_interval(next_all), interval)
  expect_equal(next_interval(prior_all), interval)
})

test_that("round_inward shrinks intervals and marks invalid cases", {
  interval <- units_interval(units::set_units(c(1, 1.5), "s"), units::set_units(c(2, 1.5), "s"))
  inward <- round_inward(interval)
  inward_lower <- lower_bounds(inward)
  inward_upper <- upper_bounds(inward)
  expect_true(all(is.na(inward_lower) | inward_lower >= lower_bounds(interval)))
  expect_true(all(is.na(inward_upper) | inward_upper <= upper_bounds(interval)))

  point <- units_interval(units::set_units(1, "s"), units::set_units(1, "s"))
  collapsed <- round_inward(point)
  expect_true(all(is.na(lower_bounds(collapsed))))
  expect_true(all(is.na(upper_bounds(collapsed))))
})

test_that("round_outward expands intervals", {
  interval <- units_interval(units::set_units(1, "m"), units::set_units(2, "m"))
  outward <- round_outward(interval)
  expect_true(all(lower_bounds(outward) <= lower_bounds(interval)))
  expect_true(all(upper_bounds(outward) >= upper_bounds(interval)))

  same <- round_outward(interval, steps = 0L)
  expect_equal(same, interval)
})

test_that("median_rounding matches median method", {
  interval <- units_interval(units::set_units(c(1, 5), "m"), units::set_units(c(2, 7), "m"))
  expect_identical(median_rounding(interval), median(interval))
})

test_that("rounding mode helpers manage state", {
  original <- get_rounding_mode()
  expect_true(original %in% rounding_modes())

  on.exit(set_rounding_mode(original), add = TRUE)

  previous <- set_rounding_mode("upward")
  expect_identical(previous, original)
  expect_identical(get_rounding_mode(), "upward")

  result <- with_rounding_mode("downward", {
    expect_identical(get_rounding_mode(), "downward")
    42
  })
  expect_identical(result, 42)
  expect_identical(get_rounding_mode(), "upward")
})

test_that("checking mode helpers manage state", {
  original <- get_checking_mode()
  expect_true(original %in% checking_modes())

  on.exit(set_checking_mode(original), add = TRUE)

  previous <- set_checking_mode("warning")
  expect_identical(previous, original)
  expect_identical(get_checking_mode(), "warning")

  result <- with_checking_mode("permissive", {
    expect_identical(get_checking_mode(), "permissive")
    99
  })
  expect_identical(result, 99)
  expect_identical(get_checking_mode(), "warning")
})

test_that("checking modes influence domain handling", {
  numerator <- units_interval(set_units(1, "m"), set_units(2, "m"))
  denominator <- units_interval(set_units(-1, "m"), set_units(1, "m"))

  expect_error(with_checking_mode("strict", numerator / denominator))

  expect_warning(result_warning <- with_checking_mode("warning", numerator / denominator))
  expect_true(all(is.na(lower_bounds(result_warning))))
  expect_true(all(is.na(upper_bounds(result_warning))))

  expect_warning(result_permissive <- with_checking_mode("permissive", numerator / denominator), NA)
  expect_true(all(is.na(lower_bounds(result_permissive))))
  expect_true(all(is.na(upper_bounds(result_permissive))))
})
