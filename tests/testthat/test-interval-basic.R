library(units)

set_units(1, "m") # preload units namespace

test_that("numeric inputs require unit", {
  expect_error(units_interval(1, 2), "unit must be supplied")
  expect_silent(units_interval(1, 2, unit = "m"))
})

test_that("unit compatibility validated", {
  x <- set_units(1, "m")
  y <- set_units(1, "s")
  expect_error(units_interval(x, y), "incompatible")
})

test_that("interval arithmetic preserves units", {
  a <- units_interval(set_units(0, "m"), set_units(1, "m"))
  b <- units_interval(set_units(0, "m"), set_units(2, "m"))

  sum <- a + b
  expect_s3_class(sum, "units_interval")
  expect_equal(units::deparse_unit(sum$lower), "m")
  expect_equal(lower_bounds(sum), set_units(0, "m"))
  expect_equal(upper_bounds(sum), set_units(3, "m"))

  diff <- b - a
  expect_equal(lower_bounds(diff), set_units(-1, "m"))
  expect_equal(upper_bounds(diff), set_units(2, "m"))
})

test_that("multiplication and division track unit algebra", {
  a <- units_interval(set_units(1, "m"), set_units(2, "m"))
  b <- units_interval(set_units(2, "s"), set_units(3, "s"))

  prod <- a * b
  expect_equal(units::deparse_unit(prod$lower), "m s")

  quot <- prod / b
  expect_equal(units::deparse_unit(quot$lower), "m")
})

test_that("division by interval spanning zero fails", {
  a <- units_interval(set_units(1, "m"), set_units(2, "m"))
  b <- units_interval(set_units(-1, "m"), set_units(1, "m"))
  expect_error(a / b, "spans zero")
})

test_that("comparison operations align units", {
  a <- units_interval(set_units(0, "m"), set_units(1, "m"))
  b <- units_interval(set_units(150, "cm"), set_units(250, "cm"))
  c <- units_interval(set_units(0, "m"), set_units(1, "m"))

  expect_identical(as.logical(a < b), TRUE)
  expect_identical(as.logical(a <= b), TRUE)
  expect_identical(as.logical(b > a), TRUE)
  expect_identical(as.logical(a == c), TRUE)
  expect_identical(as.logical(a == convert_units(c, "cm")), TRUE)
})

test_that("containment works for points and intervals", {
  i <- units_interval(set_units(0, "m"), set_units(1, "m"))
  expect_true(contains(i, set_units(0.5, "m")))
  expect_false(contains(i, set_units(2, "m")))

  j <- units_interval(set_units(0.2, "m"), set_units(0.4, "m"))
  expect_true(contains(i, j))
})

test_that("intersection and union return intervals", {
  a <- units_interval(set_units(0, "m"), set_units(1, "m"))
  b <- units_interval(set_units(0.5, "m"), set_units(2, "m"))

  inter <- interval_intersection(a, b)
  expect_equal(lower_bounds(inter), set_units(0.5, "m"))
  expect_equal(upper_bounds(inter), set_units(1, "m"))

  uni <- interval_union(a, b)
  expect_equal(lower_bounds(uni), set_units(0, "m"))
  expect_equal(upper_bounds(uni), set_units(2, "m"))
})

test_that("width and midpoint behave as expected", {
  a <- units_interval(set_units(c(0, -1), "m"), set_units(c(2, 1), "m"))
  expect_equal(width(a), set_units(c(2, 2), "m"))
  expect_equal(midpoint(a), set_units(c(1, 0), "m"))
})

test_that("empty and whole interval constructors respect units", {
  empty_two <- empty_interval(unit = "m", length = 2)
  expect_equal(length(empty_two), 2L)
  expect_true(all(is_empty(empty_two)))
  expect_false(any(is_whole(empty_two)))

  base <- units_interval(0, 1, unit = "m")
  empty_like <- empty_interval(like = base)
  expect_equal(units::deparse_unit(empty_like$lower), "m")
  expect_identical(length(empty_like), length(base))

  whole <- whole_interval(like = base)
  expect_true(all(is_whole(whole)))
  expect_true(all(is_superset(whole, base)))
  expect_true(all(is_proper_superset(whole, base)))
  expect_false(any(is_proper_superset(base, whole)))
})
