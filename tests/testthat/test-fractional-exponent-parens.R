test_that("fractional exponents: parenthesize or use numeric literal", {
  squared <- units_interval(
    set_units(1, "m^2"),
    set_units(16, "m^2")
  )

  # parenthesized fractional exponent
  root1 <- pow_interval(squared, (1/2))
  # numeric literal equivalent
  root2 <- pow_interval(squared, 0.5)

  expect_equal(lower_bounds(root1), lower_bounds(root2))
  expect_equal(upper_bounds(root1), upper_bounds(root2))
})
