test_that("even root of strictly negative intervals yields NA bounds", {
  negatives <- units_interval(
    set_units(-8, "1"),
    set_units(-1, "1")
  )

  even_root <- nth_root(negatives, 2)
  expect_true(all(is.na(lower_bounds(even_root))))
  expect_true(all(is.na(upper_bounds(even_root))))
})
