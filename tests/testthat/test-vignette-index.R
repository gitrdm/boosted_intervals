context("vignette index renders")

test_that("vignette index renders to HTML", {
  skip_on_cran()
  pkgload::load_all()
  out <- rmarkdown::render("vignettes/index.Rmd", output_format = "html_vignette", quiet = TRUE)
  expect_true(file.exists(out))
})
