context("HLA mismatch grade")
library(transplantr)

test_that("Numeric HLA mismatch grade gives correct result", {
  expect_equal(hla_mm_level(a = 0, b = 0, dr = 0), 1)
  expect_equal(hla_mm_level(a = 0, b = 1, dr = 0), 2)
  expect_equal(hla_mm_level(a = 0, b = 1, dr = 1), 3)
  expect_equal(hla_mm_level(a = 0, b = 0, dr = 2), 4)
})

test_that("String HLA mismatch grade gives correct result", {
  expect_equal(hla_mm_level_str("211", sep = FALSE), 3)
  expect_equal(hla_mm_level_str("1:0:1"), 2)
})



