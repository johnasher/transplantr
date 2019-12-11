context("Unit converters")
library(transplantr)

test_that("Bilirubin calcs give correct results", {
  expect_equal(round(bilirubin_to_US(18), 1), 1.1)
  expect_equal(round(bilirubin_to_SI(5.4), 0), 92)
})

test_that("Creatinine calcs give correct results", {
  expect_equal(round(creatinine_to_US(270), 1), 3.1)
  expect_equal(round(creatinine_to_SI(3.5), 0), 309)
})

test_that("Urea/BUN calcs give correct results", {
  expect_equal(round(urea_to_BUN(3.2), 1), 9.0)
  expect_equal(round(BUN_to_urea(12), 1), 4.3)
})

