context("eGFR calculators")
library(transplantr)

test_that("CKD-EPI gives correct result", {
  expect_equal(round(ckd_epi(creat = 120, age = 45.2, sex = "M", ethnicity = "non-black"), 1), 62.5)
})

test_that("MDRD gives correct result", {
  expect_equal(round(mdrd(creat = 120, age = 45.2, sex = "M", ethnicity = "non-black"), 1), 60.3)
})

test_that("Schwartz gives correct result", {
  expect_equal(round(schwartz_US(creat = 0.6, height = 101), 1), 69.5)
})

test_that("Cockcroft-Gault gives correct result", {
  expect_equal(round(cockcroft(creat = 88.4, age = 25, sex = "F", weight = 60), 2), 81.46)
})

