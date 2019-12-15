context("Liver risk scores")
library(transplantr)

test_that("UKELD gives correct result", {
  expect_equal(round(ukeld(1.0, 212, 54, 126), 2), 63.22)
})

test_that("MELD gives correct result", {
  expect_equal(round(meld(INR = 2.0, bili = 54, creat = 170, dialysis = 0), 3), 24.798)
})

test_that("MELD-Na gives correct result", {
  expect_equal(round(meld_na(INR = 1.8, bili = 2, creat = 2, Na = 131, dialysis = 0, units = "US"), 3), 26.257)
})

test_that("PELD gives correct result", {
  expect_equal(round(peld(INR = 2, bili = 3.1, albumin = 2.5, listing_age = 2, growth_failure = 1, units = "US"), 0), 19)
})

test_that("APRI gives correct result", {
  expect_equal(round(apri(ast = 38, plt = 150, ast_uln = 40), 2), 0.63)
  expect_equal(round(apri(ast = 160, plt = 75), 2), 5.33)
})
