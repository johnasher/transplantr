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

test_that("Nankivell gives correct result", {
  expect_equal(round(nankivell(SCr = 118, Urea = 13.4, Weight = 65, Height = 172, Sex = "M"), 1), 44.5)
  expect_equal(round(nankivell(SCr = 1.33, Urea = 38, Weight = 65, Height = 172, Sex = "M", Units = "US"), 1), 44.5)
  expect_equal(round(nankivell_US(SCr = 1.33, Urea = 38, Weight = 65, Height = 172, Sex = "M"), 1), 44.5)

})

test_that("Nankivell-SPK gives correct result", {
  expect_equal(round(nankivell_spk(SCr = 118, Age = 74, Sex = "M", Weight = 65, Height = 172), 1), 48.8)
  expect_equal(round(nankivell_spk(SCr = 1.33, Age = 74, Sex = "M", Weight = 65, Height = 172, Units = "US"), 1), 49.0)
  expect_equal(round(nankivell_spk_US(SCr = 1.33, Age = 74, Sex = "M", Weight = 65, Height = 172), 1), 49.0)
})

test_that("Walser gives correct result", {
  expect_equal(round(walser(SCr = 118, Age = 74, Weight = 65, Sex = "M"), 1), 56.1)
  expect_equal(round(walser(SCr = 1.33, Age = 74, Weight = 65, Sex = "M", Units = "US"), 1), 56.3)
  expect_equal(round(walser_US(SCr = 1.33, Age = 74, Weight = 65, Sex = "M"), 1), 56.3)
})
