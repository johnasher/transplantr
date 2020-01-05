context("Kidney risk indices")
library(transplantr)

test_that("UKKRRI gives correct result", {
  expect_equal(round(ukkrri(45, 0, 750, 0), 2), 0.61)
  expect_equal(round(ukkrri(55, 1, 960, 1), 2), 1.34)
})

test_that("UKKDRI gives correct result", {
  expect_equal(round(ukkdri(age = 50, height = 170, htn = 1, sex = "F", cmv = 0, gfr = 90, hdays = 2), 2), 0.86)
  expect_equal(round(ukkdri(age = 55, height = 170, htn = 0, sex = "M", cmv = 0, gfr = 90, hdays = 0), 2), 1.12)

})

test_that("Watson UKKDRI is correct", {
  expect_equal(round(watson_ukkdri(40, 0, 75, 0, 0), 2), 1.00)
})

test_that("UKKRRI and UKKDRI quartiles correct", {
  expect_equal(ukkrri_q(0.74), 1)
  expect_equal(ukkrri_q(1.01, prefix = TRUE), "R3")
  expect_equal(ukkdri_q(1.01), 2)
  expect_equal(ukkdri_q(1.36, prefix = TRUE), "D3")
})

test_that("USKDRI is correct", {
  expect_equal(round(uskdri(age = 52, height = 183, weight = 81, eth = "non-black", htn = 1, dm = 0,
                            cva = 1, creat = 1.7, hcv = 0, dcd = 1, scaling = 1.250609, units = "US"), 2), 1.42)
  expect_equal(round(uskdri(age = 40, height = 170, weight = 80, eth = "non-black", htn = 0, dm = 0,
                            cva = 0, creat = 120, hcv = 0, dcd = 0, scaling = 1.250609, units = "SI"), 2), 0.87)
})

test_that("EPTS raw is correct", {
  expect_equal(round(raw_epts(age = 23.5838467, dm = 0, prev_tx = 1, dx = 5.0814511), 3), 0.967)
  expect_equal(round(raw_epts(age = 52.8788501, dm = 0, prev_tx = 0, dx = 0), 3), 1.440)
  expect_equal(round(raw_epts(age = 22.5242984, dm = 1, prev_tx = 1, dx = 6.8747433), 3), 1.869)
})

test_that("EPTS percentile is correct", {
  expect_equal(epts(age = 23.5838467, dm = 0, prev_tx = 1, dx = 5.0814511), 9)
  expect_equal(epts(age = 52.8788501, dm = 0, prev_tx = 0, dx = 0), 18)
  expect_equal(epts(age = 22.5242984, dm = 1, prev_tx = 1, dx = 6.8747433), 33)
})
