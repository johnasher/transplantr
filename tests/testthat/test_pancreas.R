context("Pancreas calculators")
library(transplantr)

test_that("PDRI gives correct result", {
  expect_equal(round(pdri(age = 28, sex = "M", creat = 1.0, eth = "other", bmi = 24,
                          height = 173, cva = 0, cit = 12, dcd = 0, units = "US"), 2), 1.00)
  expect_equal(round(pdri(age = 45, sex = "M", creat = 88.4, eth = "other", bmi = 24,
                          height = 173, cva = 0, cit = 12, dcd = 0, units = "SI"), 2), 1.56)
})


