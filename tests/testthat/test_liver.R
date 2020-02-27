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

test_that("ET-DRI gives correct result", {
  expect_equal(round(et_dri(age = 39, cod = "trauma", dcd = 0, split = 0, share = "local", cit = 8, ggt = 50, rescue = 0), 2), 1.00)
  expect_equal(round(et_dri(age = 65, cod = "trauma", dcd = 0, split = 0, share = "local", cit = 8, ggt = 50, rescue = 0), 2), 1.50)
  expect_equal(round(et_dri(age = 65, cod = "trauma", dcd = 0, split = 0, share = "local", cit = 14, ggt = 50, rescue = 0), 2), 1.59)
  expect_equal(round(et_dri(age = 25, cod = "trauma", dcd = 0, split = 0, share = "local", cit = 8, ggt = 200, rescue = 0), 2), 1.09)
  expect_equal(round(et_dri(age = 25, cod = "trauma", dcd = 0, split = 0, share = "local", cit = 12, ggt = 50, rescue = 1), 2), 1.24)
  expect_equal(round(et_dri(age = 25, cod = "trauma", dcd = 1, split = 0, share = "local", cit = 8, ggt = 50, rescue = 0), 2), 1.51)
  expect_equal(round(et_dri(age = 25, cod = "cva", dcd = 0, split = 0, share = "local", cit = 8, ggt = 50, rescue = 0), 2), 1.15)
  expect_equal(round(et_dri(age = 25, cod = "trauma", dcd = 0, split = 1, share = "regional", cit = 8, ggt = 50, rescue = 0), 2), 1.69)
})

test_that("Liver DRI gives correct result", {
  expect_equal(round(liver_dri(age = 25, cod = "trauma", eth = "white", dcd = 0, split = 0, share = "local", cit = 8, height = 170), 2), 1.00)
  expect_equal(round(liver_dri(age = 64, cod = "trauma", eth = "white", dcd = 0, split = 0, share = "local", cit = 8, height = 170), 2), 1.53)
  expect_equal(round(liver_dri(age = 64, cod = "cva", eth = "white", dcd = 0, split = 0, share = "local", cit = 8, height = 170), 2), 1.77)
  expect_equal(round(liver_dri(age = 64, cod = "cva", eth = "white", dcd = 0, split = 0, share = "local", cit = 14, height = 170), 2), 1.88)
  expect_equal(round(liver_dri(age = 25, cod = "trauma", eth = "white", dcd = 0, split = 0, share = "local", cit = 14, height = 170), 2), 1.06)
  expect_equal(round(liver_dri(age = 25, cod = "trauma", eth = "white", dcd = 1, split = 0, share = "local", cit = 14, height = 170), 2), 1.60)
})

test_that("SOFT scores give correct results", {
  expect_equal(soft(Age = 35, BMI = 20, PrevTx = 0, AbdoSurg = 1, Albumin = 30, Dx = 0, ICU = 0, Admitted = 0, MELD = 29, LifeSupport = 0, Encephalopathy = 1, PVThrombosis = 0, Ascites = 1, PortalBleed = 0, DonorAge = 44, DonorCVA = 0, DonorSCr = 110, National = 0, CIT = 8), 7)
  expect_equal(p_soft(Age = 65, BMI = 36, PrevTx = 2, AbdoSurg = 1, Albumin = 29, Dx = 0, ICU = 0, Admitted = 1, MELD = 32, LifeSupport = 0, Encephalopathy = 1, PVThrombosis = 1, Ascites = 1), 37)
  expect_equal(soft2(PSoft = 4, PortalBleed = 0, DonorAge = 61, DonorCVA = 1, DonorSCr = 140, National = 1, CIT = 12), 13)
})

test_that("Pedi-SOFT score gives correct results", {
  expect_equal(pedi_soft(CTVG = 0, Weight = 31.5, Dx = 0, LifeSupport = 0, PrevTx = 0), 0)
  expect_equal(pedi_soft(CTVG = 0, Weight = 17.6, Dx = 1, LifeSupport = 0, PrevTx = 0), 17)
  expect_equal(pedi_soft(CTVG = 1, Weight = 4.9, Dx = 0, LifeSupport = 0, PrevTx = 1), 25)
  expect_equal(pedi_soft(CTVG = 1, Weight = 5.4, Dx = 1, LifeSupport = 0, PrevTx = 0), 27)
  expect_equal(pedi_soft(CTVG = 1, Weight = 5.9, Dx = 0, LifeSupport = 1, PrevTx = 1), 52)
})
