#' Calculate UK Kidney Recipient Risk Index (NHSBT, 2019 version)
#'
#' A vectorised function to calculate the UK Kidney Recipient Risk Index as used in the new
#' national kidney matching scheme implemented in September 2019.
#'
#' @param age numeric vector of patient ages in years
#' @param dx numeric vector of whether on dialysis at time of listing (1 = yes, 0 = no)
#' @param wait numeric vector of waiting time from start of dialysis
#' @param dm numeric vector of whether patient has diabetes (1 = yes, 0 = no)
#'
#' @return numeric vector of UK Kidney Recipient Risk Index values
#' @export
#'
#' @examples
#' ukkrri(45, 0, 750, 0)
ukkrri = function(age, dx, wait, dm) {
  agevar = ifelse(age <= 25, 0, 0.016 * (age - 75))
  dxvar = 0.361 * dx
  wtvar = 0.033 * (wait - 950) / 365.25
  dmvar = 0.252 * dm
  exp(agevar + dxvar + wtvar + dmvar)
}

#' Calculate UK Kidney Donor Risk Index (NHSBT, 2019 version)
#'
#' A vectorised function to calculate the UK Kidney Donor Risk Index as used in the new
#' national kidney matching scheme implemented in September 2019.
#'
#' @param age numeric vector of donor age in years
#' @param height numeric vector of donor height in cm
#' @param htn numeric vector of whether donor history of hypertension (1 = yes, 0 = no)
#' @param female character vector of donor sex ("F" = female, "M" = male)
#' @param cmv numeric vector of whether donor CMV IgG positive (1 = yes, 0 = no)
#' @param gfr numeric vector of donor eGFR at time of donation
#' @param hdays numeric vector of number of days donor in hospital before donation
#'
#' @return numeric vector of UK Kidney Donor Risk Index values (2019 version)
#' @export
#'
#' @examples
#' ukkdri(50, 170, 1, 1, 0, 90, 2)
ukkdri = function(age, height, htn, female, cmv, gfr, hdays) {
  agevar = 0.023 * (age - 50)
  heightvar = -0.152 * (height - 170) / 10
  femvar = ifelse(sex == "F", -0.184, 0)
  cmvvar = 0.190 * cmv
  gfrvar = -0.023 * (gfr - 90) / 10
  hdaysvar = 0.015 * hdays
  exp(agevar + heightvar + femvar + cmvvar + gfrvar + hdaysvar)
}

#' UK Kidney Donor Risk Index (2012 version)
#'
#' A vectorised function to calculate the UK Kidney Donor Risk Index as published by Watson et al.
#' in 2012. Please note that this is not the same risk index as used in the new UK kidney
#' matching scheme starting in September 2019.
#'
#' @param age numeric vector of donor ages
#' @param htn numeric vector of whether donor history of hypertension (1 = yes, 0 = no)
#' @param weight numeric vector of donor weights in kg
#' @param hdays numeric vector of donor length of hospital stay
#' @param adrenaline numeric vector of whether donor treated with adrenaline (1 = yes, 0 = no)
#'
#' @return numeric vector of UK Kidney Donor Risk Index (2012) values
#' @export
#'
#' @examples
#' watson_ukkdri(age = 40, htn = 0, weight = 75, hdays = 0, adrenaline = 0) # 1.00
watson_ukkdri = function(age, htn, weight, hdays, adrenaline) {
  agevar = ifelse(age < 40, -0.245, 0) + ifelse(age >= 60, 0.396, 0)
  htnvar = 0.265 * htn
  wtvar = 0.0253 * (weight - 75) / 10
  dvar = 0.00461 * hdays
  adrenvar = 0.0465 * adrenaline
  exp(agevar + htnvar + wtvar + dvar + adrenvar)
}
