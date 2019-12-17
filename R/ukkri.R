#' UK Kidney Recipient Risk Index (NHSBT, 2019 version)
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
#' ukkrri(age = 45, dx = 0, wait = 750, dm = 0)
ukkrri = function(age, dx, wait, dm) {
  agevar = ifelse(age <= 25, 0, 0.016 * (age - 75))
  dxvar = 0.361 * dx
  wtvar = 0.033 * (wait - 950) / 365.25
  dmvar = 0.252 * dm
  exp(agevar + dxvar + wtvar + dmvar)
}

#' UK Kidney Donor Risk Index (NHSBT, 2019 version)
#'
#' A vectorised function to calculate the UK Kidney Donor Risk Index as used in the new
#' national kidney matching scheme implemented in September 2019.
#'
#' @param age numeric vector of donor age in years
#' @param height numeric vector of donor height in cm
#' @param htn numeric vector of whether donor history of hypertension (1 = yes, 0 = no)
#' @param sex character vector of donor sex ("F" = female, "M" = male)
#' @param cmv numeric vector of whether donor CMV IgG positive (1 = yes, 0 = no)
#' @param gfr numeric vector of donor eGFR at time of donation
#' @param hdays numeric vector of number of days donor in hospital before donation
#'
#' @return numeric vector of UK Kidney Donor Risk Index values (2019 version)
#' @export
#'
#' @examples
#' ukkdri(age = 50, height = 170, htn = 1, sex = "F", cmv = 0, gfr = 90, hdays = 2)
ukkdri = function(age, height, htn, sex, cmv, gfr, hdays) {
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


#' UK Kidney Recipient Risk Index Quartile (2019)
#'
#' Vectorised function to convert UKKRRI values to quartiles of risk. The function takes a numeric
#' vector of UKKRRI values as input, and returns a vector of quartiles. By default this is also a
#' numeric vector with values 1-4, but this can be changed to a character string vector of R1-R4 to
#' match the nomenclature in the NHSBT ODT documentation by setting the prefix parameter to TRUE. The
#' output can also be as a vector of factors by setting fct to TRUE (this can be combined with the
#' prefix parameter).
#'
#' @param rri numeric vector of UKKRRI values
#' @param prefix whether to prefix results with "R" (default FALSE)
#' @param fct whether to return results as a factor (default FALSE)
#'
#' @return vector of UKKRRI quartiles
#' @export
#'
#' @examples
#' obtain quartile of a single value
#' ukkrri_q(1.01)
#'
#' # factor vector of results with prefix
#' rri = c(0.69, 0.75, 0.96, 1.36)
#' ukkrri_q(rri, prefix = TRUE, fct = TRUE)
ukkrri_q = function(rri, prefix = FALSE, fct = FALSE) {
  q = ifelse(rri < 0.74000001, 1,
             ifelse(rri < 0.94000001, 2,
                    ifelse(rri < 1.20000001, 3, 4)))
  if (prefix) q = paste0("R", q)
  if (fct) q = as.factor(q)
  q
}

#' UK Kidney Donor Risk Index Quartile (2019)
#'
#' Vectorised function to convert UKKDRI values to quartiles of risk. The function takes a numeric
#' vector of UKKDRI values as input, and returns a vector of quartiles. By default this is also a
#' numeric vector with values 1-4, but this can be changed to a character string vector of D1-D4 to
#' match the nomenclature in the NHSBT ODT documentation by setting the prefix parameter to TRUE. The
#' output can also be as a vector of factors by setting fct to TRUE (this can be combined with the
#' prefix parameter).
#'
#' @param rri numeric vector of UKKDRI values
#' @param prefix whether to prefix results with "D" (default FALSE)
#' @param fct whether to return results as a factor (default FALSE)
#'
#' @return vector of UKKDRI quartiles
#' @export
#'
#' @examples
#' obtain quartile of a single value
#' ukkdri_q(1.01)
#'
#' # factor vector of results with prefix
#' dri = c(0.69, 0.75, 0.96, 1.36)
#' ukkdri_q(rri, prefix = TRUE, fct = TRUE)
ukkdri_q = function(dri, prefix = FALSE, fct = FALSE) {
  q = ifelse(dri < 0.79000001, 1,
             ifelse(dri < 1.12000001, 2,
                    ifelse(dri < 1.50000001, 3, 4)))
  if (prefix) q = paste0("D", q)
  if (fct) q = as.factor(q)
  q
}
