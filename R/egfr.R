#' eGFR by CKD-EPI equation
#'
#' A vectorised function to calculate estimated glomerular filtration rate using the CKD-EPI
#' equation. By default the equation accepts serum creatinine in µmol/l but can be changed to
#' mg/dl by setting the units parameter to "US". To allow for serial measurements over time, such as
#' for transplant follow-up data, there is an optional offset = n parameter which increases the age
#' value used in the equation by n years.
#'
#' @param creat numeric vector of serum creatinine in µmol/l (or mg/dl if units = "US")
#' @param age numeric vector of age in years (accepts integers or decimals)
#' @param sex character vector of sex ("F" for female, "M" for male)
#' @param ethnicity character vector of patient ethnicity, one of "black" or "non-black"
#' @param units non-vectorised optional parameter for creatinine unit ("SI" for µmol/l (default), "US" for mg/dl)
#' @param offset non-vectorised optional numeric parameter for offset in years
#'
#' @return a numeric vector of eGFR values
#' @export
#'
#' @examples
#' ckd_epi(creat = 120, age = 45.2, sex = "M", ethnicity = "non-black")
#' ckd_epi(creat = 1.5, age = 64.3, sex = "F", ethnicity = "black", units = "US")
ckd_epi <- function(creat, age, sex, ethnicity = NULL, units = "SI", offset = 0) {

  # determine level of sexvar, alpha and kappa
  sexvar = ifelse(sex == "M", 1, 1.018)
  alpha = ifelse(sex == "M", -0.411, -0.329)
  kappa = ifelse(sex == "M", 0.9, 0.7)

  # convert SI to US units
  if (units == "SI") {
    creat = creat / 88.42
  }

  # age offset for serial measures
  if (offset > 0) age = age + offset

  gfr = 141 * ifelse(creat / kappa > 1, 1, creat / kappa)^alpha *
          ifelse(creat / kappa < 1, 1, creat / kappa)^-1.209 *
          0.993^age * sexvar

  # adjustment for black ethnicity
  gfr = ifelse(ethnicity == "black", gfr * 1.159, gfr)

  gfr
}

#' eGFR by abbreviated MDRD equation
#'
#' A vectorised function to calculate estimated glomerular filtration rate using the
#' abbreviated (four variable) MDRD equation. By default the equation accepts serum creatinine
#' in µmol/l but can be changed to mg/dl by setting the units parameter to "US".
#' To allow for serial measurements over time, such as
#' for transplant follow-up data, there is an optional offset = n parameter which increases the age
#' value used in the equation by n years.
#'
#' @param creat numeric vector of serum creatinine in µmol/l (or mg/dl if units = "US")
#' @param age numeric vector of age in years (accepts integers or decimals)
#' @param sex character vector of sex ("F" for female, "M" for male)
#' @param ethnicity character vector of patient ethnicity, one of "black" or "non-black"
#' @param units non-vectorised optional parameter for creatinine unit ("SI" for µmol/l (default), "US" for mg/dl)
#' @param offset non-vectorised optional parameter for offset in years
#'
#' @return a numeric vector of eGFR values
#' @export
#'
#' @examples
#' mdrd(creat = 120, age = 45.2, sex = "M", ethnicity = "non-black")
#' mdrd(creat = 1.5, age = 64.3, sex = "F", ethnicity = "black", units = "US")
mdrd <- function (creat, age, sex, ethnicity, units = "SI", offset = 0) {
  # determine level of sexvar
  sexvar = ifelse(sex == "M", 1, 0.742)

  # convert SI to US units
  if (units == "SI") {
    creat = creat / 88.42
  }

  # age offset for serial measures
  if (offset > 0) age = age + offset

  gfr =  186 * creat^-1.154 * age^-0.203 * sexvar

  # adjustment for black ethnicity
  gfr = ifelse(ethnicity == "black", gfr * 1.21, gfr)

  gfr
}

#' eGFR by bedside Schwartz formula
#'
#' A vectorised formula to calculate estimate glomerular filtration rate in children using the bedside Schwartz formula. By default this uses
#' serum creatinine in µmol/l but this can be changed to mg/dl by setting the optional units parameter to "US".
#'
#' @param creat numeric vector of creatinine levels in µmol/l (or mg/dl if units = "US")
#' @param height numeric vector of heights in cm
#' @param units non-vectorised optional parameter for creatinine unit ("SI" for µmol/l (default), "US" for mg/dl)
#'
#' @return numeric vector of eGFR values
#' @export
#'
#' @examples
#' # calculate using creatinine in µmol/l
#' schwartz(creat = 64, height = 101)
#'
#' # calculate using mg/dl
#' schwartz(creat = 0.7, height = 101, units = "US")
schwartz = function(creat, height, units = "SI") {
  if (units == "SI") {
    gfr = 36.2 * height / creat
  } else {
    gfr = 0.41 * height / creat
  }
  gfr
}

#' eGFR by CKD-EPI equation (US units)
#'
#' A wrapper function for the ckd_epi() vectorised function to calculate estimated glomerular
#' filtration rate using the CKD-EPI equation, using serum creatinine in mg/dl.
#' To allow for serial measurements over time, such as
#' for transplant follow-up data, there is an optional offset = n parameter which increases the age
#' value used in the equation by n years.
#'
#' @param creat numeric vector of serum creatinine in µmol/l (or mg/dl if units = "US")
#' @param age numeric vector of age in years (accepts integers or decimals)
#' @param sex character vector of sex ("F" for female, "M" for male)
#' @param ethnicity character vector of patient ethnicity, one of "black" or "non-black"
#' @param offset non-vectorised optional parameter for offset in years
#'
#' @return a numeric vector of eGFR values
#' @export
#'
#' @examples
#' ckd_epi_US(creat = 1.5, age = 64.3, sex = "F", ethnicity = "black")
ckd_epi_US <- function (creat, age, sex, ethnicity, offset = 0) {
  ckd_epi(creat, age, sex, ethnicity, units = "US", offset = offset)
}

#' eGFR by abbreviated MDRD equation (US units)
#'
#' A wrapper for the mdrd4v() vectorised function to calculate estimated glomerular filtration rate
#' using the abbreviated (four variable) MDRD equation, but using serum creatinine in mg/dl.
#' To allow for serial measurements over time, such as
#' for transplant follow-up data, there is an optional offset = n parameter which increases the age
#' value used in the equation by n years.
#'
#' @param creat numeric vector of serum creatinine in µmol/l (or mg/dl if units = "US")
#' @param age numeric vector of age in years (accepts integers or decimals)
#' @param sex character vector of sex ("F" for female, "M" for male)
#' @param ethnicity character vector of patient ethnicity, one of "black" or "non-black"
#' @param offset non-vectorised optional parameter for offset in years
#'
#' @return a numeric vector of eGFR values
#' @export
#'
#' @examples

#' mdrd_US(creat = 1.5, age = 64.3, sex = "F", ethnicity = "black")
mdrd_US <- function (creat, age, sex, ethnicity, offset = 0) {
  mdrd(creat, age, sex, ethnicity, units = "US", offset = offset)
}

#' eGFR by bedside Schwartz formula (US units)
#'
#' A wrapper function for the schwartz() vectorised formula to calculate estimate glomerular filtration rate in children
#' using the bedside Schwartz formula, using
#' serum creatinine in mg/dl. Use the schwartz() function instead for µmol/l.
#'
#' @param creat numeric vector of creatinine levels in µmol/l (or mg/dl if units = "US")
#' @param height numeric vector of heights in cm
#'
#' @return numeric vector of eGFR values
#' @export
#'
#' @examples
#' # calculate using creatinine in -mg/dl
#' schwartz_US(creat = 0.7, height = 101)
schwartz_US = function(creat, height) {
  schwartz(creat = creat, height = height, units = "US")
}

#' Creatinine clearance by Cockcroft-Gault equation
#'
#' A vectorised function to estimate creatinine clearance using the Cockcroft-Gault equation.
#' By default this uses serum creatinine in µmol/l but can be changed to mg/dl by setting the
#' units parameter to "US"
#'
#' @param creat numeric vector of creatinine levels in µmol/l (or mg/dl if units = "US")
#' @param age numeric vector of ages in years
#' @param sex character vector of sex ("F" = female, "M" = male)
#' @param weight numeric vector of weights in kilograms
#' @param units non-vectorised parameter for creatinine units ("SI" for µmol/l (default) or "US" for mg/dl)
#'
#' @return numeric vector of creatinine clearances in ml/min
#' @export
#'
#' @examples
#' # calculate creatinine clearance using creatinine in µmol/l
#' cockcroft(creat = 88.4, age = 25, sex = "F", weight = 60)
#'
#' # calculate using creatinine in mg/dl
#' cockcroft(creat = 1, age = 25, sex = "F", weight = 60, units = "US")
cockcroft = function(creat, age, sex, weight, units = "SI"){
  if (units == "SI") {
    creat = creat / 88.4
  }

  sexvar = ifelse(sex == "M", 1, 0.85)
  sexvar * (140 - age) * weight / creat / 72
}


#' Creatinine clearance by Cockcroft-Gault equation (US units)
#'
#' A wrapper function for cockcroft(), a vectorised function to estimate creatinine clearance using the Cockcroft-Gault equation,
#' but using creatinine in mg/dl
#'
#' @param creat numeric vector of creatinine levels in mg/dl
#' @param age numeric vector of ages in years
#' @param sex character vector of sex ("F" = female, "M" = male)
#' @param weight numeric vector of weights in kilograms
#'
#' @return numeric vector of creatinine clearances in ml/min
#' @export
#'
#' @examples
#' cockcroft_US(creat = 1, age = 25, sex = "F", weight = 60)
cockcroft_US = function(creat, age, sex, weight){
  cockcroft(creat = creat, age = age, sex = sex, weight = weight, units = "US")
}

#' Ideal body weight
#'
#' A vectorised function to calculate adult ideal body weight based on height and sex.
#' This function assumes ideal BMI of 21.5 for females and 23 for males.
#'
#' @param height numeric vector of heights in cm
#' @param sex character vector of sex ("F" for female or "M" for male)
#'
#' @return numeric vector of ideal body weights in kg
#' @export
#'
#' @examples
#' ibw(height = 183, sex = "M")
ibw = function(height, sex) {
  bmivar = ifelse(sex == "M", 23, 21.5)
  height = height / 100
  height ^ 2 * bmivar
}
