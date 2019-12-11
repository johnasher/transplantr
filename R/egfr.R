#' eGFR by CKD-EPI equation
#'
#' A vectorised function to calculate estimated glomerular filtration rate using the CKD-EPI
#' equation. By default the equation accepts serum creatinine in mcmol/l but can be changed to
#' mg/dl by setting the units parameter to "US". To allow for serial measurements over time, such as
#' for transplant follow-up data, there is an optional offset = n parameter which increases the age
#' value used in the equation by n years.
#'
#' @param creat numeric vector of serum creatinine in mcmol/l (or mg/dl if units = "US")
#' @param age numeric vector of age in years (accepts integers or decimals)
#' @param sex character vector of sex ("F" for female, "M" for male)
#' @param ethnicity character vector of patient ethnicity, one of "black" or "non-black"
#' @param units non-vectorised optional parameter for creatinine unit ("SI" for mcmol/l (default), "US" for mg/dl)
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
#' in mcmol/l but can be changed to mg/dl by setting the units parameter to "US".
#' To allow for serial measurements over time, such as
#' for transplant follow-up data, there is an optional offset = n parameter which increases the age
#' value used in the equation by n years.
#'
#' @param creat numeric vector of serum creatinine in mcmol/l (or mg/dl if units = "US")
#' @param age numeric vector of age in years (accepts integers or decimals)
#' @param sex character vector of sex ("F" for female, "M" for male)
#' @param ethnicity character vector of patient ethnicity, one of "black" or "non-black"
#' @param units non-vectorised optional parameter for creatinine unit ("SI" for mcmol/l (default), "US" for mg/dl)
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

#' eGFR by CKD-EPI equation (US units)
#'
#' A wrapper function for the ckd_epi() vectorised function to calculate estimated glomerular
#' filtration rate using the CKD-EPI equation, using serum creatinine in mg/dl.
#' To allow for serial measurements over time, such as
#' for transplant follow-up data, there is an optional offset = n parameter which increases the age
#' value used in the equation by n years.
#'
#' @param creat numeric vector of serum creatinine in mcmol/l (or mg/dl if units = "US")
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
#' @param creat numeric vector of serum creatinine in mcmol/l (or mg/dl if units = "US")
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
