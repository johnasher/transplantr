#' eGFR using Nankivell formula
#'
#' A vectorised function to calculate eGFR using the Nankivell formula. By default the equation accepts
#' serum creatinine in µmol/l but can be changed to mg/dl by setting the units parameter to "US".
#' The Urea parameter is serum urea in mmol/l, but if the units parameter is set to "US",
#' Blood Urea Nitrogen (BUN) in mg/dl is used instead.
#'
#' Reference: Nankivell BJ, Gruenwald SM, Allen RD, Chapman JR: Predicting glomerular filtration rate
#' after renal transplantation. Transplantation 1995; 59:1683-89.
#'
#' @param SCr numeric vector of serum creatinine in µmol/l (or mg/dl if units = "US")
#' @param Urea numeric vector of serum urea in mmol/l (or BUN in mg/dl if units = "US")
#' @param Weight numeric vector of patient weights in kilograms
#' @param Height numeric vector of patient heights in metres
#' @param Sex character vector of sex ("F" for female, "M" for male)
#' @param Units non-vectorised optional parameter for creatinine and urea/BUN units ("SI" for µmol/l (default), "US" for mg/dl)
#'
#' @return a numeric vector of eGFR values
#' @export
#'
#' @examples
nankivell = function(SCr, Urea, Weight, Height, Sex, Units = "SI") {
  # convert creatinine to mg/dl
  if (units == "SI") {
    SCr = SCr / 88.4
  }

  # conversion factor for Sex
  corr = ifelse(Sex == "M", 35, 25)

  nvlGFR = 6.7 / SCr +
          0.25 * Weight +
          -0.5 * Urea - 100 +
          -100 / Height ^ 2 +
          corr
  nvlGFR
}

#' eGFR using the Nankivell-SPK formula
#'
#' A vectorised function to calculate the eGFR using the Nankivell-SPK formula. By default the
#' equation accepts serum creatinine in µmol/l but can be changed to
#' mg/dl by setting the units parameter to "US". To allow for serial measurements over time, such as
#' for transplant follow-up data, there is an optional offset = n parameter which increases the age
#' value used in the equation by n years.
#'
#' Reference: Nankivell BJ, Chapman JR, Allen RD: Predicting glomerular filtration rate after
#' simultaneous pancreas and kidney transplantation. Clin Transplant 1995; 9(2): 129-134
#'
#' @param SCr numeric vector of serum creatinine in µmol/l (or mg/dl if units = "US")
#' @param Age numeric vector of patient ages in years
#' @param Sex character vector of sex ("F" for female, "M" for male)
#' @param Weight numeric vector of patient weights in kilograms
#' @param Height numeric vector of patient heights in metres
#' @param Units non-vectorised optional parameter for creatinine and urea/BUN units ("SI" for µmol/l (default), "US" for mg/dl)
#' @param Offset non-vectorised optional numeric parameter for offset in years
#'
#' @return numeric vector of eGFR values
#' @export
#'
#' @examples
nankivell_spk = function(SCr, Age, Sex, Weight, Height, Units = "SI", Offset = 0) {
  # convert creatinine to umol/l
  if (units == "US") {
    SCr = SCr * 88.4
  }

  # conversion factor for Sex
  corr = ifelse(Sex == "M", 71.4, 50.4)

  nvspkGFR = corr + 5520 / SCr + 0.27 * Weight -
            0.50 * Age - 0.29 * height
  nvspkGFR
}

#' eGFR using the Walser formula
#'
#' A vectorised function to calculate eGFR using the Walser formula. By default the equation accepts
#' serum creatinine in µmol/l but can be changed to mg/dl by setting the units parameter to "US".
#'
#'
#' @param SCr numeric vector of serum creatinine in µmol/l (or mg/dl if units = "US")
#' @param Age numeric vector of patient ages in years
#' @param Weight numeric vector of patient weights in kilograms
#' @param Sex character vector of sex ("F" for female, "M" for male)
#' @param Units non-vectorised optional parameter for creatinine units ("SI" for µmol/l (default), "US" for mg/dl)
#'
#' @return numeric vectors of eGFR values
#' @export
#'
#' @examples
walser = function(SCr, Age, Weight, Sex, Units = "SI") {
  # convert creatinine to mg/dl
  if (units == "SI") {
    SCr = SCr / 88.4
  }

  #correction factors
  walfage = ifelse(Sex == "M", 0.103, 0.08)
  walfem = ifelse(Sex == "M", 7.57, 6.05)
  walwt = ifelse(Sex == "M", 0.096, 0.08)
  walcorr = ifelse(Sex == "M", 6.66, 4.81)

  wlsGFR = walfem / (SCr / 10) -
            walfage * Age +
            walwt * Weight - walcorr
  wlsGFR
}
