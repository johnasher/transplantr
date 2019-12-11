#' Calculate UKELD score
#'
#' A vectorised function to calculate the UKELD score using SI units for bilirubin and creatinine.
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of bilirubin in mcmol/l
#' @param creat numeric vector of creatinine in mcmol/l
#' @param Na numeric vector of sodium in mmol/l
#' @param units Units for bilirubin and creatinine ("SI" for mcmol/l (default), "US" for mg/dl)
#'
#' @return numeric vector of UKELD scores
#' @export
#'
#' @examples
#' ukeld(INR = 1.0, bili = 212, creat = 54, Na = 126)
ukeld = function(INR, bili, creat, Na, units = "SI") {
  if (units == "US") {
    bili = bili / 17.1
    creat = creat / 88.4
  }

  5.395 * log(INR) +
    1.485 * log(creat) +
    3.13 * log(bili) -
    81.565 * log(Na) +
    435
}

#' Calculate UKELD score (US units)
#'
#' A vectorised function to calculate the UKELD score using US units for bilirubin and creatinine.
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of bilirubin in mg/dl
#' @param creat numeric vector of creatinine in mg/dl
#' @param Na numeric vector of sodium in mmol/l
#'
#' @return UKELD score
#' @export
#'
#' @examples
#' ukeld_US(INR = 2.0, bili = 1.8, creat = 170, Na = 130)
ukeld_US = function(INR, bili, creat, Na) {
  5.395 * log(INR) +
    1.485 * log(creat / 88.4) +
    3.13 * log(bili / 17.1) -
    81.565 * log(Na) +
    435
}

#' MELD score
#'
#' A vectorised function to calculate the MELD score using mcmol/l for bilirubin and creatinine.
#' The units can be changed to mg/dl by setting the optional units parameter to "US". If the
#' patient is on CVVH or has been dialysed at least twice in the same week, the dialysis argument
#' should be set to 1, which changes the creatinine level used in the formula to 4mg/dl (353mcmol/l).
#' Following UNOS guidelines, the values for INR as well bilirubin and creatinine (in mg/dl) are
#' set to a minimum value of 1 if less than 1.0
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of bilirubin (mcmol/l)
#' @param creat numeric vector of creatinine (mcmol/l)
#' @param dialysis numeric vector of whether on dialysis/CVVH (1 = yes, 2 = no)
#' @param units Units for bilirubin and creatinine ("SI" for mcmol/l (default), "US" for mg/dl)
#' @return MELD score
#' @export
#'
#' @examples
#' meld(INR = 2.0, bili = 54, creat = 170, dialysis = 0)
#' meld(INR = 2.0, bili = 3.1, creat = 1.9, dialysis = 0, units = "US")
meld = function(INR, bili, creat, dialysis, units = "SI") {
  # convert bilirubin and creatinine to mg/dl
  if (units == "SI") {
    bili = bili / 17.1
    creat = creat / 88.4
  }

  # convert lab values to 1.0 if lower than 1.0
  bili = ifelse(bili < 1, 1, bili)
  creat = ifelse(creat < 1, 1, creat)
  creat = ifelse(creat > 4, 4, creat)
  INR = ifelse(INR < 1, 1, INR)

  # convert creatinine to 4.0mg/dl if on dialysis/CVVH
  creat = ifelse(dialysis == 1, 4.0, creat)

  # calculate MELD score
  meldi = (0.957 * log(creat) + 0.378 * log(bili) + 1.12 * log(INR) + 0.643)
  meld = 10 * round(meldi, digits = 10)
  meld
}

#' MELD score (using US units)
#'
#' A wrapper for the vecotorised function meld() to calculate the MELD score, but
#' using mg/dl for bilirubin and creatinine. If the
#' patient is on CVVH or has been dialysed at least twice in the same week, the dialysis argument
#' should be set to 1, which changes the creatinine level used in the formula to 4mg/dl.
#' Following UNOS guidelines, the values for INR as well bilirubin and creatinine are
#' set to a minimum value of 1 if less than 1.0
#'
#' @param INR numeric vector ofINR
#' @param bili numeric vector of bilirubin in mg/dl
#' @param creat numeric vector of creatinine in mg/dl
#' @param dialysis numeric vector of whether on dialysis/CVVH (1 = yes, 0 = no)
#' @return MELD score
#' @export
#'
#' @examples
#' meld_US(INR = 2.0, bili = 2.3, creat = 1.9, dialysis = 1)
meld_US = function(INR, bili, creat, dialysis) {
  meld(INR, bili, creat, dialysis, units = "US")
}

#' MELD-Na score
#'
#' A vectorised function to calculate the MELD-Na score, a variant of the MELD score incorporating
#' serum sodium levels. By default, bilirubin and creatinine are in mcmol/l but this can be
#' changed to mg/dl by setting the optional units parameter to "US".
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of bilirubin (mcmol/l)
#' @param creat numeric vector of creatinine (mcmol/l)
#' @param Na numeric vector of sodium (mmol/l)
#' @param dialysis whether on dialysis/CVVH (1 = yes, 0 = no)
#' @param units Units for bilirubin and creatinine ("SI" for mcmol/l (default), "US" for mg/dl)
#'
#' @return numeric vector of MELD-Na scores
#' @export
#'
#' @examples
#' meld_na(INR = 1.8, bili = 34, creat = 176, Na = 131, dialysis = 0)
#' meld_na(INR = 1.8, bili = 2, creat = 2, Na = 131, dialysis = 0, units = "US")
meld_na = function(INR, bili, creat, Na, dialysis, units = "SI") {
  # convert to US units
  if (units == "SI") {
    bili = bili / 17.1
    creat = creat / 88.4
  }

  # convert lab values to 1.0 if lower than 1.0
  bili = ifelse(bili < 1, 1, bili)
  creat = ifelse(creat < 1, 1, creat)
  INR = ifelse(INR < 1, 1, INR)

  # convert creatinine to 4.0mg/dl if on dialysis/CVVH
  creat = ifelse(dialysis == 1, 4.0, creat)

  # calculate MELD score
  meldscore = 10 * round((0.957 * log(creat) + 0.378 * log(bili) + 1.12 * log(INR) + 0.643),
                         digits = 10)

  # convert to MELD-Na
  meldna0 = meldscore - Na - (0.025 * meldscore * (140 - Na)) + 140
  meldna = ifelse(meldscore > 11, meldna0, meldscore)
  meldna
}

#' MELD-Na score (US units)
#'
#' A wrapper for meld_na(), a vectorised function to calculate the MELD-Na score,
#' a variant of the MELD score incorporating
#' serum sodium levels. In this wrapper function, bilirubin and creatinine are in mg/dl.
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of serum bilirubin (mg/dl)
#' @param creat numeric vector of serum creatinine (mg/dl)
#' @param Na numeric vector of serum sodium (mmol/l)
#' @param dialysis whether on dialysis/CVVH (1 = yes, 0 = no)
#'
#' @return numeric vector of MELD-Na scores
#' @export
#'
#' @examples
#' meld_na_US(INR = 1.8, bili = 2, creat = 2, Na = 131, dialysis = 0)
meld_na_US = function(INR, bili, creat, Na, dialysis) {
  meld_na(INR, bili, creat, Na, dialysis, units = "US")
}

#' PELD score
#'
#' A vectorised function to generate a PELD score for paediatric liver transplant candidates.
#' The default unit for bilirubin is mcmol/l but this can be changed to mg/dl by setting the
#' optional units parameter to "US".
#'
#' @param INR INR
#' @param bili serum biliruin (mcmol/l)
#' @param albumin serum albumin
#' @param listing_age age at the time of listing (years; integer or decimal)
#' @param growth_failure whether there is growth failure (1 = yes, 0 = no)
#' @param units units used for bilirubin ("SI" for mcmol/l (default), "US" for mg/dl)
#'
#' @return numeric vector of PELD scores
#' @export
#'
#' @examples
#' peld(INR = 2, bili = 54, albumin = 25, listing_age = 2, growth_failure = 1)
#' peld(INR = 2, bili = 3.1, albumin = 25, listing_age = 2, growth_failure = 1, units = "US")
peld = function(INR, bili, albumin, listing_age, growth_failure, units = "SI") {
  # convert bilirubin to US units
  if (units == "SI") {
    bili = bili / 17.1
  }

  # calculate PELD score
  peld1 = 4.80 * log(bili) + 18.57 * log(INR) - 6.87 * log(albumin)
  peld2 = ifelse(listing_age < 1, 4.36, 0)
  peld3 = ifelse(growth_failure, 6.67, 0)
  peld1 + peld2 + peld3
}

#' PELD score (US units)
#'
#' A wrapper for peld(), a vectorised function to generate a PELD score for paediatric liver
#' transplant candidates, using  mg/dl as the unit for the serum bilirubin level.
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of serum biliruin (mg/dl)
#' @param albumin numeric vector of serum albumin
#' @param listing_age numeric vector of age at the time of listing (years)
#' @param growth_failure numeric vector of whether there is growth failure (1 = yes, 0 = no)
#'
#' @return numeric vector of PELD scores
#' @export
#'
#' @examples
#' peld_US(INR = 2, bili = 3.1, albumin = 25, listing_age = 2, growth_failure = 1)
peld_US = function(INR, bili, albumin, listing_age, growth_failure) {
  peld(INR, bili, albumin, listing_age, growth_failure, units = "US")
}
