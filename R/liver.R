#' UKELD score
#'
#' A vectorised function to calculate the UKELD score using SI units for bilirubin and creatinine.
#'
#' Reference: Barber KM, Madden S, Allen J, et al. Elective liver transplant list mortality:
#' development of a United Kingdom end-stage liver disease score. Transplantation
#' 2011; 92(4):469-76.
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of bilirubin in µmol/l
#' @param creat numeric vector of creatinine in µmol/l
#' @param Na numeric vector of sodium in mmol/l
#' @param units Units for bilirubin and creatinine ("SI" for µmol/l (default), "US" for mg/dl)
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

#' UKELD score (US units)
#'
#' A vectorised function to calculate the UKELD score using US units for bilirubin and creatinine.
#'
#' Reference: Barber KM, Madden S, Allen J, et al. Elective liver transplant list mortality:
#' development of a United Kingdom end-stage liver disease score. Transplantation
#' 2011; 92(4):469-76.
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
#' A vectorised function to calculate the MELD score using µmol/l for bilirubin and creatinine.
#' The units can be changed to mg/dl by setting the optional units parameter to "US". If the
#' patient is on CVVH or has been dialysed at least twice in the same week, the dialysis argument
#' should be set to 1, which changes the creatinine level used in the formula to 4mg/dl (353µmol/l).
#' Following UNOS guidelines, the values for INR as well bilirubin and creatinine (in mg/dl) are
#' set to a minimum value of 1 if less than 1.0
#'
#' Reference: Kamath PS, Wiesner RH, Malinchoc M, et al. A model to predict survival in patients
#' with end-stage liver disease. Hepatology 2001; 33:464-470.
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of bilirubin (µmol/l)
#' @param creat numeric vector of creatinine (µmol/l)
#' @param dialysis numeric vector of whether on dialysis/CVVH (1 = yes, 2 = no)
#' @param units Units for bilirubin and creatinine ("SI" for µmol/l (default), "US" for mg/dl)
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
#' Reference: Kamath PS, Wiesner RH, Malinchoc M, et al. A model to predict survival in patients
#' with end-stage liver disease. Hepatology 2001; 33:464-470.
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
#' serum sodium levels. By default, bilirubin and creatinine are in µmol/l but this can be
#' changed to mg/dl by setting the optional units parameter to "US".
#'
#' Reference: Biggins SW, Kim WR, Terrault NA, et al. Evidence-based incorporation of serum sodium
#' concentration into MELD. Gastroenterology. 2006; 130(6):1652-60.
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of bilirubin (µmol/l)
#' @param creat numeric vector of creatinine (µmol/l)
#' @param Na numeric vector of sodium (mmol/l)
#' @param dialysis whether on dialysis/CVVH (1 = yes, 0 = no)
#' @param units Units for bilirubin and creatinine ("SI" for µmol/l (default), "US" for mg/dl)
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
#' Reference: Biggins SW, Kim WR, Terrault NA, et al. Evidence-based incorporation of serum sodium
#' concentration into MELD. Gastroenterology. 2006; 130(6):1652-60.
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
#' The default unit for bilirubin is µmol/l and albumin in g/l, but these can be changed to
#' mg/dl and g/dl respectively by setting the
#' optional units parameter to "US".
#'
#' Some labs report albumin in g/dl rather than the g/l used in this function.
#' If units are set to "US" then g/dl is assumed and albumin should be divided by 10 if the
#' lab reports are in g/l. If using SI units,
#' take care to
#' multiply the lab albumin by 10 if the lab output is in g/dl.
#'
#' Reference: McDiarmid SV, Anand R, Lindblad AS, et. al. Development of a pediatric end-stage
#' liver disease score to predict poor outcome in children awaiting liver transplantation.
#' Transplantation 2002; 74(2):173-81.
#'
#' @param INR INR
#' @param bili serum biliruin (µmol/l)
#' @param albumin serum albumin (g/l)
#' @param listing_age age at the time of listing (years; integer or decimal)
#' @param growth_failure whether there is growth failure (1 = yes, 0 = no)
#' @param units units used for bilirubin ("SI" for µmol/l (default), "US" for mg/dl)
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
    albumin = albumin / 10
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
#' Reference: McDiarmid SV, Anand R, Lindblad AS, et. al. Development of a pediatric end-stage
#' liver disease score to predict poor outcome in children awaiting liver transplantation.
#' Transplantation 2002; 74(2):173-81.
#'
#' @param INR numeric vector of INR
#' @param bili numeric vector of serum biliruin (mg/dl)
#' @param albumin numeric vector of serum albumin (g/dl)
#' @param listing_age numeric vector of age at the time of listing (years)
#' @param growth_failure numeric vector of whether there is growth failure (1 = yes, 0 = no)
#'
#' @return numeric vector of PELD scores
#' @export
#'
#' @examples
#' peld_US(INR = 2, bili = 3.1, albumin = 2.5, listing_age = 2, growth_failure = 1)
peld_US = function(INR, bili, albumin, listing_age, growth_failure) {
  peld(INR, bili, albumin, listing_age, growth_failure, units = "US")
}

#' AST to Platelet Ratio (APRI)
#'
#' A vectorised function to calculate the APRI score, a predictor of hepatic fibrosis.
#'
#' Reference: Lin ZH, Xin YN, Dong QJ, et al. Performance of the aspartate aminotransferase-to-platelet
#' ratio index for the staging of hepatitis C-related fibrosis: an updated meta-analysis.
#' Hepatology 2011; 53:726-736.
#'
#' @param ast numeric vector of serum AST levels in IU/l
#' @param plt numeric vector of platelet counts (10^9/l)
#' @param ast_uln single number value for lab upper limit of normal for AST levels (default is 40)
#'
#' @return numeric vector of APRI scores
#' @export
#'
#' @examples
#' apri(ast = 38, plt = 150, ast_uln = 40)
#'
#' # if the lab upper limit of normal is 40, ast_uln can be omitted
#' apri(ast = 160, plt = 75)
apri = function(ast, plt, ast_uln = 40) {
  ast / ast_uln / plt * 100
}
