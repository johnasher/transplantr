#' Pancreas donor risk index
#'
#' A vectorised function to calculate the Pancreas Donor Risk Index as published by Axelrod et al.
#' By default, the serum creatinine is used in mcmmol/l but this can be changed to mg/dl by setting
#' the optional units parameter to "US"
#'
#' @param age numeric vector of ages in years
#' @param sex character vector of patient sex ("F" for female, "M" for male)
#' @param creat numeric vector of serum creatinine (mcmol/l by default)
#' @param eth character vector of ethnicity, one of "asian", "black" or "other" in each case
#' @param bmi numeric vector of body mass index (BMI)
#' @param height numeric vector of heights in centimetres
#' @param cva numeric vector of whether CVA is cause of death (1 = yes, 0 = no)
#' @param cit numeric vector of cold ischaemic times in hours
#' @param dcd numeric vector of whether donor after circulatory death (1 = DCD, 0 = DBD)
#' @param intent character vector of implant intent, "PAK" for pancreas after kidney or "other"
#' @param units single character string for creatinine units: one of "SI" (for mcmol/l) or "US" (for mg/dl)
#'
#' @return numeric vector of pancreas donor risk index values
#' @export
#'
#' @examples
#' pdri(age = 28, sex = "M", creat = 1.0, eth = "other", bmi = 24,
#' height = 173, cva = 0, cit = 12, dcd = 0, units = "US") # 1.00
#'
#' pdri(age = 45, sex = "M", creat = 88.4, eth = "other", bmi = 24,
#' height = 173, cva = 0, cit = 12, dcd = 0, units = "SI") # 1.56
pdri = function(age, sex, creat, eth, bmi, height, cva, cit, dcd, intent = c("SPK", "PAK", "Other"), units = "SI") {
  # convert creatinine mcmol/l to mg/dl
  if (units == "SI") {
    creat = creat / 88.4
  }

  sexvar = ifelse(sex == "F", -0.13792, 0)
  agevar = ifelse(age < 20, 0.034455 * (age - 20), 0.026149 * (age - 28))
  creatvar = ifelse(creat > 2.5, 0.19490, 0)
  ethvar = ifelse(eth == "black", 0.23951, ifelse(eth == "asian", 0.15711, 0))
  bmivar0 = -0.000986347 * (bmi - 24)
  bmivar = ifelse(bmi > 25, bmivar0 + 0.033274 * (bmi - 25), bmivar0)
  heightvar = -0.006073879 * (height - 173)
  cvavar = ifelse(cva == 1, 0.21018, 0)

  # might need to replace this with a try/except block to allow for mixed vectorised/non-vectorised use
  cvavar2 = ifelse(cva == 1 && intent == "PAK", -0.28137, 0)

  citvar = 0.014678 * (cit - 12)
  dcdvar = ifelse(dcd == 1, 0.33172, 0)

  exp(sexvar + agevar + creatvar + ethvar + bmivar + heightvar + cvavar + cvavar2 + citvar + dcdvar)
}

#' P-PASS pre-procurement pancreas suitability score
#'
#' A vectorised function to calculate the P-PASS pre-procurement pancreas allocation suitability
#' score used in the Eurotransplant area. The score are between 9 and 27, and in a study published
#' by Vinkers et al. in 2008, pancreata with P-PASS score less than 17 were three times more likely
#' to be transplanted than those with scores of 17 or more.
#'
#' At least one of amylase or lipase is needed for each case, but this function can take datasets
#' with a mixture of amylase and lipase levels and will allocate points based on the higher points
#' for cases when both are provided.
#'
#' @param age numeric vector of donor ages in years
#' @param bmi numeric vector of donor body mass index (BMI)
#' @param icu numeric vector of length of donor ICU stay in days
#' @param c.arr numeric vector for duration of cardiac arrest (use 0 if no cardiac arrest)
#' @param Na numeric vector of donor serum sodium in mmmol/l
#' @param amylase numeric vector of donor serum amylase in IU/l (0 if not available)
#' @param lipase numeric vector of donor serum lipase in IU/l (0 if not available)
#' @param norad numeric vector of noradrenaline (0 if not used)
#' @param dopam numeric vector of dopamine or dobutamine (0 if not used)
#'
#' @return numeric vector of P-PASS scores
#' @export
#'
#' @examples
#' # as a single case
#' p_pass(age = 25, bmi = 19, icu = 0, c.arr = 0, Na = 135,
#'       amylase = 101, lipase = 120, norad = 0, dopam = 0) # 9
#'
#' # as a vector with mixed amylase and lipase availability
#' p_pass(age = c(25, 31, 45), bmi = c(18, 22, 35), icu = c(2, 5, 10), c.arr = c(0, 4, 10),
#'        Na = c(135, 157, 164), amylase = c(120, NA, 400), lipase = c(155, 170, NA),
#'       norad = c(0, 0.02, 0.06), dopam = c(0, 5, 11)) # 9, 19, 25
#'
#' # as a vector with all lipase values missing
#' p_pass(age = c(25, 31, 45), bmi = c(18, 22, 35), icu = c(2, 5, 10), c.arr = c(0, 4, 10),
#'        Na = c(135, 157, 164), amylase = c(120, 145, 400),
#'        norad = c(0, 0.02, 0.06), dopam = c(0, 5, 11)) # 9, 19, 25
p_pass = function(age, bmi, icu, c.arr, Na, amylase = NULL, lipase = NULL, norad, dopam) {
   age_points = ifelse(age < 30, 2, ifelse(age < 40, 4, 6))
   bmi_points = ifelse(bmi < 20, 2, ifelse(age < 25, 4, 6))
   icu_points = ifelse(icu < 3, 1, ifelse(icu < 7, 2, 3))
   c.arr_points = ifelse(c.arr == 0, 1, ifelse(c.arr < 5, 2, 3))
   Na_points = ifelse(Na < 155, 1, ifelse(Na < 160, 2, 3))
   amylase_points = ifelse(!is.na(amylase),
                           ifelse(amylase < 130 , 1, ifelse(amylase < 390 , 2, 3)), 1)
   lipase_points = ifelse(!is.na(lipase),
                          ifelse(lipase < 160 , 1, ifelse(lipase < 480 , 2, 3)), 1)
   norad_points = ifelse(norad == 0, 1, ifelse(norad < 0.05 , 2, 3))
   dopam_points = ifelse(dopam == 0, 1, ifelse(dopam < 10 , 2, 3))

   enzyme_points = ifelse(amylase_points > lipase_points, amylase_points, lipase_points)
   pressor_points = ifelse(norad_points > dopam_points, norad_points, dopam_points)

   age_points + bmi_points + icu_points + c.arr_points + Na_points + enzyme_points + pressor_points
}

# some test cases
#
# p_pass(age = 25, bmi = 19, icu = 0, c.arr = 0, Na = 135,
#        amylase = 101, lipase = 120, norad = 0, dopam = 0) # 9
#
#
# p_pass(age = c(25, 31, 45), bmi = c(18, 22, 35), icu = c(2, 5, 10), c.arr = c(0, 4, 10),
#        Na = c(135, 157, 164), amylase = c(120, NA, 400), lipase = c(155, 170, NA),
#        norad = c(0, 0.02, 0.06), dopam = c(0, 5, 11)) # 9, 19, 25
#
# p_pass(age = c(25, 31, 45), bmi = c(18, 22, 35), icu = c(2, 5, 10), c.arr = c(0, 4, 10), Na = c(135, 157, 164), amylase = c(120, 145, 400), norad = c(0, 0.02, 0.06), dopam = c(0, 5, 11)) # 9, 19, 25
