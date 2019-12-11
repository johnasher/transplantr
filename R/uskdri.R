#' US Kidney Donor Risk Index
#'
#' Vectorised function to calculate US Kidney Donor Risk Index as published by UNOS. Please note
#' that this function uses creatinine measured in µmol/l by default, but can be changed to
#' mg/dl if the optional units parameter is set to "US".
#'
#' The KDRI is normalised by a scaling factor based on the median KDRI in the previous year. For 2018, this was
#' approximately 1.250609 as is published on the OPTN website. The scaling parameter in this function defaults to 1,
#' so can be left out to calculate the KDRI without scaling.
#'
#' @param age numeric vector of donor ages in years
#' @param height numeric vector of donor heights in cm
#' @param weight numeric vector of donor weights in kg
#' @param eth character string vector of donor ethnicity, "black" or "non-black"
#' @param htn numeric vector of donor hypertension history (1 = yes, 0 = no)
#' @param dm numeric vector of donor diabetes history (1 = yes, 0 = no)
#' @param cva numeric vector of whether donor death due to CVA (1 = yes, 0 = no)
#' @param creat numeric vector of donor serum creatinine (µmol/l)
#' @param hcv numeric vector of donor hepatitis C history (1 = yes, 0 = no)
#' @param dcd numeric vector of type of donor (1 = DCD, 0 = DBD)
#' @param scaling single numeric value for OPTN scaling factor (optional, defaults to 1)
#' @param units single string value to indicate creatinine units ("SI" for µmol/l, "US" for mg/dl)
#'
#' @return numeric vector of US KDRI values
#' @export
#'
#' @examples
#' # with creatinine in µmol/l (units = "SI" can be omitted)
#' uskdri(age = 40, height = 170, weight = 80, eth = "non-black", htn = 0, dm = 0,
#'     cva = 0, creat = 120, hcv = 0, dcd = 0, scaling = 1.250609, units = "SI")
#'
#' # with creatinine in mg/dl
#' uskdri(age = 40, height = 170, weight = 80, eth = "non-black", htn = 0, dm = 0,
#'     cva = 0, creat = 1.4, hcv = 0, dcd = 0, scaling = 1.250609, units = "US")
uskdri = function(age, height, weight, eth, htn, dm, cva, creat, hcv, dcd, scaling = 1, units = "SI"){
  # convert creatinine to mg/dl
  if (units == "SI") {
    creat = creat / 88.4
  }

  agevar = 0.0128 * (age - 40) + ifelse(age < 18, -0.0194 * (age - 18), 0) + ifelse(age > 50, 0.0107 * (age - 50), 0)
  heightvar = -0.0464 * (height - 170) / 10
  weightvar = ifelse(weight < 80, -0.0199 * (weight - 80) / 5, 0)
  ethvar = ifelse(eth == "black", 0.1790, 0)
  htnvar = 0.1260 * htn
  dmvar = 0.1300 * dm
  cvavar = 0.0881 * cva
  creatvar = 0.2200 * (creat - 1) + ifelse(creat > 1.5, -0.2090 * (creat - 1.5), 0)
  hcvvar = 0.2400 * hcv
  dcdvar = 0.1330 * dcd
  kdri = exp(agevar + heightvar + weightvar + ethvar + htnvar +
               dmvar + cvavar + creatvar + hcvvar + dcdvar)
  kdri / scaling
}

#' US Kidney Donor Risk Index (US units)
#'
#' Wrapper function for the uskdri() vectorised function to calculate US Kidney Donor Risk Index
#' as published by UNOS but using mg/dl as the units for creatinine.
#'
#' The KDRI is normalised by a scaling factor based on the median KDRI in the previous year. For 2018, this was
#' approximately 1.250609 as is published on the OPTN website. The scaling parameter in this function defaults to 1,
#' so can be left out to calculate the KDRI without scaling.
#'
#' @param age numeric vector of donor ages in years
#' @param height numeric vector of donor heights in cm
#' @param weight numeric vector of donor weights in kg
#' @param eth character string vector of donor ethnicity, "black" or "non-black"
#' @param htn numeric vector of donor hypertension history (1 = yes, 0 = no)
#' @param dm numeric vector of donor diabetes history (1 = yes, 0 = no)
#' @param cva numeric vector of whether donor death due to CVA (1 = yes, 0 = no)
#' @param creat numeric vector of donor serum creatinine (mg/dl)
#' @param hcv numeric vector of donor hepatitis C history (1 = yes, 0 = no)
#' @param dcd numeric vector of type of donor (1 = DCD, 0 = DBD)
#' @param scaling single numeric value for OPTN scaling factor (optional, defaults to 1)
#'
#' @return numeric vector of US KDRI values
#' @export
#'
#' @examples
#' uskdri(age = 40, height = 170, weight = 80, eth = "non-black", htn = 0, dm = 0,
#'     cva = 0, creat = 1.4, hcv = 0, dcd = 0)
uskdri_US = function(age, height, weight, eth, htn, dm, cva, creat, hcv, dcd, scaling = 1) {
  uskdri(age, height, weight, eth, htn, dm, cva, creat, hcv, dcd, units = "US")
}
