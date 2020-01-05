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
#' Reference: Rao PS, Schaubel DE, Guidinger MK, et al. A Comprehensive Risk Quantification Score
#' for Deceased Donor Kidneys: The Kidney Donor Risk Index. Transplantation 2009; 88:231-236.
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
#' Reference: Rao PS, Schaubel DE, Guidinger MK, et al. A Comprehensive Risk Quantification Score
#' for Deceased Donor Kidneys: The Kidney Donor Risk Index. Transplantation 2009; 88:231-236.
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
  uskdri(age, height, weight, eth, htn, dm, cva, creat, hcv, dcd, scaling, units = "US")
}

#' US KDPI
#'
#' Vectorised function to calculate US KDPI percentile as published by OPTN/UNOS. Please note
#' that this function uses creatinine measured in µmol/l by default, but can be changed to
#' mg/dl if the optional units parameter is set to "US".
#'
#' The KDRI is calculated from KDRI normalised by a scaling factor based on the median KDRI in the previous year.
#' For 2018, this was approximately 1.250609 as is published on the OPTN website.
#' The scaling parameter in this function defaults to 1, so can be left out to calculate the KDPI without scaling.
#'
#' This function requires the dplyr package to be installed.
#'
#' Reference: Rao PS, Schaubel DE, Guidinger MK, et al. A Comprehensive Risk Quantification Score
#' for Deceased Donor Kidneys: The Kidney Donor Risk Index. Transplantation 2009; 88:231-236.
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
#' kdpi(age = 40, height = 170, weight = 80, eth = "non-black", htn = 0, dm = 0,
#'     cva = 0, creat = 120, hcv = 0, dcd = 0, scaling = 1.250609, units = "SI")
#'
#' # with creatinine in mg/dl
#' kdpi(age = 40, height = 170, weight = 80, eth = "non-black", htn = 0, dm = 0,
#'     cva = 0, creat = 1.4, hcv = 0, dcd = 0, scaling = 1.250609, units = "US")
kdpi = function(age, height, weight, eth, htn, dm, cva, creat, hcv, dcd, scaling = 1, units = "SI") {
  kdri = uskdri(age, height, weight, eth, htn, dm, cva, creat, hcv, dcd, scaling, units)
  kdpi_lookup(kdri, scaling = 1)
}

#' US KDPI (US units)
#'
#' Wrapper function for the uskdpi() vectorised function to calculate US KDPI percentile as published by OPTN/UNOS,
#' using creatinine measured in mg/dl (please use the uskdpi() function for µmol/l).
#'
#' The KDRI is calculated from KDRI normalised by a scaling factor based on the median KDRI in the previous year.
#' For 2018, this was approximately 1.250609 as is published on the OPTN website.
#' The scaling parameter in this function defaults to 1, so can be left out to calculate the KDPI without scaling.
#'
#' This function requires the dplyr package to be installed.
#'
#' Reference: Rao PS, Schaubel DE, Guidinger MK, et al. A Comprehensive Risk Quantification Score
#' for Deceased Donor Kidneys: The Kidney Donor Risk Index. Transplantation 2009; 88:231-236.
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
#' # with creatinine in mg/dl
#' kdpi_US(age = 40, height = 170, weight = 80, eth = "non-black", htn = 0, dm = 0,
#'     cva = 0, creat = 1.4, hcv = 0, dcd = 0, scaling = 1.250609)
kdpi_US = function(age, height, weight, eth, htn, dm, cva, creat, hcv, dcd, scaling = 1) {
  kdri = uskdri(age, height, weight, eth, htn, dm, cva, creat, hcv, dcd, scaling, units = "US")
  kdpi_lookup(kdri, scaling = 1)
}

#' US KDPI lookup function
#'
#' A vectorised function to convert kdri KDRI scores to KDPI percentiles. If the OPTN scaling factor was
#' not used when calculating the KDRI, it can be set here using the optional scaling parameter which
#' uses a default value of 1 (for no scaling).
#'
#' This function requires the dplyr package to be installed.
#'
#' @param kdri numeric vector of KDRI values
#' @param scaling optional parameter for scaling factor (default is 1)
#'
#' @return numeric vector of KDPI percentiles
#' @export
#'
#' @examples
#' # if scaling factor was used when calculating KDRI
#' kdpi_lookup(1.25)
#'
#' # if scaling factor for 2018 needs to be applied
#' kdpi_lookup(1.25, scaling =  1.2506957544151)
kdpi_lookup = function(kdri, scaling = 1) {
  if(!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The dplyr package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  kdpi = dplyr::case_when(
    kdri <= 0.49876511772967 ~ 0,
    kdri <= 0.56307750275834 ~ 1,
    kdri <= 0.58488618864631 ~ 2,
    kdri <= 0.59976342091324 ~ 3,
    kdri <= 0.61217321834846 ~ 4,
    kdri <= 0.62394066704334 ~ 5,
    kdri <= 0.63542577053361 ~ 6,
    kdri <= 0.6445737715023 ~ 7,
    kdri <= 0.65522066955566 ~ 8,
    kdri <= 0.66500946855438 ~ 9,
    kdri <= 0.67285865074985 ~ 10,
    kdri <= 0.68051868467058 ~ 11,
    kdri <= 0.68878920374291 ~ 12,
    kdri <= 0.69636731947259 ~ 13,
    kdri <= 0.70315523051737 ~ 14,
    kdri <= 0.71022603532484 ~ 15,
    kdri <= 0.71776001984627 ~ 16,
    kdri <= 0.72658195089529 ~ 17,
    kdri <= 0.73384165685169 ~ 18,
    kdri <= 0.74114128784946 ~ 19,
    kdri <= 0.74824860245686 ~ 20,
    kdri <= 0.7551113931652 ~ 21,
    kdri <= 0.76208894834223 ~ 22,
    kdri <= 0.76968857037996 ~ 23,
    kdri <= 0.77912794810015 ~ 24,
    kdri <= 0.7864233644084 ~ 25,
    kdri <= 0.79429019469951 ~ 26,
    kdri <= 0.80043495925199 ~ 27,
    kdri <= 0.80949231453375 ~ 28,
    kdri <= 0.81714722369107 ~ 29,
    kdri <= 0.82459740969579 ~ 30,
    kdri <= 0.83413568333986 ~ 31,
    kdri <= 0.84510026657137 ~ 32,
    kdri <= 0.8536373705678 ~ 33,
    kdri <= 0.86244836373684 ~ 34,
    kdri <= 0.87073290711542 ~ 35,
    kdri <= 0.87780711172565 ~ 36,
    kdri <= 0.88495365728057 ~ 37,
    kdri <= 0.89361521923418 ~ 38,
    kdri <= 0.90266841208094 ~ 39,
    kdri <= 0.91084731254587 ~ 40,
    kdri <= 0.9199039652366 ~ 41,
    kdri <= 0.92744665590833 ~ 42,
    kdri <= 0.93528273970436 ~ 43,
    kdri <= 0.94661766515018 ~ 44,
    kdri <= 0.95820187283546 ~ 45,
    kdri <= 0.96605433397544 ~ 46,
    kdri <= 0.97392517565498 ~ 47,
    kdri <= 0.9820431801057 ~ 48,
    kdri <= 0.99114028067804 ~ 49,
    kdri <= 1.0000000000000 ~ 50,
    kdri <= 1.00962603508795 ~ 51,
    kdri <= 1.0185499518415 ~ 52,
    kdri <= 1.02886268206259 ~ 53,
    kdri <= 1.0394835465958 ~ 54,
    kdri <= 1.04827336061879 ~ 55,
    kdri <= 1.05844410040513 ~ 56,
    kdri <= 1.06873958906964 ~ 57,
    kdri <= 1.07846636550991 ~ 58,
    kdri <= 1.09098395494362 ~ 59,
    kdri <= 1.10332240959206 ~ 60,
    kdri <= 1.11393634618198 ~ 61,
    kdri <= 1.1264600314863 ~ 62,
    kdri <= 1.13689401974787 ~ 63,
    kdri <= 1.14952682364131 ~ 64,
    kdri <= 1.16082620815589 ~ 65,
    kdri <= 1.172567747438 ~ 66,
    kdri <= 1.18304544582233 ~ 67,
    kdri <= 1.1959968239922 ~ 68,
    kdri <= 1.20691558281891 ~ 69,
    kdri <= 1.21964520393117 ~ 70,
    kdri <= 1.23141017941636 ~ 71,
    kdri <= 1.24438222257714 ~ 72,
    kdri <= 1.25983404251641 ~ 73,
    kdri <= 1.27501762239354 ~ 74,
    kdri <= 1.2904874303634 ~ 75,
    kdri <= 1.30438279915696 ~ 76,
    kdri <= 1.32210557724578 ~ 77,
    kdri <= 1.33862905906667 ~ 78,
    kdri <= 1.35425027686849 ~ 79,
    kdri <= 1.37148103037223 ~ 80,
    kdri <= 1.39244333640203 ~ 81,
    kdri <= 1.41295039933911 ~ 82,
    kdri <= 1.43347275466868 ~ 83,
    kdri <= 1.45649082784051 ~ 84,
    kdri <= 1.47831067495385 ~ 85,
    kdri <= 1.5014660538092 ~ 86,
    kdri <= 1.5300310051455 ~ 87,
    kdri <= 1.55612693470058 ~ 88,
    kdri <= 1.58573242755821 ~ 89,
    kdri <= 1.61830612440207 ~ 90,
    kdri <= 1.64828606573425 ~ 91,
    kdri <= 1.68394625461656 ~ 92,
    kdri <= 1.7213898209904 ~ 93,
    kdri <= 1.76828827076567 ~ 94,
    kdri <= 1.82797350338431 ~ 95,
    kdri <= 1.89480058347067 ~ 96,
    kdri <= 1.96598383615514 ~ 97,
    kdri <= 2.06845096281863 ~ 98,
    kdri <= 2.26132290901436 ~ 99,
    kdri <= 4.74323788214012 ~ 100,
    TRUE ~ 100)
  kdpi
}
