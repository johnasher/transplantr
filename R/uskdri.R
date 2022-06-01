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
  kdpi_lookup(kdri)
}

#' US KDPI (US units)
#'
#' Wrapper function for the kdpi() vectorised function to calculate US KDPI percentile as published by OPTN/UNOS,
#' using creatinine measured in mg/dl (please use the kdpi() function for µmol/l).
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
  kdpi_lookup(kdri)
}

#' US KDPI lookup function
#'
#' A vectorised function to convert KDRI scores to KDPI percentiles. In order to obtain
#' valid scores the KDRI scores need to be scaled beforehand.
#'
#' This function requires the dplyr package to be installed.
#'
#' @param kdri numeric vector of KDRI values
#'
#' @return numeric vector of KDPI percentiles
#' @export
#'
#' @examples
#' kdpi_lookup(1.25)
kdpi_lookup = function(kdri) {
  if(!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The dplyr package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  kdpi = dplyr::case_when(
    kdri < 0 ~ NA_real_,
    
    kdri <= 0.46142857688301 ~ 0,
    kdri <= 0.54197901283437 ~ 1,
    kdri <= 0.56160304740180 ~ 2,
    kdri <= 0.57764949747539 ~ 3,
    kdri <= 0.59201222143224 ~ 4,
    
    kdri <= 0.60353092319351 ~ 5,
    kdri <= 0.61457764696553 ~ 6,
    kdri <= 0.62524856769420 ~ 7,
    kdri <= 0.63558591800552 ~ 8,
    kdri <= 0.64587583471311 ~ 9,
    
    kdri <= 0.65501967868782 ~ 10,
    kdri <= 0.66369803467456 ~ 11,
    kdri <= 0.67393069484252 ~ 12,
    kdri <= 0.68234628550006 ~ 13,
    kdri <= 0.68996942083730 ~ 14,
    
    kdri <= 0.69819838318796 ~ 15,
    kdri <= 0.70647462075572 ~ 16,
    kdri <= 0.71488899506376 ~ 17,
    kdri <= 0.72338622620749 ~ 18,
    kdri <= 0.73122212560016 ~ 19,
    
    kdri <= 0.73948742626363 ~ 20,
    kdri <= 0.74767416814119 ~ 21,
    kdri <= 0.75489183869684 ~ 22,
    kdri <= 0.76328988492547 ~ 23,
    kdri <= 0.77263540810356 ~ 24,
    
    kdri <= 0.78104071392144 ~ 25,
    kdri <= 0.78961641663597 ~ 26,
    kdri <= 0.79763195276282 ~ 27,
    kdri <= 0.80629306366570 ~ 28,
    kdri <= 0.81315950139425 ~ 29,
    
    kdri <= 0.82133189024934 ~ 30,
    kdri <= 0.82993557639931 ~ 31,
    kdri <= 0.83833021902837 ~ 32,
    kdri <= 0.84644064330782 ~ 33,
    kdri <= 0.85517598569799 ~ 34,
    
    kdri <= 0.86354609606526 ~ 35,
    kdri <= 0.87190221510694 ~ 36,
    kdri <= 0.88063855849677 ~ 37,
    kdri <= 0.88865695084855 ~ 38,
    kdri <= 0.89695104031643 ~ 39,
    
    kdri <= 0.90553622222640 ~ 40,
    kdri <= 0.91404086359403 ~ 41,
    kdri <= 0.92315954924259 ~ 42,
    kdri <= 0.93233041928184 ~ 43,
    kdri <= 0.94219294550761 ~ 44,
    
    kdri <= 0.95174513059252 ~ 45,
    kdri <= 0.96312121772671 ~ 46,
    kdri <= 0.97205975507178 ~ 47,
    kdri <= 0.98215317510147 ~ 48,
    kdri <= 0.99183352829341 ~ 49,
    
    kdri <= 1.00000000000000 ~ 50,
    kdri <= 1.01017417074092 ~ 51,
    kdri <= 1.01835848244924 ~ 52,
    kdri <= 1.02684438950822 ~ 53,
    kdri <= 1.03579785070930 ~ 54,
    
    kdri <= 1.04562207982550 ~ 55,
    kdri <= 1.05722970377490 ~ 56,
    kdri <= 1.06708646003775 ~ 57,
    kdri <= 1.07634603079629 ~ 58,
    kdri <= 1.08575543697971 ~ 59,
    
    kdri <= 1.09710182684712 ~ 60,
    kdri <= 1.10693175142868 ~ 61,
    kdri <= 1.11742172465403 ~ 62,
    kdri <= 1.12918033244636 ~ 63,
    kdri <= 1.14105126527764 ~ 64,
    
    kdri <= 1.15331684032151 ~ 65,
    kdri <= 1.16542473503044 ~ 66,
    kdri <= 1.17696136175085 ~ 67,
    kdri <= 1.18927324253637 ~ 68,
    kdri <= 1.20164565923095 ~ 69,
    
    kdri <= 1.21541307690033 ~ 70,
    kdri <= 1.22911205232018 ~ 71,
    kdri <= 1.24245840305292 ~ 72,
    kdri <= 1.25730683392563 ~ 73,
    kdri <= 1.27215765519370 ~ 74,
    
    kdri <= 1.28707474004391 ~ 75,
    kdri <= 1.30379077237733 ~ 76,
    kdri <= 1.32043073795346 ~ 77,
    kdri <= 1.33470727747430 ~ 78,
    kdri <= 1.35223394649512 ~ 79,
    
    kdri <= 1.36711461717176 ~ 80,
    kdri <= 1.38613042767243 ~ 81,
    kdri <= 1.40388472846480 ~ 82,
    kdri <= 1.42405153123492 ~ 83,
    kdri <= 1.44236712388566 ~ 84,
    
    kdri <= 1.46037025078765 ~ 85,
    kdri <= 1.48228201160165 ~ 86,
    kdri <= 1.50315404920549 ~ 87,
    kdri <= 1.53116364711516 ~ 88,
    kdri <= 1.55977565496533 ~ 89,
    
    kdri <= 1.58862998476342 ~ 90,
    kdri <= 1.62205197669837 ~ 91,
    kdri <= 1.65260362699008 ~ 92,
    kdri <= 1.70084638382016 ~ 93,
    kdri <= 1.75251517684434 ~ 94,
    
    kdri <= 1.80242900060437 ~ 95,
    kdri <= 1.85527326893612 ~ 96,
    kdri <= 1.93490145976700 ~ 97,
    kdri <= 2.03614843248107 ~ 98,
    kdri <= 2.21047627199221 ~ 99,
    
    kdri <= 3.18031420972338 ~ 100,
    kdri <= 20 ~ 100,
    TRUE ~ NA_real_)
  kdpi
}
