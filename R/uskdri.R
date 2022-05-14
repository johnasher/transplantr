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
    
    kdri <= 0.49288618187448 ~ 0,
    kdri <= 0.55766759610873 ~ 1,
    kdri <= 0.57704916969103 ~ 2,
    kdri <= 0.59364308928216 ~ 3,
    kdri <= 0.60609593309627 ~ 4,
    
    kdri <= 0.61821968953949 ~ 5,
    kdri <= 0.62983877817678 ~ 6,
    kdri <= 0.63920695742794 ~ 7,
    kdri <= 0.64902957059053 ~ 8,
    kdri <= 0.65809432951739 ~ 9,
    
    kdri <= 0.66612562506720 ~ 10,
    kdri <= 0.67558452491476 ~ 11,
    kdri <= 0.68412337829430 ~ 12,
    kdri <= 0.69185602594588 ~ 13,
    kdri <= 0.69880621985069 ~ 14,
    
    kdri <= 0.70790554868322 ~ 15,
    kdri <= 0.71579819664996 ~ 16,
    kdri <= 0.72404263514936 ~ 17,
    kdri <= 0.73190408867748 ~ 18,
    kdri <= 0.74144299374481 ~ 19,
    
    kdri <= 0.74970386281724 ~ 20,
    kdri <= 0.75737785660771 ~ 21,
    kdri <= 0.76631975317226 ~ 22,
    kdri <= 0.77361805204334 ~ 23,
    kdri <= 0.78264427231080 ~ 24,
    
    kdri <= 0.79070589237185 ~ 25,
    kdri <= 0.79775240427962 ~ 26,
    kdri <= 0.80533011892674 ~ 27,
    kdri <= 0.81290014488090 ~ 28,
    kdri <= 0.82079901878607 ~ 29,
    
    kdri <= 0.82783534753742 ~ 30,
    kdri <= 0.83590776577662 ~ 31,
    kdri <= 0.84241964344697 ~ 32,
    kdri <= 0.85068701756456 ~ 33,
    kdri <= 0.85855116695904 ~ 34,
    
    kdri <= 0.86674973513388 ~ 35,
    kdri <= 0.87531645075402 ~ 36,
    kdri <= 0.88387732375932 ~ 37,
    kdri <= 0.89239448179147 ~ 38,
    kdri <= 0.90094830333877 ~ 39,
    
    kdri <= 0.91013985906309 ~ 40,
    kdri <= 0.91862526835859 ~ 41,
    kdri <= 0.92683752357503 ~ 42,
    kdri <= 0.93686414179677 ~ 43,
    kdri <= 0.94645959319719 ~ 44,
    
    kdri <= 0.95388992193620 ~ 45,
    kdri <= 0.96357341294590 ~ 46,
    kdri <= 0.97223571380888 ~ 47,
    kdri <= 0.98143155774226 ~ 48,
    kdri <= 0.99161832376591 ~ 49,
    
    kdri <= 1.00000000000000 ~ 50,
    kdri <= 1.01019461458336 ~ 51,
    kdri <= 1.02114342056441 ~ 52,
    kdri <= 1.03235541277888 ~ 53,
    kdri <= 1.04108037909272 ~ 54,
    
    kdri <= 1.05134994466249 ~ 55,
    kdri <= 1.06124526827265 ~ 56,
    kdri <= 1.07289757239163 ~ 57,
    kdri <= 1.08168823370293 ~ 58,
    kdri <= 1.09378373320070 ~ 59,
    
    kdri <= 1.10488692564162 ~ 60,
    kdri <= 1.11276621409158 ~ 61,
    kdri <= 1.12617056845307 ~ 62,
    kdri <= 1.13787330701880 ~ 63,
    kdri <= 1.15032326169124 ~ 64,
    
    kdri <= 1.16220957579890 ~ 65,
    kdri <= 1.17507855419745 ~ 66,
    kdri <= 1.18902851528348 ~ 67,
    kdri <= 1.20326536655901 ~ 68,
    kdri <= 1.21549694329305 ~ 69,
    
    kdri <= 1.22745509802558 ~ 70,
    kdri <= 1.24150331981455 ~ 71,
    kdri <= 1.25477089231939 ~ 72,
    kdri <= 1.26711542725798 ~ 73,
    kdri <= 1.28049023409929 ~ 74,
    
    kdri <= 1.29521020345262 ~ 75,
    kdri <= 1.30795519779436 ~ 76,
    kdri <= 1.32331056421167 ~ 77,
    kdri <= 1.33954901319506 ~ 78,
    kdri <= 1.35417308679937 ~ 79,
    
    kdri <= 1.37299460853031 ~ 80,
    kdri <= 1.39325536754786 ~ 81,
    kdri <= 1.41385921859655 ~ 82,
    kdri <= 1.43186673205109 ~ 83,
    kdri <= 1.44779686858670 ~ 84,
    
    kdri <= 1.46753663431589 ~ 85,
    kdri <= 1.48887827371017 ~ 86,
    kdri <= 1.51131328801805 ~ 87,
    kdri <= 1.53637078180307 ~ 88,
    kdri <= 1.56599436621390 ~ 89,
    
    kdri <= 1.59388080454631 ~ 90,
    kdri <= 1.62599998190581 ~ 91,
    kdri <= 1.66067309649100 ~ 92,
    kdri <= 1.69776893848280 ~ 93,
    kdri <= 1.74595729786717 ~ 94,
    
    kdri <= 1.80117569936833 ~ 95,
    kdri <= 1.86909994095062 ~ 96,
    kdri <= 1.94997197367416 ~ 97,
    kdri <= 2.06166972375206 ~ 98,
    kdri <= 2.23606163552925 ~ 99,
    
    kdri <= 3.32515071199901 ~ 100,
    kdri <= 99 ~ 100,
    TRUE ~ NA_real_)
  kdpi
}
