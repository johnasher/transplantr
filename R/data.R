#' Simulated dataset to illustrate eGFR calculator vignette.
#'
#' A simulated dataset of patient characteristics and creatinine levels
#'
#' @format A data frame with 4 rows and 6 variables:
#' \describe{
#'   \item{Creatinine}{serum creatinine in µmol/l}
#'   \item{Age}{patient age in years}
#'   \item{Sex}{Patient sex, M or F}
#'   \item{Ethnicity}{Patient ethnicity}
#'   \item{Weight}{Patient weight in kg}
#'   \item{Height}{Patient height in cm}
#'   ...
#' }
"results"

#' Simulated dataset to illustrate eGFR calculator vignette.
#'
#' A simulated dataset of patient characteristics and creatinine levels
#'
#' @format A data frame with 4 rows and 6 variables:
#' \describe{
#'   \item{Creatinine}{serum creatinine in mg/dl}
#'   \item{Sex}{Patient sex, M or F}
#'   \item{Age}{patient age in years}
#'   \item{Ethnicity}{Patient ethnicity}
#'   \item{Weight}{Patient weight in kg}
#'   \item{Height}{Patient height in cm}
#'   ...
#' }
"results_US"

#' Simulated dataset to illustrate serial results eGFR calculator vignette.
#'
#' A simulated dataset of patient characteristics and creatinine levels
#'
#' @format A data frame with 4 rows and 6 variables:
#' \describe{
#'   \item{Age}{patient age in years}
#'   \item{Sex}{Patient sex, M or F}
#'   \item{Ethnicity}{Patient ethnicity}
#'   \item{Creatinine_1yr}{serum creatinine at 1 year, in µmol/l}
#'   \item{Creatinine_5yr}{serum creatinine at 5 years, in µmol/l}
#'
#'   ...
#' }
"serial.results"

#' Simulated dataset to illustrate mismatches for HLA vignette.
#'
#' A simulated dataset of HLA mismatches
#'
#' @format A data frame with 4 rows and 5 variables:
#' \describe{
#'   \item{HLA.A.MM}{HLA A mismatch}
#'   \item{HLA.B.MM}{HLA B mismatch}
#'   \item{HLA.DR.MM}{HLA DR mismatch}
#'   \item{HLA.MM}{HLA mismatch as string}
#'   \item{HLA.MM.s}{HLA mismatch as string with separators}
#'   ...
#' }
"mismatches"

#' Simulated dataset of donors to illustrate KDRI vignette.
#'
#' A simulated dataset of kidney donors to illustrate the KDRI vignette
#'
#' @format A data frame with 4 rows and 7 variables:
#' \describe{
#'   \item{Donor.Age}{donor age in years}
#'   \item{Donor.Height}{donor height in cm}
#'   \item{Donor.Hypertension}{donor hypertension}
#'   \item{Donor.Sex}{donor sex}
#'   \item{Donor.CMV}{donor CMV status}
#'   \item{Donor.GFR}{donor GFR}
#'   \item{Donor.Hospital_Stay}{donor hospital stay in days}
#'   ...
#' }
"kidney.donors"

#' Simulated dataset to illustrate MELD calculator vignette.
#'
#' A simulated dataset of liver patient characteristics for the MELD vignette
#'
#' @format A data frame with 4 rows and 6 variables:
#' \describe{
#'   \item{Patient.Age}{patient age in years}
#'   \item{Patient.INR}{Patient INR}
#'   \item{Patient.Bilirubin}{Patient serum bilirubin in µmol/l}
#'   \item{Patient.Creatinine}{serum serum creatinine in µmol/l}
#'   \item{Patient.Sodium}{Patient serum sodium in mmol/l}
#'   \item{Patient.Dialysed}{Whether patient dialysed (1 = yes, 0 = no)}
#'   ...
#' }
"liver.pts"

