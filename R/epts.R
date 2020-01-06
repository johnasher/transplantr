#' Estimated Post-Transplant Survival Score (EPTS)
#'
#' A vectorised function to calculate raw EPTS scores for norm-related prediction of
#' patient survival after adult renal transplants. This function generates the raw
#' EPTS scores which can be converted to percentiles using the lookup table on the OPTN
#' website at https://optn.transplant.hrsa.gov/media/2973/epts_mapping_table_2018.pdf
#'
#' References: https://optn.transplant.hrsa.gov/resources/allocation-calculators/epts-calculator/ and
#' https://optn.transplant.hrsa.gov/media/1511/guide_to_calculating_interpreting_epts.pdf
#'
#' @param age numeric vector of patient age in years (with decimals)
#' @param dm numeric vector of whether patient diabetic (1 = yes, 0 = no)
#' @param prev_tx numeric vector of whether patient has a previous solid organ transplant
#' @param dx numeric vector of duration of dialysis in years (with decimals)
#'
#' @return numeric vector of raw EPTS scores
#' @export
#'
#' @examples
#' raw_epts(age = 23.5838467, dm = 0, prev_tx = 1, dx = 5.0814511) # 0.9666517
#' raw_epts(age = 52.8788501, dm = 0, prev_tx = 0, dx = 0) # 1.440306
#' raw_epts(age = 22.5242984, dm = 1, prev_tx = 1, dx = 6.8747433) # 1.868751
raw_epts <- function(age, dm, prev_tx, dx) {
  agebase = max(age - 25, 0)
  agevar = 0.047 * agebase - 0.015 * dm * agebase
  prevtxvar = 0.398 * prev_tx - 0.237 * dm * prev_tx
  log_dx = log(dx + 1)
  dxnull = ifelse(dx == 0, 1, 0)
  dxvar1 = 0.315 * log_dx - 0.099 * dm * log_dx
  dxvar2 = 0.130 * dxnull - 0.348 * dm * dxnull

  agevar + prevtxvar + dxvar1 + dxvar2 + 1.262 * dm
}

#' Estimated Post-Transplant Survival Score (EPTS)
#'
#' A vectorised function to calculate EPTS scores as percentiles for norm-related prediction of
#' patient survival after adult renal transplants. This function generates the EPTN scores
#' as percentiles using the most recent lookup table on the OPTN
#' website published in March 2019 and using SRTR data from 2018. The table can be found
#' at https://optn.transplant.hrsa.gov/media/2973/epts_mapping_table_2018.pdf
#'
#' This function requires the dplyr package to be installed.
#
#' References: https://optn.transplant.hrsa.gov/resources/allocation-calculators/epts-calculator/ and
#' https://optn.transplant.hrsa.gov/media/1511/guide_to_calculating_interpreting_epts.pdf
#'
#' @param age numeric vector of patient age in years (with decimals)
#' @param dm numeric vector of whether patient diabetic (1 = yes, 0 = no)
#' @param prev_tx numeric vector of whether patient has a previous solid organ transplant
#' @param dx numeric vector of duration of dialysis in years (with decimals)
#'
#' @return numeric vector of EPTS scores as percentiles
#' @export
#'
#' @examples
#' epts(age = 23.5838467, dm = 0, prev_tx = 1, dx = 5.0814511)
#' epts(age = 52.8788501, dm = 0, prev_tx = 0, dx = 0)
#' epts(age = 22.5242984, dm = 1, prev_tx = 1, dx = 6.8747433)
epts = function(age, dm, prev_tx, dx) {
  raw = raw_epts(age, dm, prev_tx, dx)
  epts = epts_lookup(raw)
  epts
}

#' EPTS lookup function
#'
#' A vectorised function to convert EPTS scores to percentiles for norm-related prediction of
#' patient survival after adult renal transplants. This calculator uses the most recent lookup
#' table published in March 2019 and using SRTR data from 2018 from the OPTN
#' website at https://optn.transplant.hrsa.gov/media/2973/epts_mapping_table_2018.pdf
#'
#' This function requires the dplyr package to be installed.
#'
#' References: https://optn.transplant.hrsa.gov/resources/allocation-calculators/epts-calculator/ and
#' https://optn.transplant.hrsa.gov/media/1511/guide_to_calculating_interpreting_epts.pdf
#'
#' @param raw numeric vector of raw EPTS scores
#'
#' @return numeric vector of EPTS scores as percentiles
#' @export
#'
#' @examples
#' epts_lookup(1.54) # 21
epts_lookup = function(raw) {
  if(!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The dplyr package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  epts = dplyr::case_when(
    raw <= 0.01760937493806 ~ 0,
    raw <= 0.26646040897072 ~ 1,
    raw <= 0.43019022030158 ~ 2,
    raw <= 0.53311909650924 ~ 3,
    raw <= 0.6326384644618 ~ 4,
    raw <= 0.72237546897176 ~ 5,
    raw <= 0.79626762491444 ~ 6,
    raw <= 0.86666326886303 ~ 7,
    raw <= 0.93318206707734 ~ 8,
    raw <= 0.99713552361396 ~ 9,
    raw <= 1.05717462942817 ~ 10,
    raw <= 1.11295047122014 ~ 11,
    raw <= 1.16625651808412 ~ 12,
    raw <= 1.21680853847071 ~ 13,
    raw <= 1.26376711078916 ~ 14,
    raw <= 1.31368583162218 ~ 15,
    raw <= 1.35657090379317 ~ 16,
    raw <= 1.40054004106776 ~ 17,
    raw <= 1.44364750171116 ~ 18,
    raw <= 1.48333283375386 ~ 19,
    raw <= 1.52214579055442 ~ 20,
    raw <= 1.55765040533122 ~ 21,
    raw <= 1.58995558100723 ~ 22,
    raw <= 1.62089164593683 ~ 23,
    raw <= 1.65069609856263 ~ 24,
    raw <= 1.67861943874059 ~ 25,
    raw <= 1.70721730298086 ~ 26,
    raw <= 1.73409171800137 ~ 27,
    raw <= 1.76033059548255 ~ 28,
    raw <= 1.78570658610832 ~ 29,
    raw <= 1.81141204654346 ~ 30,
    raw <= 1.83677817077353 ~ 31,
    raw <= 1.86147227926078 ~ 32,
    raw <= 1.8856943405636 ~ 33,
    raw <= 1.90975222450376 ~ 34,
    raw <= 1.93251418676668 ~ 35,
    raw <= 1.95384268564753 ~ 36,
    raw <= 1.97509582477755 ~ 37,
    raw <= 1.99555578370979 ~ 38,
    raw <= 2.01713252865473 ~ 39,
    raw <= 2.03840588637919 ~ 40,
    raw <= 2.0573216975 ~ 41,
    raw <= 2.0760282837 ~ 42,
    raw <= 2.0946334302 ~ 43,
    raw <= 2.1124041904 ~ 44,
    raw <= 2.1309607446 ~ 45,
    raw <= 2.1493271732 ~ 46,
    raw <= 2.1665608233 ~ 47,
    raw <= 2.1831697467 ~ 48,
    raw <= 2.2002004889 ~ 49,
    raw <= 2.2167886167 ~ 50,
    raw <= 2.2327798091 ~ 51,
    raw <= 2.249696783 ~ 52,
    raw <= 2.2657166324 ~ 53,
    raw <= 2.2815880018 ~ 54,
    raw <= 2.2969371337 ~ 55,
    raw <= 2.3123839836 ~ 56,
    raw <= 2.3273073238 ~ 57,
    raw <= 2.3420260096 ~ 58,
    raw <= 2.3577084189 ~ 59,
    raw <= 2.3726607709 ~ 60,
    raw <= 2.3878466804 ~ 61,
    raw <= 2.4036167009 ~ 62,
    raw <= 2.4189776636 ~ 63,
    raw <= 2.4346310746 ~ 64,
    raw <= 2.4502258727 ~ 65,
    raw <= 2.4651170255 ~ 66,
    raw <= 2.4801013005 ~ 67,
    raw <= 2.4945728953 ~ 68,
    raw <= 2.5093709788 ~ 69,
    raw <= 2.52526276 ~ 70,
    raw <= 2.5420048257 ~ 71,
    raw <= 2.5579622234 ~ 72,
    raw <= 2.5740517596 ~ 73,
    raw <= 2.5916171143 ~ 74,
    raw <= 2.6082122769 ~ 75,
    raw <= 2.6254483231 ~ 76,
    raw <= 2.6420702932 ~ 77,
    raw <= 2.6597107489 ~ 78,
    raw <= 2.677196478 ~ 79,
    raw <= 2.6963709068 ~ 80,
    raw <= 2.7149238167 ~ 81,
    raw <= 2.733058547 ~ 82,
    raw <= 2.7525722108 ~ 83,
    raw <= 2.7725258038 ~ 84,
    raw <= 2.7918184831 ~ 85,
    raw <= 2.8130560826 ~ 86,
    raw <= 2.8346703962 ~ 87,
    raw <= 2.8567884134 ~ 88,
    raw <= 2.8787142208 ~ 89,
    raw <= 2.9010274586 ~ 90,
    raw <= 2.9253225296 ~ 91,
    raw <= 2.9496725194 ~ 92,
    raw <= 2.9765579121 ~ 93,
    raw <= 3.0046060752 ~ 94,
    raw <= 3.0355958919 ~ 95,
    raw <= 3.0710265739 ~ 96,
    raw <= 3.1104036029 ~ 97,
    raw <= 3.1633656423 ~ 98,
    TRUE ~ 100)
  epts
}


