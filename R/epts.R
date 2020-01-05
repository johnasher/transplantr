#' Estimated Post-Transplant Survival Score calculator
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
