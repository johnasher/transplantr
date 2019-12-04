#' Convert CHI number to date of birth
#'
#' A vectorised function to convert a Scottish CHI number to date of birth in
#' POSIXct date format. Note that this function does not always work as not all
#' CHI numbers correspond accurately to date of birth, and any person born
#' before 1920 will appear to be 100 years younger unless the optional cent
#' parameter set to TRUE. Childen born in or after 2020 will appear 100 years
#' older unless the optional paed parameter is set to TRUE. This function requires the
#' stringr package.
#'
#' The paed and cent parameters can either be provided as vectors for each case,
#' for example in a series where there are patients with dates of birth in both
#' the 1910s and 2010s, or alteratively can be set as a single TRUE or FALSE for
#' the whole series.
#'
#' @param chi vector of CHI numbers (as numeric or string)
#' @param paed Whether paediatric patient (TRUE/FALSE), either a vector or a
#'   single TRUE/FALSE for whole series
#' @param cent Whether born before 1920 (TRUE/FALSE), either a vector or a
#'   single TRUE/FALSE for whole series
#'
#' @return a vector of POSIXct dates
#' @export
#'
#' @examples
#' # as a single numeric
#' chi2dob(1503541234)
#'
#' # as a single character string, for a patient born in 1919
#' chi2dob("1108191234", cent = TRUE)
#'
#' # as a mixed vector of adults and children, including one born in 1919
#' chi2dob(chi = c("1503541234", "1108191234", "0510141234"),
#'         cent = c(FALSE, TRUE, FALSE))
chi2dob = function(chi, paed = FALSE, cent = FALSE) {
  if(!requireNamespace("stringr", quietly = TRUE)) {
    stop("The stringr package is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  chi = stringr::str_pad(chi, width = 10, side = "left", pad = "0")
  d = stringr::str_sub(chi, 1, 2)
  m = stringr::str_sub(chi, 3, 4)
  y = stringr::str_sub(chi, 5, 6)
  y2 = ifelse(as.numeric(y) < 20, "20", "19")

  ## correction factor for paediatric recipients
  if (length(paed) == 1 && paed == TRUE) {
    y2 = "20"
  } else if (length(paed) > 1) {
    y2 = ifelse(paed == TRUE, "20", y2)
  }

  ## correction factor for centenarians
  if (length(cent) == 1 && cent == TRUE) {
    y2 = "19"
  } else if (length(cent) > 1) {
    y2 = ifelse(cent == TRUE, "19", y2)
  }

  dob = stringr::str_c(y2, y, "-", m, "-", d)
  as.POSIXct(dob)
}
