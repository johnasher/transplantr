#' HLA mismatch level
#'
#' Vectorised function to calculate HLA mismatch level as used in UK national deceased
#' donor kidney matching scheme.
#'
#' Mismatches should be provided as numeric vectors of integers with values from 0 to 2
#'
#' @param a numeric vector of HLA A locus mismatches (0-2)
#' @param b numeric vector of HLA B locus mismatches (0-2)
#' @param dr numeric vector of HLA DR locus mismatches (0-2)
#'
#' @return numeric vector of HLA mismatch levels (1-4)
#' @export
#'
#' @examples
#' hla_mm_level(a = 0, b = 1, dr = 1)
hla_mm_level = function(a, b, dr) {
  mm = ifelse(a + b + dr == 0, 1,
              ifelse(dr == 2, 4,
                     ifelse(b + dr > 2, 4,
                            ifelse(b + dr == 2, 3, 2))))
  mm
}


#' HLA mismatch level from string
#'
#' Vectorised function to calculate HLA mismatch levels where the HLA A, B and DR mismatch is
#' recorded as a string rather than as separate numeric values, e.g. "1:0:1" or "101". The
#' function calculates the mismatch level as used in the UK national deceased donor kidney
#' matching scheme. By default, the function assumes a single separator character is used
#' between each of the three numbers in the mismatch; if not, set the sep parameter to FALSE.
#' This function needs the stringr package to be installed.
#'
#' @param mm character string vector of HLA mismatches, e.g. "1:1:0" or "211"
#' @param sep logical to indicate whether separator used in the HLA mismatch strings (default TRUE)
#'
#' @return numeric vector of HLA mismatch levels
#' @export
#'
#' @examples
#' # using string of HLA mismatches with colons
#' hla_mm_level_str("1:1:0")
#'
#' # using string of HLA mismatches without separator
#' hla_mm_level_str("211", sep = FALSE)
hla_mm_level_str = function(mm, sep = TRUE) {
  if(!requireNamespace("stringr", quietly = TRUE)) {
    stop("The stringr package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (sep) {
    a = stringr::str_sub(mm, 1, 1)
    b = stringr::str_sub(mm, 3, 3)
    dr = stringr::str_sub(mm, 5, 5)
  } else {
    a = stringr::str_sub(mm, 1, 1)
    b = stringr::str_sub(mm, 2, 2)
    dr = stringr::str_sub(mm, 3, 3)
  }
  hla_mm_level(as.numeric(a), as.numeric(b), as.numeric(dr))
}
