#' Bilirubin unit converter (µmol/l -> mg/dl)
#'
#' A vectorised function to convert serum bilirubin levels from µmol/l to mg/dl
#'
#' @param bili numeric vector of bilirubin levels (µmol/l)
#'
#' @return numeric vector of bilirubin levels in mg/dl
#' @export
#'
#' @examples
#' bilirubin_to_US(bili = 54)
bilirubin_to_US = function(bili) {
  bili / 17.1
}

#' Bilirubin unit converter (mg/dl -> µmol/l)
#'
#' A vectorised function to convert serum bilirubin levels from mg/dl to µmol/l
#'
#' @param bili numeric vector of bilirubin levels (mg/dl)
#'
#' @return numeric vector of bilirubin levels in µmol/l
#' @export
#'
#' @examples
#' bilirubin_to_SI(bili = 3.1)
bilirubin_to_SI = function(bili) {
  bili * 17.1
}

#' Creatinine unit converter (µmol/l -> mg/dl)
#'
#' A vectorised function to convert serum creatinine levels from µmol/l to mg/dl
#'
#' @param creat numeric vector of creatinine levels (µmol/l)
#'
#' @return numeric vector of creatinine levels in mg/dl
#' @export
#'
#' @examples
#' creatinine_to_US(creat = 176)
creatinine_to_US = function(creat) {
  creat / 88.4
}

#' Creatinine unit converter (mg/dl -> µmol/l)
#'
#' A vectorised function to convert serum creatinine levels from mg/dl to µmol/l
#'
#' @param creat numeric vector of creatinine levels (mg/dl)
#'
#' @return numeric vector of creatinine levels in µmol/l
#' @export
#'
#' @examples
#' creatinine_to_SI(creat = 2.0)
creatinine_to_SI = function(creat) {
  creat * 88.4
}

#' Convert urea to BUN
#'
#' A vectorised function to convert urea to blood urea nitrogen (BUN),
#' By default the urea is measured in mmol/l but this can be changed to mg/dl by
#' setting the optional units parameter to "US"
#'
#' @param urea numeric vector of urea levels (mmol/l by default)
#' @param units units for urea ("SI" for mmol/l, "US" for mg/dl)
#'
#' @return numeric vector of blood urea nitrogen (BUN) levels in mg/dl
#' @export
#'
#' @examples
#' urea_to_bun(5.4)
urea_to_bun = function(urea, units = "SI") {
  if (units == "SI") {
    urea / 0.3571
  } else {
    urea / 2.14
  }
}

#' Convert BUN to urea
#'
#' A vectorised function to convert blood urea nitrogen (BUN) to urea.
#' The default unit for urea is mmol/l but this can be changed to mg/dl by
#' setting the optional units parameter to "US"
#'
#' @param BUN numeric vector of blood urea nitrogen levels (mg/dl)
#' @param units units for urea ("SI" for mmol/l, "US" for mg/dl)
#'
#' @return numeric vector of urea levels
#' @export
#'
#' @examples
#' bun_to_urea(8.0)
bun_to_urea = function(BUN, units = "SI") {
  if (units == "SI") {
    BUN * 0.3571
  } else {
    BUN * 2.14
  }
}
