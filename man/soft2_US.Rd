% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/liver_scores.R
\name{soft2_US}
\alias{soft2_US}
\title{SOFT score from P-SOFT (US units)}
\usage{
soft2_US(PSoft, PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT)
}
\arguments{
\item{PSoft}{numeric vector of P-SOFT scores}

\item{PortalBleed}{numeric vector of whether portal bleeding in 48 hours pre-transplant (1 = "yes", 0 = "no")}

\item{DonorAge}{numeric vector of donor ages in years}

\item{DonorCVA}{numeric vector of whether donor cause of death is CVA/stroke (1 = "yes", 0 = "no")}

\item{DonorSCr}{numeric vector of donor terminal serum creatinine in mg/dl}

\item{National}{numeric vector of whether national allocation (1 = "yes", 0 = "no")}

\item{CIT}{numeric vector of cold ischaemic time in hours}
}
\value{
numeric vector of SOFT Scores
}
\description{
A wrapper using US units for the soft2() vectorised function to calculate SOFT Scores
for predicting patient survival after liver
transplantation when the P-SOFT score is already known. The P-SOFT Score can be calculated using
the transplantr::p_soft() function. Alternatively, the SOFT Score can be calculated in full,
including the P-SOFT parameters using the transplantr::soft() or transplantr::soft_US() function.
The units for donor serum creatinine are in mg/dl
}
\details{
Reference: Rana A, Hardy MA, Halazun KJ, et al. Survival Outcomes Following Liver Transplantation
(SOFT) Score: A Novel Method to Predict Patient Survival Following Liver Transplantation.
American Journal of Transplantation 2008; 8:2537-2546.
}
\examples{
soft2_US(PSoft = 4, PortalBleed = 0, DonorAge = 61, DonorCVA = 1, DonorSCr = 1.6,
    National = 1, CIT = 12) # 13
}
