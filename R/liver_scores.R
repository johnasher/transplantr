#' Eurotransplant Donor Risk Index in Liver Transplantation (ET-DRI)
#'
#' A vectorised function to calculate the Eurotransplant Donor Risk Index for liver transplantation.
#' The ET-DRI is a variant of the American DRI published by Feng et al but adapted to the European
#' population. The American liver DRI is available using the transplantr::liver_dri() function.
#'
#' Reference: Braat AE, Blok JJ, Putter H, et al. The Eurotransplant Donor Risk Index in Liver
#' Transplantation: ET-DRI. American Journal of Transplantation 2012; 12: 2789–2796.
#'
#' @param Age numeric vector of patient ages in years
#' @param COD character string vector of donor causes of death, one of "anoxia", "cva" or "other"
#' @param DCD numeric vector of whether DCD (1 = yes, 0 = no)
#' @param Split numeric vector of whether liver split (1 = yes, 0 = no)
#' @param Share character string vector of type of sharing, one of "regional" or "national"
#' @param CIT numeric vector of cold ischaemic times in hours
#' @param GTT numeric vector of last pre-transplant serum gamma-GT level in IU/l
#' @param Rescue numeric vector of whether rescue transplant (1 = yes, 0 = no)
#'
#' @return numeric vector of ET-DRI scores
#' @export
#'
#' @examples
et_dri <- function(Age, COD, DCD, Split, Share, CIT, GTT, Rescue) {
	agevar = ifelse(Age < 40, 0,
			ifelse(Age < 50, 0.154,
			ifelse(Age < 60, 0.274,
			ifelse(Age < 70, 0.424, 0.501))))
	codvar = ifelse(COD == "anoxia", 0.079,
			ifelse(COD == "cva", 0.145, 0.184))
	dcdvar = 0.411 * DCD
	splitvar = 0.422 * Split
	sharevar = ifelse(Share == "regional", 0.105, 0.244)
	citvar = 0.010 * (CIT - 8)
	ggtvar = 0.06 * (GGT - 50) / 100
	rescuevar = 0.180 * Rescue

	exp(agevar + codvar + dcdvar + splitvar + sharevar +
			citvar + ggtvar + rescuevar)
}

#' Title
#'
#' @param Age
#' @param COD
#' @param Race
#' @param DCD
#' @param Split
#' @param Share
#' @param CIT
#' @param Height
#'
#' @return
#' @export
#'
#' @examples
liver_dri <- function(Age, COD, Race, DCD, Split, Share, CIT, Height) {
	agevar = ifelse(Age < 40, 0,
			ifelse(Age < 50, 0.154,
			ifelse(Age < 60, 0.274,
			ifelse(Age < 70, 0.424, 0.501))))
	codvar = ifelse(COD == "anoxia", 0.079,
			ifelse(COD == "cva", 0.145, 0.184))
	racevar = ifelse(Race == "black", 0.176, 0.126)
	dcdvar = 0.411 * DCD
	splitvar = 0.422 * Split
	sharevar = ifelse(Share == "regional", 0.105, 0.244)
	citvar = 0.010 * CIT
	heightvar = 0.066 * (170 - height) / 10
}

#P-SOFT score
p_soft <- function(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted, MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites) {
	agepts = ifelse(Age > 60, 4, 0)
	bmipts = ifelse(BMI > 35, 2, 0)
	txpts = ifelse(PrevTx == 0, 0, ifelse(PrevTx == 1, 9, 14))
	albpts = ifelse(Albumin < 2.0, 2, 0)
	meldpts = ifelse(MELD > 30, 4, 0)
	psoft = agepts + bmipts + txpts + albpts + meldpts +
			3 * Dx + 6 * ICU + 3 * Admitted +
			9 * LifeSupport + 2 * Encephalopathy +
			5 * PVThrombosis + 3 * Ascites
}

# SOFT score from P-SOFT
soft2 <- function(PSoft, PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT) {
	dagepts = ifelse(DonorAge >= 10 && DonorAge <= 20, -2,
			ifelse(DonorAge > 60, 3, 0))
	dcrpts = ifelse(DonorSCr > 1.5, 2, 0)
	citpts = ifelse(CIT < 6, -3, 0)
	PSoft + 6 * PortalBleed + dagepts + 2 * DonorCVA + dcrpts +
		2 * National + citpts
}

# SOFT score
soft <- function(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted,
			MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites,
			PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT) {
	Psoft = p_soft(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted,
			MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites)
	soft2(Psoft, PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT)
}

# Pedi-SOFT
pedi_soft = function(CTVG, Weight, Dx, LifeSupport, PrevTx) {
	weightpts = ifelse(Weight < 6, 4, 0)
	txpts = ifelse(PrevTx == 0, 0, ifelse(PrevTx == 1, 15, 49))
	weightpts + txpts + 4 * CTVG + 17 * Dx + 27 * LifeSupport
}
