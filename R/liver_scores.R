#' Eurotransplant Donor Risk Index in Liver Transplantation (ET-DRI)
#'
#' A vectorised function to calculate the Eurotransplant Donor Risk Index for liver transplantation.
#' The ET-DRI is a variant of the American DRI published by Feng et al but adapted to the European
#' population. The American liver DRI is available using the transplantr::liver_dri() function.
#'
#' Reference: Braat AE, Blok JJ, Putter H, et al. The Eurotransplant Donor Risk Index in Liver
#' Transplantation: ET-DRI. American Journal of Transplantation 2012; 12:2789–2796.
#'
#' @param age numeric vector of patient ages in years
#' @param cod character string vector of donor causes of death, one of "trauma", "anoxia", "cva" or "other"
#' @param dcd numeric vector of whether DCD (1 = yes, 0 = no)
#' @param split numeric vector of whether liver split (1 = yes, 0 = no)
#' @param share character string vector of type of sharing, one of "local", regional" or "national"
#' @param cit numeric vector of cold ischaemic times in hours
#' @param ggt numeric vector of last pre-transplant serum gamma-GT level in IU/l
#' @param rescue numeric vector of whether rescue transplant (1 = yes, 0 = no)
#'
#' @return numeric vector of ET-DRI scores
#' @export
#'
#' @examples
#' et_dri(age = 39, cod = "trauma", dcd = 0, split = 0, share = "local",
#'     cit = 8, ggt = 50, rescue = 0) # 1.00
#'
#' et_dri(age = 25, cod = "cva", dcd = 0, split = 0, share = "local",
#'     cit = 8, ggt = 50, rescue = 0) # 1.15
et_dri <- function(age, cod, dcd, split, share, cit, ggt, rescue) {
	agevar = ifelse(age < 40, 0,
			ifelse(age < 50, 0.154,
			ifelse(age < 60, 0.274,
			ifelse(age < 70, 0.424, 0.501))))
	codvar = ifelse(cod == "anoxia", 0.079,
			ifelse(cod == "cva", 0.145, ifelse(cod == "trauma", 0, 0.184)))
	dcdvar = 0.411 * dcd
	splitvar = 0.422 * split
	sharevar = ifelse(share == "local", 0, ifelse(share == "regional", 0.105, 0.244))
	citvar = 0.010 * (cit - 8)
	ggtvar = (ggt - 50) / 100
	rescuevar = 0.180 * rescue

	exp(0.960 * (agevar + codvar + citvar + sharevar) + dcdvar + splitvar + 0.06 * ggtvar + rescuevar)
}

#' Liver Donor Risk Index (DRI)
#'
#' A vectorised function to calculate the Liver Donor Risk Index as published by Feng and others.
#'
#' Reference: Feng S, Goodrich NP, Bragg-Gresham JL et al. Characteristics Associated with
#' Liver Graft Failre: The Concept of a Donor Risk Index.
#' American Journal of Transplantation 2006; 6:783-790.
#'
#' @param age numeric vector of patient ages in years
#' @param cod character string vector of donor causes of death, one of "trauma", "anoxia", "cva" or "other"
#' @param eth character string vector of ethnicity, one of "black", "white" or "other"
#' @param dcd numeric vector of whether DCD (1 = yes, 0 = no)
#' @param split numeric vector of whether liver split (1 = yes, 0 = no)
#' @param share character string vector of type of sharing, one of "regional" or "national"
#' @param cit numeric vector of cold ischaemic times in hours
#' @param height numeric vector of patient heights in cm
#'
#' @return numeric vector of liver DRI values
#' @export
#'
#' @examples
#' liver_dri(age = 25, cod = "trauma", eth = "white", dcd = 0, split = 0,
#'     share = "local", cit = 8, height = 170) # 1.00
#'
#' liver_dri(age = 64, cod = "cva", eth = "white", dcd = 0, split = 0,
#'     share = "local", cit = 14, height = 170) # 1.88
liver_dri <- function(age, cod, eth, dcd, split, share, cit, height) {
	agevar = ifelse(age < 40, 0,
			        ifelse(age < 50, 0.154,
			        ifelse(age < 60, 0.274,
			        ifelse(age < 70, 0.424, 0.501))))
	codvar = ifelse(cod == "anoxia", 0.079,
			        ifelse(cod == "cva", 0.145, ifelse(cod == "trauma", 0, 0.184)))
	racevar = ifelse(eth == "black", 0.176, ifelse(eth == "white", 0, 0.126))
	dcdvar = 0.411 * dcd
	splitvar = 0.422 * split
	sharevar = ifelse(share == "local", 0, ifelse(share == "regional", 0.105, 0.244))
	citvar = 0.010 * (cit - 8)
	heightvar = 0.066 * (170 - height) / 10

	exp(agevar + codvar + racevar + dcdvar + splitvar + citvar + heightvar)
}

#' P-SOFT Score
#'
#' A vectorised function to calculate the pre-procurement component of the SOFT score used to
#' predict patient survival after liver transplantation. The function needs the MELD score as one of
#' its inputs - this is available using the transplantr::meld() function.
#' The units for albumin are g/l but this
#' is changed to g/dl if the optional Units parameter is set to "US"
#'
#' Reference: Rana A, Hardy MA, Halazun KJ, et al. Survival Outcomes Following Liver Transplantation
#' (SOFT) Score: A Novel Method to Predict Patient Survival Following Liver Transplantation.
#' American Journal of Transplantation 2008; 8:2537-2546.
#'
#' @param Age numeric vector of patient ages in years
#' @param BMI numeric vector of patient BMI in kg/m2
#' @param PrevTx numeric vector of number of previous transplants
#' @param AbdoSurg numeric vector of whether previous abdominal surgery (1 = "yes", 0 = "no")
#' @param Albumin numeric vector of serum albumin in g/l
#' @param Dx numeric vector of whether on dialysis before transplant (1 = "yes", 0 = "no")
#' @param ICU numeric vector of whether patients in intensive care unit before transplant (1 = "yes", 0 = "no")
#' @param Admitted numeric vector of whether admitted to hospital pre-transplant (1 = "yes", 0 = "no")
#' @param MELD numeric vector of MELD scores
#' @param LifeSupport numeric vector of whether on life support pre-transplant (1 = "yes", 0 = "no")
#' @param Encephalopathy numeric vector of whether encephalopathy present (1 = "yes", 0 = "no")
#' @param PVThrombosis numeric vector of whether portal vein thrombosis (1 = "yes", 0 = "no")
#' @param Ascites numeric vector of whether ascites pre-transplant (1 = "yes", 0 = "no")
#' @param Units optional scalar for albumin units (one of "SI" for g/l, "US" for g/dl)
#'
#' @return numeric vector of P-SOFT scores
#' @export
#'
#' @examples
#' p_soft(Age = 65, BMI = 36, PrevTx = 2, AbdoSurg = 1, Albumin = 29, Dx = 0,
#'     ICU = 0, Admitted = 1, MELD = 32, LifeSupport = 0, Encephalopathy = 1,
#'     PVThrombosis = 1, Ascites = 1) # 37
p_soft <- function(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted, MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites, Units = "SI") {
	agepts = ifelse(Age > 60, 4, 0)
	bmipts = ifelse(BMI > 35, 2, 0)
	txpts = ifelse(PrevTx == 0, 0, ifelse(PrevTx == 1, 9, 14))
	if (Units == "SI") {
	  Albumin = Albumin / 10
	  }
	albpts = ifelse(Albumin < 2.0, 2, 0)
	meldpts = ifelse(MELD > 30, 4, 0)
	psoft = agepts + bmipts + txpts + albpts + meldpts +
			3 * Dx + 6 * ICU + 3 * Admitted +
			9 * LifeSupport + 2 * Encephalopathy +
			5 * PVThrombosis + 3 * Ascites
}

#' P-SOFT Score (US units)
#'
#' A wrapper for the p_soft() vectorised function to calculate the pre-procurement component of the SOFT score used to
#' predict patient survival after liver transplantation. The function needs the MELD score as one of
#' its inputs - this is available using the transplantr::meld() function.
#' The units for albumin are g/dl (rather than g/l in p_soft() function)
#'
#' Reference: Rana A, Hardy MA, Halazun KJ, et al. Survival Outcomes Following Liver Transplantation
#' (SOFT) Score: A Novel Method to Predict Patient Survival Following Liver Transplantation.
#' American Journal of Transplantation 2008; 8:2537-2546.
#'
#' @param Age numeric vector of patient ages in years
#' @param BMI numeric vector of patient BMI in kg/m2
#' @param PrevTx numeric vector of number of previous transplants
#' @param AbdoSurg numeric vector of whether previous abdominal surgery (1 = "yes", 0 = "no")
#' @param Albumin numeric vector of serum albumin in g/dl
#' @param Dx numeric vector of whether on dialysis before transplant (1 = "yes", 0 = "no")
#' @param ICU numeric vector of whether patients in intensive care unit before transplant (1 = "yes", 0 = "no")
#' @param Admitted numeric vector of whether admitted to hospital pre-transplant (1 = "yes", 0 = "no")
#' @param MELD numeric vector of MELD scores
#' @param LifeSupport numeric vector of whether on life support pre-transplant (1 = "yes", 0 = "no")
#' @param Encephalopathy numeric vector of whether encephalopathy present (1 = "yes", 0 = "no")
#' @param PVThrombosis numeric vector of whether portal vein thrombosis (1 = "yes", 0 = "no")
#' @param Ascites numeric vector of whether ascites pre-transplant (1 = "yes", 0 = "no")
#'
#' @return numeric vector of P-SOFT scores
#' @export
#'
#' @examples
#' p_soft_US(Age = 65, BMI = 36, PrevTx = 2, AbdoSurg = 1, Albumin = 2.9,
#'     Dx = 0, ICU = 0, Admitted = 1, MELD = 32, LifeSupport = 0, Encephalopathy = 1,
#'     PVThrombosis = 1, Ascites = 1) # 37
p_soft_US = function(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted, MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites) {
  p_soft(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted, MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites, Units = "US")
}

#' SOFT score from P-SOFT
#'
#' A vectorised function to calculate SOFT Scores for predicting patient survival after liver
#' transplantation when the P-SOFT score is already known. The P-SOFT Score can be calculated using
#' the transplantr::p_soft() function. Alternatively, the SOFT Score can be calculated in full,
#' including the P-SOFT parameters using the transplantr::soft() function. The units for donor serum
#' creatinine are in µmol/l but can be changed to mg/dl by setting the Units parameter to "US".
#'
#' Reference: Rana A, Hardy MA, Halazun KJ, et al. Survival Outcomes Following Liver Transplantation
#' (SOFT) Score: A Novel Method to Predict Patient Survival Following Liver Transplantation.
#' American Journal of Transplantation 2008; 8:2537-2546.
#'
#' @param PSoft numeric vector of P-SOFT scores
#' @param PortalBleed numeric vector of whether portal bleeding in 48 hours pre-transplant (1 = "yes", 0 = "no")
#' @param DonorAge numeric vector of donor ages in years
#' @param DonorCVA numeric vector of whether donor cause of death is CVA/stroke (1 = "yes", 0 = "no")
#' @param DonorSCr numeric vector of donor terminal serum creatinine
#' @param National numeric vector of whether national allocation (1 = "yes", 0 = "no")
#' @param CIT numeric vector of cold ischaemic time in hours
#' @param Units units to use for creatinine, "SI" (default) for µmol/l, "US" for mg/dl
#'
#' @return numeric vector of SOFT Scores
#' @export
#'
#' @examples
#' soft2(PSoft = 4, PortalBleed = 0, DonorAge = 61, DonorCVA = 1, DonorSCr = 140,
#'     National = 1, CIT = 12) # 13
soft2 <- function(PSoft, PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT, Units = "SI") {
	dagepts = ifelse(DonorAge >= 10 && DonorAge <= 20, -2,
			ifelse(DonorAge > 60, 3, 0))
	if (Units == "SI") {
	  DonorSCr = DonorSCr / 88.4
	}
	dcrpts = ifelse(DonorSCr > 1.5, 2, 0)
	citpts = ifelse(CIT < 6, -3, 0)
	PSoft + 6 * PortalBleed + dagepts + 2 * DonorCVA + dcrpts +
		2 * National + citpts
}

#' SOFT score from P-SOFT (US units)
#'
#' A wrapper using US units for the soft2() vectorised function to calculate SOFT Scores
#' for predicting patient survival after liver
#' transplantation when the P-SOFT score is already known. The P-SOFT Score can be calculated using
#' the transplantr::p_soft() function. Alternatively, the SOFT Score can be calculated in full,
#' including the P-SOFT parameters using the transplantr::soft() or transplantr::soft_US() function.
#' The units for donor serum creatinine are in mg/dl
#'
#' Reference: Rana A, Hardy MA, Halazun KJ, et al. Survival Outcomes Following Liver Transplantation
#' (SOFT) Score: A Novel Method to Predict Patient Survival Following Liver Transplantation.
#' American Journal of Transplantation 2008; 8:2537-2546.
#'
#' @param PSoft numeric vector of P-SOFT scores
#' @param PortalBleed numeric vector of whether portal bleeding in 48 hours pre-transplant (1 = "yes", 0 = "no")
#' @param DonorAge numeric vector of donor ages in years
#' @param DonorCVA numeric vector of whether donor cause of death is CVA/stroke (1 = "yes", 0 = "no")
#' @param DonorSCr numeric vector of donor terminal serum creatinine in mg/dl
#' @param National numeric vector of whether national allocation (1 = "yes", 0 = "no")
#' @param CIT numeric vector of cold ischaemic time in hours
#'
#' @return numeric vector of SOFT Scores
#' @export
#'
#' @examples
#' soft2_US(PSoft = 4, PortalBleed = 0, DonorAge = 61, DonorCVA = 1, DonorSCr = 1.6,
#'     National = 1, CIT = 12) # 13
soft2_US = function(PSoft, PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT) {
  soft2(PSoft, PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT, Units = "US")
}

#' SOFT score (Survival Outcomes Following Liver Transplantation)
#'
#' A vectorised function to calculate SOFT Scores for predicting patient survival after liver
#' transplantation The units for donor serum creatinine are in µmol/l
#' and recipient serum albumin in g/l but they can be changed to mg/dl and g/dl respectively
#' by setting the Units parameter to "US".
#'
#' Reference: Rana A, Hardy MA, Halazun KJ, et al. Survival Outcomes Following Liver Transplantation
#' (SOFT) Score: A Novel Method to Predict Patient Survival Following Liver Transplantation.
#' American Journal of Transplantation 2008; 8:2537-2546.
#'
#' @param Age numeric vector of patient ages in years
#' @param BMI numeric vector of patient BMI in kg/m2
#' @param PrevTx numeric vector of number of previous transplants
#' @param AbdoSurg numeric vector of whether previous abdominal surgery (1 = "yes", 0 = "no")
#' @param Albumin numeric vector of serum albumin in g/l
#' @param Dx numeric vector of whether on dialysis before transplant (1 = "yes", 0 = "no")
#' @param ICU numeric vector of whether patients in intensive care unit before transplant (1 = "yes", 0 = "no")
#' @param Admitted numeric vector of whether admitted to hospital pre-transplant (1 = "yes", 0 = "no")
#' @param MELD numeric vector of MELD scores
#' @param LifeSupport numeric vector of whether on life support pre-transplant (1 = "yes", 0 = "no")
#' @param Encephalopathy numeric vector of whether encephalopathy present (1 = "yes", 0 = "no")
#' @param PVThrombosis numeric vector of whether portal vein thrombosis (1 = "yes", 0 = "no")
#' @param Ascites numeric vector of whether ascites pre-transplant (1 = "yes", 0 = "no")
#' @param PortalBleed numeric vector of whether portal bleeding in 48 hours pre-transplant (1 = "yes", 0 = "no")
#' @param DonorAge numeric vector of donor ages in years
#' @param DonorCVA numeric vector of whether donor cause of death is CVA/stroke (1 = "yes", 0 = "no")
#' @param DonorSCr numeric vector of donor terminal serum creatinine
#' @param National numeric vector of whether national allocation (1 = "yes", 0 = "no")
#' @param CIT numeric vector of cold ischaemic time in hours
#' @param Units units to use for creatinine and albumin, "SI" (default) for µmol/l and g/l, "US" for mg/dl and g/dl
#'
#' @return numeric vector of SOFT Scores
#' @export
#'
#' @examples
#' soft(Age = 35, BMI = 20, PrevTx = 0, AbdoSurg = 1, Albumin = 30, Dx = 0,
#'     ICU = 0, Admitted = 0, MELD = 29, LifeSupport = 0, Encephalopathy = 1,
#'     PVThrombosis = 0, Ascites = 1, PortalBleed = 0, DonorAge = 44, DonorCVA = 0,
#'     DonorSCr = 110, National = 0, CIT = 8) # 7
soft <- function(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted,
			MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites,
			PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT, Units = "SI") {
  # convert units if needed
  if (Units == "SI") {
    Albumin = Albumin / 10
    DonorSCr = DonorSCr / 88.4
  }
  # calculate P-SOFT
	ps = p_soft(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted,
			MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites)
	# calculate SOFT
	soft2(ps, PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT)
}

#' SOFT score (Survival Outcomes Following Liver Transplantation) (US units)
#'
#' A wrapper function using US units for the soft() vectorised function to calculate SOFT Scores
#' for predicting patient survival after liver
#' transplantation. The units for donor serum creatinine and recipient serum albumin in g/l.
#'
#' Reference: Rana A, Hardy MA, Halazun KJ, et al. Survival Outcomes Following Liver Transplantation
#' (SOFT) Score: A Novel Method to Predict Patient Survival Following Liver Transplantation.
#' American Journal of Transplantation 2008; 8:2537-2546.
#'
#' @param Age numeric vector of patient ages in years
#' @param BMI numeric vector of patient BMI in kg/m2
#' @param PrevTx numeric vector of number of previous transplants
#' @param AbdoSurg numeric vector of whether previous abdominal surgery (1 = "yes", 0 = "no")
#' @param Albumin numeric vector of serum albumin in g/dl
#' @param Dx numeric vector of whether on dialysis before transplant (1 = "yes", 0 = "no")
#' @param ICU numeric vector of whether patients in intensive care unit before transplant (1 = "yes", 0 = "no")
#' @param Admitted numeric vector of whether admitted to hospital pre-transplant (1 = "yes", 0 = "no")
#' @param MELD numeric vector of MELD scores
#' @param LifeSupport numeric vector of whether on life support pre-transplant (1 = "yes", 0 = "no")
#' @param Encephalopathy numeric vector of whether encephalopathy present (1 = "yes", 0 = "no")
#' @param PVThrombosis numeric vector of whether portal vein thrombosis (1 = "yes", 0 = "no")
#' @param Ascites numeric vector of whether ascites pre-transplant (1 = "yes", 0 = "no")
#' @param PortalBleed numeric vector of whether portal bleeding in 48 hours pre-transplant (1 = "yes", 0 = "no")
#' @param DonorAge numeric vector of donor ages in years
#' @param DonorCVA numeric vector of whether donor cause of death is CVA/stroke (1 = "yes", 0 = "no")
#' @param DonorSCr numeric vector of donor terminal serum creatinine in mg/dl
#' @param National numeric vector of whether national allocation (1 = "yes", 0 = "no")
#' @param CIT numeric vector of cold ischaemic time in hours
#'
#' @return numeric vector of SOFT Scores
#' @export
#'
#' @examples
#' soft_US(Age = 35, BMI = 20, PrevTx = 0, AbdoSurg = 1, Albumin = 3.0, Dx = 0,
#'     ICU = 0, Admitted = 0, MELD = 29, LifeSupport = 0, Encephalopathy = 1,
#'     PVThrombosis = 0, Ascites = 1, PortalBleed = 0, DonorAge = 44, DonorCVA = 0,
#'     DonorSCr = 1.2, National = 0, CIT = 8) # 7
soft_US = function(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted,
                   MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites,
                   PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT) {
  soft(Age, BMI, PrevTx, AbdoSurg, Albumin, Dx, ICU, Admitted,
       MELD, LifeSupport, Encephalopathy, PVThrombosis, Ascites,
       PortalBleed, DonorAge, DonorCVA, DonorSCr, National, CIT, Units = "US")
}

#' Pedi-SOFT Score
#'
#' A vectorised function to calculate the Pedi-SOFT score used to predict survival after
#' liver transplantation in children.
#'
#' Reference: Rana A, Pallister ZS, Guiteau JJ, et al.
#' Survival Outcomes Following Pediatric Liver Transpantation (Pedi-SOFT) Score:
#' A Novel Predictive Index.
#' American Journal of Transplantation 2015; 15:1855-1863.
#'
#' @param CTVG numeric vector of whether cadaveric technical variant graft (1 for "yes", 0 for "no")
#' @param Weight numeric vector of recipient weight in kg
#' @param Dx numeric vector of whether on dialysis or creatinine clearance under 30 (1 for "yes", 0 for "no")
#' @param LifeSupport numeric vector of whether on life support pre-transplant (1 for "yes", 0 for "no")
#' @param PrevTx numeric vector of number of previous liver transplants
#'
#' @return numeric vector of Pedi-SOFT scores
#' @export
#'
#' @examples
#' pedi_soft(CTVG = 1, Weight = 10, Dx = 0, LifeSupport = 0, PrevTx = 0) # 4
pedi_soft = function(CTVG, Weight, Dx, LifeSupport, PrevTx) {
	weightpts = ifelse(Weight < 6.0, 6, 0)
	txpts = ifelse(PrevTx == 0, 0, ifelse(PrevTx == 1, 15, 49))
	weightpts + txpts + 4 * CTVG + 17 * Dx + 27 * LifeSupport
}

#' BAR (Balance of Risk) score in liver transplantation
#'
#' A vectorised function to calculate the BAR score to predict patient survival after liver
#' transplantation using a composite of donor and recipient factors.
#'
#' Reference: Dutkowski P, Oberkofler CE, Slankamenac K, et al. Are There Better Guidelines for
#' Allocation in Liver Transplantation? A Novel Score Targeting Justice and Utility in the
#' Model for End-Stage Liver Disease Era. Annals of Surgery 2011; 254:745-753.
#'
#' @param Age numeric vector of recipient ages in years
#' @param MELD numeric vector of MELD scores
#' @param ReTx numeric vector of whether retransplant (1 = "yes", 0 = "no")
#' @param LifeSupport numeric vector of whether on life support pre-transplant (1 = "yes", 0 = "no")
#' @param CIT numeric vector of cold ischaemic time in hours
#' @param DonorAge numeric vector of donor ages
#'
#' @return numeric vector of BAR scores
#' @export
#'
#' @examples
#' bar_score(Age = 63, MELD = 27, ReTx = 0, LifeSupport = 0, CIT = 9.5, DonorAge = 67)
bar_score = function(Age, MELD, ReTx, LifeSupport, CIT, DonorAge) {
  agepts = ifelse(Age > 60, 3, ifelse(Age > 40, 1, 0))
  meldpts = ifelse(MELD > 35, 14, ifelse(MELD > 25, 10, ifelse(MELD > 15, 5, 0)))
  citpts = ifelse(CIT > 12, 2, ifelse(CIT > 6, 1, 0))
  dagepts = ifelse(DonorAge > 60, 1, ifelse(DonorAge > 40, 1, 0))
  agepts + meldpts + citpts + dagepts + 4 * ReTx + 3 * LifeSupport
}
