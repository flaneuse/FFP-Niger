/*-------------------------------------------------------------------------------
# Name:		02_HealthAnthro
# Purpose:	process anthropometric data for Niger 2014 LSSM
# Author:	Tim Essam, Ph.D.
# Created:	2017/09/11
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathlog/anthro.log", replace

* Change anthro data file to merge on children under 5
	use "$wave2/ecvma2_anthro.dta",
	clonevar rosterID = MS01Q0
	save "$pathout/ecvma2_anthro.dta", replace
	clear

* Load in anthro data to derive malnutrition statistics
	use "$wave2/ECVMA2_MS03P1.dta" 
	
* Cleaning up names for merging on anthro dataset (ecvma2_anthro.dta)
	rename PASSAGE GRAPPE MENAGE EXTENSION, lower
	clonevar rosterID = MS03Q00
	clonevar respondID = MS03Q

* Caputure health problems of children or household
	recode MS03Q01 (1 = 1 "Yes") (2 = 0 "No"), gen(healthProb4wks) 
	recode MS03Q02 (1 = 1 "Fever/Malaria")(2 = 2 "Diarrhea")(3 = 3 "Accident")/*
	*/(4 = 4 "Mouth")(5 = 5 "Skin")(6 = 6 "Eyes")(7 = 7 "Tension")(8 = 8 "Typhoid")/*
	*/(9 = 9 "Ulcer")(10 = 10 "Ear, Nose, Throat")(11 = 11 "Diabetes")(12 =12  "Menegitis")/*
	*/(13 = 13 "Other"), gen(healthProbType)
	
* Creating under 5 flag to capture correct observations
	clonevar under5Flag = MS03Q52
	clonevar ageMonths 	= MS03Q53
	clonevar oedema 	= MS03Q56
	clonevar measured	= MS03Q57

* Notice the lumpiness around 3 year and 4 year cutoffs. Bunching here.
	tab ageMonths if under5Flag
	
* Skipping vaccinations for now, can go back and add in as needed.
	
* Creating under 2 year vars -- crosswalk between months and their flag is not accurate
	recode MS03Q64 (1 = 1 "Yes")(2 = 0 "No"), gen(under2Flag)
	recode MS03Q65 (1 = 1 "Yes")(2 = 0 "No"), gen(diarrheaUnd2)
	recode MS03Q67 (1 = 1 "Yes")(2 = 0 "No"), gen(breastFeeding)
	clonevar ageStopBFeed	= MS03Q68
	
* Flagging ageMonths with under2s taht do not match. Still some at intersection that are questionable.
	tab ageMonths under2Flag, mi
	g byte under2FlagProblem = (ageMonth < 24 & under2Flag == 0) | /*
		*/ (ageMonth > 24 & under2Flag  == 1 & ageMonth != .)

* keep only for the kiddos first
*preserve
	keep if under5Flag == 1
	ds (MS03Q01- MS03Q73), not
	keep `r(varlist)'

	* Merge in child anthro data from WB
	merge 1:1 grappe menage extension rosterID using "$pathout/ecvma2_anthro.dta", gen(_merge)
	drop if _merge == 2
	
	* Check age in months calcuations between data sets
	*assert MS03Q53 == ageMonths
	g byte flagAgeMatch = (MS03Q53 == ageMonths)
	la var flagAgeMatch "flag on whether ages match across datasets"
	
	* Merge in hh_roster data
	merge 1:1 grappe menage extension rosterID using "$pathout/hh_roster.dta", gen(_mergeHH)
	drop if _mergeHH == 2
	
	* Check range of values before running Zscore06 package
	sum ageMonths 
	
	
* Required inputs
	/* 	a = age in months
		s = sex, boys == 1 & girls coded as 2
		h = Length/height in centimeters
		w = weight in kg	*/
						
	
	* Calculate z-scores using zscore06 package
	zscore06, a(ageMonths) s(sex) h(w2_08) w(w2_07) measure(w2_09) female(0)

* Remove scores that are implausible
	replace haz06=. if haz06<-6 | haz06>6
	replace waz06=. if waz06<-6 | waz06>5
	replace whz06=. if whz06<-5 | whz06>5
	replace bmiz06=. if bmiz06<-5 | bmiz06>5

	
	
	
	
	save "$pathout/HealthAnthroKids.dta", replace
*restore
	
	
	
	
* Stopping here w/ health module processing, but can return to grab hh vars.

	
	
