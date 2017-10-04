/*-------------------------------------------------------------------------------
# Name:		01_ProcessChildMalnutrition
# Purpose:	Create series of folders Food Rwanda Stunting Analysis
# Author:	Tim Essam, Ph.D.
# Created:	2017/10/04
# Owner:	USAID GeoCenter | OakStream Systems, LLC
# License:	MIT License
# Ado(s):	see below
#-------------------------------------------------------------------------------
*/

clear
capture log close
log using "$pathlog/01_ProcessChildMalnutrition.txt", replace

use "$pathin/emc2014_p4_anthropometrie_27022015.dta"

/* Assumptions made:
  Passage = 4 is the common wave and time frame used to survey households
  Merging on:  zd menage numind passage
  Child's age: assuming calculated values are correct until time permits verifying
*/

* Check age values for validity,  retain only eligible children and check for missing ids
  g byte ageCheck = (ANT04 <= 59)
  drop if ageCheck == 0
  tab numind, mi
  
* Create valid anthropometric indicators
  clonevar childsWeight = ANT11A
  clonevar childsHeight = ANT11B
  clonevar ageMonths = ANT04
  
* A few observations appear to have height/weight values that are missing
  sum childs* if ageCheck, d
   
**** NOTE: Turns out the regions are mislabeled. Drop these to ensure you are creating
**** statistics over the correction regions ***  
  label list region
  tab region, mi
  label drop region
  label drop milieu
  drop region milieu  
  
* Merge in household weights
/* Assumptions: there does not exist a similar women's weight as in the DHS. 
  Using guidance from the Bank we will multiply the household weight by the 
  number of children under 5 so as not to overweight the households with only
  one child. Will run sensitivity analysis with / without weights to make sure
  this is not too crazy of an idea. 
*/
 
* Generate household id variable by concatenating (zd * 1000) + menage 
  gen hhid = .
  replace hhid = (zd * 1000) + menage

  merge m:1 hhid using "$pathin/emc2014_welfare.dta", gen(_hhid_merge)
  keep if _hhid_merge == 3 
 
* Check the region variable to see that it maps somewhat into the welfare region stats
* around 6500 hh in the dataframe, and around 11000 children 
  bys hhid: gen id = _n
  tab region if id == 1, mi
 
* Merge in household roster data to determine gender of child (Matches 11544 / 11587)
  clonevar passage = A6
  isid zd menage numind passage
  merge 1:1 zd menage numind passage using "$pathin/emc2014_p4_individu_27022015.dta"
  keep if _merge == 3


* Revalue sex variables 
  recode B2 (2 = 0 "girl")(1 = 1 "boy"), gen(sex)
  
* Required inputs
  /* 	
    a = age in months
	s = sex, boys == 1 & girls recoded as 0
	h = Length/height in centimeters
	w = weight in kg
	female(0) = indicates that females are coded by 0 not 2 (default)
  */
						
* Calculate z-scores using zscore06 package
  zscore06, a(ageMonths) s(sex) h(childsHeight) w(childsWeight) female(0)

* Remove scores that are implausible
  replace haz06=. if haz06<-6 | haz06>6
  replace waz06=. if waz06<-6 | waz06>5
  replace whz06=. if whz06<-5 | whz06>5
  replace bmiz06=. if bmiz06<-5 | bmiz06>5
	
  rename (haz06 waz06 whz06 bmiz06) (stunting underweight wasting BMI)

  g byte stunted = stunting < -2 if stunting != .
  g byte underwgt = underweight < -2 if underweight != . 
  g byte wasted = wasting < -2 if wasting != . 
  g byte BMIed = BMI <-2 if BMI ~= . 
  la var stunted "Child is stunted"
  la var underwgt "Child is underweight for age"
  la var wasted "Child is wasted"

* Calculate weight for households using the # of children as a multiplier
  bys hhid: gen totChildren = _N
  g cweight = hhweight * totChildren

  svyset grappe [pweight=hhweight], strata(strate) singleunit(centered) 
  svy:mean stunted, over(region milieu)
  
  g lnpcexp = ln(pcexp)
  
  la var totChildren "total children in household"
  la var cweight "houshold weight X total children"
  la var lnpcexp "logged per capita income"

* Export cut of data for plotting in R
  ds(ANT* B*), not
  keep `r(varlist)'
 
  
  


