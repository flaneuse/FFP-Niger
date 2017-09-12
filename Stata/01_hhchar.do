/*-------------------------------------------------------------------------------
# Name:		01_hhchar
# Purpose:	Process household data and create hh characteristic variables
# Author:	Tim Essam, Ph.D.
# Created:	2017/09/08
# Owner:	USAID GeoCenter | USDA/FAS
# License:	MIT License
#-------------------------------------------------------------------------------
*/
capture log close
log using "$pathlog/hhchar", replace

* Call in Socio-demographic characteristics and pull out useful info for stunting
	clear
	use "$wave2/ECVMA2_MS01P1.dta"
	
	rename PASSAGE GRAPPE MENAGE EXTENSION, lower	
	clonevar  rosterID = MS01Q0
	clonevar  respondID = MS01Q00A
	clonevar  tiesToHoh = MS01Q02
	recode MS01Q01 (1 = 1 "Male")(2 = 0 "Female"), gen(sex)
	clonevar ethnicity = MS01Q24
	
	* Birthday info
	clonevar dayOfBirth = MS01Q05J  
	clonevar dateOfBirth = MS01Q05
	clonevar ageYearsLastBday = MS01Q06A
	clonevar ageMonthsLastBday = MS01Q06B
	
* save for merging in with other files later
	keep passage grappe menage extension rosterID- ageMonthsLastBday
	
	save "$pathout/hh_roster.dta", replace
	







/* Call in using data for Socio-demographic characteristics
* Socio-demographic characteristics (HH 1) *
use "$wave1/ecvmaind_p1p2_en.dta", clear
set more off

/* -- KEEP ONLY HOUSEHOLD MEMBERS THAT ARE PRESENT --
	Drop household members who were away for more than 
	6 months out of the year --  */
	drop if ms01q19 <=6

* # Module contains questions from Section 1 - Section 4 #
* Create head of household variables
	g byte hoh = (ms01q02==1)
	la var hoh "Head of household"

	g byte femHead = (ms01q02==1 &  ms01q01==2)
	la var femHead "Female head of household"

	g ageHead = ms01q06a if hoh==1
	la var  ageHead "HoH age"

* Married or Single Hoh? 
* Any type of marriage (monogamous or polygamous)
	g byte marriedHead = (ms01q15==2 | ms01q15==3) & hoh==1
	la var marriedHead "married HoH"

	g byte widowHead = (ms01q15==4 & hoh==1)
	la var widowHead "widowed HoH"

	g byte singleHead = (marriedHead==0 & hoh==1)
	la var singleHead "single HoH"

* Polygamous marraige present in household
	g byte polygamous = (ms01q15==3)

* Create HH size variables (one for each wave)
	bysort hid: gen hhSize = _N if passage1==1 & visitor!=2
	bysort hid: gen hhSize2 = _N if passage2==2 & visitor!=2
	replace hhSize = hhSize2 if hhSize==.
	la var hhSize "Household size"
	drop hhSize2

* Create sex ratio for households
	g byte male = (ms01q01==1)
	g byte female = (ms01q01==2)
	la var male "male hh members"
	la var female "female hh members"

	egen msize = total(male), by(hid)
	la var msize "number of males in hh"

	egen fsize = total(female), by(hid)
	la var fsize "number of females in hh"

	g sexRatio =msize/fsize
	recode sexRatio (. = 0) if fsize==0
	la var sexRatio "Number of males divided by females in HH"


/* Create intl. HH dependency ratio (age ranges appropriate for Niger?)
# HH Dependecy Ratio = [(# people 0-14 + those 65+) / # people aged 15-64 ] * 100 # */
	g byte numDepRatio = (ms01q06a<15 | ms01q06a>64) & visitor!=2
	g byte demonDepRatio = numDepRatio!=1 & visitor!=2
	egen totNumDepRatio = total(numDepRatio), by(hid)
	egen totDenomDepRatio = total(demonDepRatio), by(hid)

* Check that numbers add to hhsize
	assert hhSize == totNumDepRatio+totDenomDepRatio
	g depRatio = (totNumDepRatio/totDenomDepRatio)*100 if totDenomDepRatio!=.
	recode depRatio (. = 0) if totDenomDepRatio==0
	la var depRatio "Dependency Ratio"

	drop numDepRatio demonDepRatio totNumDepRatio totDenomDepRatio

/* Household Labor Shares */
	g byte hhLabort = (ms01q06a>= 12 & ms01q06a<60)
	egen hhlabor = total(hhLabort), by(hid)
	la var hhlabor "hh labor age>11 & < 60"

	g byte mlabort = (ms01q06a>= 12 & ms01q06a<60 & ms01q01==1)
	egen mlabor = total(mlabort), by(hid)
	la var mlabor "hh male labor age>11 & <60"

	g byte flabort = (ms01q06a>= 12 & ms01q06a<60 & ms01q01==2)
	egen flabor = total(flabort), by(hid)
	la var flabor "hh female labor age>11 & <60"
	drop hhLabort mlabort flabort

* Male/Female labor share in hh
	g mlaborShare = mlabor/hhlabor
	recode mlaborShare (. = 0) if hhlabor == 0
	la var mlaborShare "share of working age males in hh"

	g flaborShare = flabor/hhlabor
	recode flaborShare (. = 0) if hhlabor == 0
	la var flaborShare "share of working age females in hh"

* Number of hh members under 15
	g byte under15t = ms01q06a<15
	egen under15 = total(under15t), by(hid)
	la var under15 "number of hh members under 15"
	egen under15male = total(under15t) if male==1, by(hid)
	la var under15male "number of hh male members under 15"
	recode under15male (. = 0) if under15male==.

* Number of hh members under 24
	g byte under24t = ms01q06a<24
	egen under24 = total(under24t), by(hid)
	egen under24male = total(under24t) if male==1, by(hid)
	recode under24male (. = 0) if under24male==.
	la var under24 "number of hh members under 24"
	la var under24male "number of hh male members under 24"

* HH share of members under 15/24
	g under15Share = under15/hhSize
	la var under15Share "share of hh members under 15"
	g under24Share = under24/hhSize
	la var under24Share "share of hh members under 24"

* Not a great metric b/c 73% of sample missing data on education
	g educHead = ms02q23 if hoh==1
* label dir //# To get label names
	label copy ms02q23code ed
	label define ed 0 "None" 1 "Koranic or Literacy", modify
	label values educHead ed
	la var educHead "HoH education"
	replace educHead = 0 if  ms02q04==4 & hoh==1 
	replace educHead = 1 if (ms02q04==2 | ms02q04==3) & hoh==1

* Look at HoH literacy (Can read & write)
	g byte literateHead = ( ms02q01==1 & ms02q02==1) & hoh==1
	la var literateHead "HoH is literate"

* Look at Hoh place of education
	g byte eduMuslimHead = (ms02q04==2) & hoh==1
	la var eduMuslimHead "Hoh has Koranic education"

	g byte eduAnyHead = (ms02q04!=4) & hoh==1
	label define ed2 0 "no school" 1 "some school"

	label values eduAnyHead ed2 
	la var eduAnyHead "Hoh has any education"

* Education for individuals *
* No education listed
	g edu=0 if ms02q04==4
	la var edu "Education levels"
* Koranic School (not sure how it ranks)
	replace edu = 1 if ms02q04==2
* Primary
	replace edu = 2 if ms02q23<=2 
* Secondary first cycle
	replace edu = 3 if ms02q23==3 | ms02q23==4 
* Secondary second cycle +
	replace edu = 4 if ms02q23==5 | ms02q23==6 
* Superior
	replace edu = 5 if ms02q23==7

* label education variables
	la define edlab 0 "No education" 1 "Koranic Schooling" 2 "Primary" /*
	*/ 3 "At least secondary first cycle"   /*
	*/  4 "At least secondary second cycle" 5 "Superior"
	label values edu edlab
	tab edu

* Highest education in hh (excluding koranic education edu == 1)
	egen educHigh = max(edu) if edu!=1, by(hid)
	la var educHigh "highest level of education in hh"
	recode educHigh (2=1) (3=2) (4=3) (5=4)

* Average education for adults in hh
	egen educAdult = mean(edu) if (ms01q06a>=15 & ms01q06a<.) & edu!=1, by(hid)
	la var educAdult "average education in hh age>15"  

* housecleaning
	drop under15t under24t 

* Create a variable to represent ethnically mixed households
	egen ethnicity = mean(ms01q24), by(hid)
	label copy ms01q24code eth0
	label values ethnicity eth0
	la var ethnicity "Household ethnicities (mean)"

	g byte hhmixed = mod(ethnicity, 1)!=0
	la define eth1 0 "HH one ethnicity" 1 "HH mixed ethnicity"
	la values hhmixed eth1

* Keep tabs on how many mobile phones are owned by hh
	g byte mobile = ms02q26==1 
	egen mobileTot = total(mobile), by(hid)

/* -- HEALTH -- 
* Create variables that reflect household health */

*Mother alive?
	ren ms01q12 motherAlive 

/* Health Module Questions */
* Had health problems in last week?
	ren ms03q01 hhHealth 

* Malaria
	g byte malaria = (ms03q02==1)
	la var malaria "HH had Malaria like symptoms"

* Diarrhea
	g byte diarrhea = (ms03q02==2)
	la var diarrhea "HH had diarrhea symptoms"

/* Reproductive Health Questions */
* Has spouse had at least one live birth?
	g byte births = (ms03q38==1 & ms01q02==2)
	la var births "HH spouse had at least once live birth"
	ren ms03q39 birthAge 

* Has spouse delivered live birth or is currently pregnant in last 12 mo?
	g byte fertility = inlist(ms03q45, 1, 3)==1 & ms03q40==1

* Mosquito net access in hh
	g byte mosquitoNet = (ms03q31==1 & ms03q32==1)

* Contraceptive use
	g byte contraceptionUse = (ms03q49==1)

* keep only the variables you have created and ones need to match on
	ds(ms0*), not
	keep `r(varlist)'

* Check for missing values before collapsing
	mdesc

save "$pathout/hhind.dta", replace

* Now determine the correct collapse method to roll-up to hh-level
/* -- The following can be collapsed using a normal mean  -- 

# Use MAX option for the following variables:
hoh femHead marriedHead widowHead singleHead male female under15male
under24male educHead literateHead eduMuslimHead eduAnyHead educHigh 
educAdult mobile mobileTot malaria diarrhea births

# Use default (mean) option for following:
hhSize agehead fsize sexRatio depRatio hhlabor mlabor flabor
mlaborShare flaborShare under15 under24 under15Share under24Share
*/

* Copy variable labels to reapply after collapse
	include "$pathdo/copylabels.do"

* Collapse data down to HH level
#delimit ;
	collapse (max) hoh femHead marriedHead widowHead singleHead male 
		female under15male under24male educHead literateHead 
		eduMuslimHead eduAnyHead educHigh educAdult mobile mobileTot
		malaria diarrhea births fertility polygamous contraceptionUse
		(mean) hhSize ageHead fsize sexRatio depRatio hhlabor
		mlabor flabor mlaborShare flaborShare under15 under24 
		under15Share under24Share ethnicity hhmixed mosquitoNet
		passage1 passage2 grappe menage, by(hid) fast;
#delimit cr
	order hid menage grappe passage1 passage2

* Reapply variable lables
include "$pathdo/attachlabels.do"

* Labeling
	lab define eth0 0 "Mixed", modify
	label values ethnicity eth0
	replace ethnicity=0 if mod(ethnicity,1)!=0
	label values educHead ed

sort hid
save "$pathout/hhchar.dta", replace

* Pull in base variables to be used for merging throughout
use "$wave1/ecvmamen_p1_en.dta", clear
local keeplist hid passage grappe menage region urbrur zae strate hhweight hhsize
keep `keeplist'

merge 1:1 hid using "$pathout/hhchar.dta", gen(hhjoin)
merge m:1 grappe using "$wave1/NER_EA_Offsets.dta", gen(mergeGeo)
drop if mergeGeo==2

save "$pathout/hhchar.dta", replace

log2html "$pathlog/hhchar", replace
