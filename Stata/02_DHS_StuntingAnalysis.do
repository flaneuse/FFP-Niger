* Combine kids data with household wealth index and conduct regression analysis
* Author: Tim Essam
* Date: 2017/09/25

* Run 01_DHS_Wealth_index prior to running this .do file to set globals
	macro list

* Load kids data and merge in wealth index for regressions
use "$pathin/NER_2012_DHS/nikr61dt/NIKR61FL.DTA", clear
	g cweight = (v005/1000000)
	clonevar anthroTag = v042
	clonevar DHSCLUST = v001
	clonevar hh_num = v002
	clonevar cluster = v001
	clonevar rep_num = v003
	clonevar intdate = v008
	clonevar psu	= v021
	
	* January is 1345, use the following to map in months
	di 12*(2012-1900)+1
	recode intdate (1351 = 1350)
	la def cmc 1346 "Feb. 2012" 1347 "Mar. 2012" 1348 "Apr. 2012" /*
	*/ 1349 "May 2012" 1350 "Jun. 2012" 
	la val intdate cmc
	
	* Distance to health facility
	g byte distHealthFacProb = inlist(v467d, 1)
	la var distHealthFacProb "getting med help for self is a prob"
	
	clonevar wealth = v190
	clonevar wealth_rur = v190r 
	clonevar strata		= v022
	
	g wealthIndex_rur = (v191r/1e5)
	g wealthIndex = (v191/1e5)
	clonevar floorType = v127
	g dirtFloor = inlist(floorType, 11)
	
	* Mother's bmi
	replace v445 = . if v445 == 9999
	g bmitmp = (v445/100)
	egen motherBMI = cut(bmitmp), at(0, 18.5, 25.0, 50) label
	la def bmi 0 "undernourished" 1 "normal" 2 "overweight"
	la val motherBMI bmi
	
	* Mother's education
	clonevar motherEd = v106
	replace motherEd = . if motherEd == 9
		
	keep hh_num cweight- motherEd
save "$pathout/NER_DHS_hhcore.dta", replace

use "$pathout/NER_Kids_data_20170925.dta", clear
	merge m:m hh_num cluster using "$pathout/NER_DHS_hhcore.dta", gen(_hhcore_merge)
	merge m:1 hh_num cluster using "$pathout/NER_DHS_2012_WealthIndex.dta", gen(_hh_merge)
	tab _hh_merge rural, mi
	
	
* Retain rural households as the analysis is focused there
	keep if rural == 1
	
	* Flag kiddos with missing stunting values
	g byte elig = !missing(stunting)
	
	g byte stunted = (stunting < -2.0)
	replace stunted = . if stunting == .
	
	g byte extstunted = (stunting < -3.0)
	replace extstunted =. if stunting == .
	
	* Recode a few variables for regression
	recode sex (1 = 0 "male")(2 = 1 "female"), gen(female)
	
	* calculate moving average for male and females
	* No relation can be found w/ derived wealth index
	preserve
		collapse (sum) stunted (count) stuntN = stunted, by(age_months female)
		drop if age_months == . | age_months < 6
		sort female age_months
		xtset  female age_months

		bys female: g smoothStunt = (l2.stunted + l1.stunted + stunted + f1.stunted + f2.stunted)/ /*
		*/		(l2.stuntN + l1.stuntN + stuntN + f1.stuntN + f2.stuntN) 

		tssmooth ma stuntedMA = (stunted/stuntN), window(2 1 2)
		xtline(stuntedMA)
	restore

	* Fix regression variables
	replace mom_stunting = mom_stunting/100
	recode femhead (1 = 0 "male")(2 = 1 "female"), gen(femhead2)
	g byte orsKnowledge = inlist(heard_ors, 1, 2)
	replace highest_ed = . if highest_ed == 9
	
	g byte bnetITNuse = inlist(mos_net, 1) & mps_net == 1
	la var bnetITNuse "own ITN mosquito bednet"
	
	g byte landless = (owns_land == 0)
	la var landless "hh is landless"
	
	winsor2 TLU, cuts(0 99) trim
	
	recode int_parasites (0 8 9 = 0 "No")(1 = 1 "Yes"), gen(intParasites)
	recode vitA (0 8 9 = 0 "No")(1 2 3 = 1 "Yes"), gen(vitaminA)
	
	replace diarrhea = 1 if diarrhea == 2
	
	recode anemic (4 = 0 "not anemic")(3 = 1 "mild")(2 = 2 "moderate")(1 = 4 "severe")(9 = .), gen(anemia)
	g byte anemic_bin = inlist(anemia, 1, 2, 3)
	
	xtile landquant = land_size, n(5)
	
	* Generate survey statistics
	svyset psu [pw = cweight], strata(strata)	

	* Check stunting over standard covariates
	svy:mean stunted, over(region)
	matrix smean = r(table)
	matrix district = smean'

	* Create locals for reference lines in coefplot
	local stuntmean = smean[1,1]
	local lb = smean[5, 1]
	local ub = smean[6, 1]

	matrix plot = r(table)'
	matsort plot 1 "down"
	matrix plot = plot'
	coefplot (matrix(plot[1,])), ci((plot[5,] plot[6,])) xline(`stuntmean' `lb' `ub')
	
	* running a few other statistics
	local varlist female wealth_rur wealth diarrhea impr_toilet impr_water region
	foreach x of local varlist {
		svy:mean stunted, over(`x')
		}
	*end	
	
* -----------------------------------------------------------------------------------------------------------
	keep if elig == 1
* Create groups for covariates as they map into conceptual framework for stunting
	global motchar "mom_stunting mom_bmi  motherEd femhead2 orsKnowledge"
	global hhchar "ib(3).wealth_rur ib(1).landquant"
	global hhchar1 "wealth_rural impr_toilet impr_water bnetITNuse ib(1).landquant TLU"
	global hhchar2 "impr_toilet impr_water bnetITNuse landless"
	global hhchar3 "impr_toilet impr_water bnetITNuse landless TLU"
	global hhchar4 "impr_toilet i.impr_water##ib(5).region"
	global hhag "TLU"
	global assets "wealthIndex_rur"
	global demog "hhsize agehead kids_under5"
	global chldchar "age_months age_months_sq birth_order birth_size"
	global chealth "intParasites vitaminA diarrhea anemic_bin"
	global geog "distHealthFacProb ib(5).region ib(1348).intdate i.water_shortage"
	global geog1 "distHealthFacProb ib(1348).intdate"
	global geog2 "distHealthFacProb i.water_shortage ib(1348).intdate if region == 7"
	global cluster "cluster(DHSCLUST)"
	
	
	global depvar "stunting"
	global modtype "reg"
	
	est clear
	qui eststo stedl_0: reg $depvar $motchar $hhchar2 $demog female $chldchar $chealth $geog, $cluster
	qui eststo stedl_1: reg $depvar $motchar $hhchar3 $demog female $chldchar $chealth $geog, $cluster
	qui eststo stedl_2: reg $depvar $motchar $hhchar1 $demog female $chldchar $chealth $geog, $cluster
	qui eststo sted1_3: reg $depvar $motchar $hhchar $demog female $chldchar $chealth $geog, $cluster 
	esttab sted*, se star(* 0.10 ** 0.05 *** 0.01) label ar2 pr2 beta not /*eform(0 0 1 1 1)*/ compress
	esttab sted* using "tmp.csv", se star(* 0.10 ** 0.05 *** 0.001) label replace
	
	
	* Appears to be small, but signficant relationship between z-scores and improved water for Zinder

	est clear
	qui eststo stedl_0: $modtype $depvar $motchar $hhchar2 female $chldchar $geog2, $cluster
	qui eststo stedl_1: $modtype $depvar $motchar $hhchar2 $demog female $chldchar $chealth $geog2, $cluster
	qui eststo stedl_2: $modtype $depvar $motchar $hhchar3 $demog female $chldchar $chealth $geog2, $cluster
	qui eststo stedl_3: $modtype $depvar $motchar $hhchar1 $demog female $chldchar $chealth $geog2, $cluster
	
	* Report standardized coefficients for only significant covariates; Can also use 'reg y x ... , beta' command
	*  bStdXY is the fully standardized coefficient, (mean 0 and standard deviation of 1)
	listcoef,constant pv(.05)
	
	qui eststo sted1_4: $modtype $depvar $motchar $hhchar $demog female $chldchar $chealth $geog2, $cluster 
	esttab sted*, se star(* 0.10 ** 0.05 *** 0.01) label ar2 pr2 beta not /*eform(0 0 1 1 1)*/ compress
	esttab sted* using "tmp2.csv", se star(* 0.10 ** 0.05 *** 0.001) label replace
	
	esttab stedl*, se star(* 0.10 ** 0.05 *** 0.01) label ar2 beta
	coefplot stedl_0 || stedl_1 || stedl_2 || sted1_3 , drop(_cons ) /*
	*/ xline(0) /*mlabel format(%9.2f) mlabposition(11) mlabgap(*2)*/ byopts(row(1)) 
	
	
	* Regional variations
	est clear
	local i = 0
	levelsof region, local(levels)
	foreach x of local levels {
		local name =  strtoname("`x'")
		eststo stunt_`name', title("Stunted `x'"): reg stunting $motchar $hhchar1 $demog /*
		*/ female $chldchar $chealth $geog2 if region == `x', $cluster 
		local i = `++i'
		}
