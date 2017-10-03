* Import data from download folder and run PCA for Niger Analysis
* Author: Tim Essam
* Date: 2017/09/25

clear
global pathin "C:\Users\Tim\Documents\Niger\Datain\"
global pathout "C:\Users\Tim\Documents\Niger\Dataout\"
*import delimited "$pathout\NER_DHS_2012_PCA_LDH.csv"
use "$pathout\NER_DHS_2012_PCA_LDH.dta"

* Verify summary statistics for main vars
	ds(hh_num cluster ), not
	* Double check that 
	sum `r(varlist)'

	
	/* Destring variables so you can run pca
	foreach x of varlist ppl_room-where_cook_clumpedother owns_land-motor_pump {
		capture replace `x' = . if `x' == "NA"
		destring `x', force replace
		}
	*end
	*/
	
	* Use the diagnostics from Stata to determine a reasonable factor score
	* Little variation from wall type and roof type
	#delimit ;
	ds(ppl_room elec floor_type_clumpedDirt_sand 
		cooking_fuel_clumpedWood radio tv bicycle motorcycle 
		mobile vcr watch animal_cart plow motor_pump owns_bednet);
	#delimit cr
	factor `r(varlist)', pcf
	estat kmo
	scree
	loadingplot
	predict wealth_rural
	histogram wealth_rural
	
	keep hh_num cluster ppl_room wealth_rural owns_land land_size TLU
	
save "$pathout\NER_DHS_2012_WealthIndex.dta", replace
	
