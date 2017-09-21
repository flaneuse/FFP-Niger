* Import data from download folder and run PCA for Niger Analysis
* Author: Tim Essam
* Date: 2017/09/21

clear
global pathin "C:\Users\Tim\Downloads"
global pathout "C:\Users\Tim\Documents\Niger"
import delimited "$pathin\NER_DHS_2012_PCA_LDH.csv"

drop floor_type_clumped

* Verify summary statistics for main vars
	ds(v1 hh_num cluster ), not
	* Double check that 
	sum `r(varlist)'

	
	* Destring variables so you can run pca
	foreach x of varlist ppl_room-where_cook_clumpedother owns_land-motor_pump {
		capture replace `x' = . if `x' == "NA"
		destring `x', force replace
		}
	*end
	
	ds(v1 hh_num cluster tlu owns_land land_size where_cook_clumpedother), not
	factor `r(varlist)', pcf
	predict wealth_rural
	scree
	loadingplot
	
	export delimited "$pathout\NER_DHS_wealthindex.csv", replace
	
	
	* replace missing land_ownership with median values for land ownership
	/*clonevar land_size2 = land_size
	sum land_size, d
	replace land_size2 = r(p50) if land_size2 == .
	
	ds(v1 hh_num cluster wealth_rural land_size owns_land land_size), not
	factor `r(varlist)', pcf
	predict wealth_rural2
	scree
	loadingplot
	
	scatter wealth_rural wealth_rural2
	
	*/
