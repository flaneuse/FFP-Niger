
# Import, clean, and calculate stats for Burkina Faso data ----------------



# setup -------------------------------------------------------------------
library(tidyverse)
library(haven)
library(svywrangler)

# Import household-level data, 2014
bf = read_stata('~/Documents/Burkina Faso/rawdata/BF_2014_MIS_10022017_1149_89151/bfhr70dt/BFHR70FL.DTA')

# Import household-level data, 2010

# Import children's data, 2010.
# No stunting/wasting data collected in 2014 MIS
bf_kids = read_stata("~/Documents/Burkina Faso/rawdata/BF_2010_DHS_10022017_1149_89151/bfkr62dt/BFKR62FL.DTA")


View(pull_labels(bf))


# clean kids data ---------------------------------------------------------

# View(pull_labels(bf_kids))


bf_kids = bf_kids %>% 
  mutate(rural = ifelse(v025 == 2, 1, 0)) %>% 
  select(
    cluster = v001, hh_num = v002, 
    psu = v021, strata = v022, region = v024,
    svy_weight = v005,
    rural,
    dejure = v135,
    # wealth
    dhs_WI_cat = v190,
    dhs_WI = v191,
    age_months = hw1,
    # malnutrition
    stunting = hw70, underweight = hw71, wasting = hw72, bmi = hw73, 
    diarrhea = h11
  ) 

# id_weirdos(bf_kids)

bf_kids = bf_kids %>% 
  # Convert NA values -------------------------------------------------------
replace_missing(missing_codes = c(9996, 9997, 9998, 9999), stunting, wasting, bmi) %>% 
  replace_missing(missing_codes = c(8, 9), diarrhea) %>% 
  # Convert numbers to real decimals -----------------------------------------
mutate(
  svy_weight = svy_weight / 1e6,
  
  stunting = stunting / 1e2,
  underweight = underweight / 1e2,
  wasting = wasting / 1e2,
  bmi = bmi / 1e2,
  # Classify stunting, diarrhea --------------------------------------------------------

  stunted = as.numeric(stunting < -2),
  wasted = as.numeric(wasting < -2),
  diarrhea = ifelse(is.na(diarrhea), NA, ifelse(diarrhea %in% 1:2, 1, 0))
  ) %>% 
# Factorize values --------------------------------------------------------
factorize('_lab', region)

# filter non-dejure residents; have no hh level info about them
bf_kids = bf_kids %>% filter(dejure == 1)







# calculate percents ------------------------------------------------------
# national
# rural, national
# rural, centre-nord

# (7) Diarrhea
lapply(c('stunted', 'wasted', 'diarrhea'), function(x) calcPtEst(bf_kids, var = x, use_weights = TRUE, 
                                                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))

lapply(c('stunted', 'wasted', 'diarrhea'), function(x) calcPtEst(bf_kids, var = x, by_var = 'rural', use_weights = TRUE, 
          psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))

lapply(c('stunted', 'wasted', 'diarrhea'), function(x) calcPtEst(bf_kids %>% filter(rural == 1), var = x, by_var = 'region_lab', use_weights = TRUE, 
                                                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))



