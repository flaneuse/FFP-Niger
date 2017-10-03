
# Calculate various indicators from DHS household-level data --------------
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter
# 3 October 2017


# Indicator list ----------------------------------------------------------
# for all: all Niger + all rural + rural Zinder
# 1. Stunting % 
# 2. Wasting %
# 3. Open defecation
# 4. Improved Sanitation
# 5. Handwashing (n/a)
# 6. Improved drinking water
# 7. diarrhea 


hh = hh %>% mutate(toilet_src = na_if(toilet_src, 99),
                   od = ifelse(toilet_src == 31, 1, 0))

hh %>% count(urban, od)

library(svywrangler)


# nationally
calcPtEst(hh, 'od', use_weights = TRUE, strata_var = 'strata',
          psu_var = 'psu', weight_var = 'hh_wt')

# U/R split
calcPtEst(hh, 'od', by_var = 'urban', use_weights = TRUE, strata_var = 'strata',
          psu_var = 'psu', weight_var = 'hh_wt')
# Zinder
calcPtEst(hh %>% filter(region == 7), 'od', by_var = 'region', use_weights = TRUE, strata_var = 'strata',
          psu_var = 'psu', weight_var = 'hh_wt')

calcPtEst(hh, 'od', by_var = 'urban', use_weights = TRUE, strata_var = 'strata',
          psu_var = 'psu', weight_var = 'hh_wt')