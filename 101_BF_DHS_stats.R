
# Import, clean, and calculate stats for Burkina Faso data ----------------



# setup -------------------------------------------------------------------
library(tidyverse)
library(haven)
library(svywrangler)
library(llamar)

data_dir = '~/Documents/Burkina Faso/rawdata/'
# data_dir = '~/Documents/USAID/Burkina Faso/rawdata/'

# Import household-level data, 2014
bf = read_stata(paste0(data_dir, 'BF_2014_MIS_10022017_1149_89151/bfhr70dt/BFHR70FL.DTA'))


# Import household-level data, 2010
bf10 = read_stata(paste0(data_dir, 'BF_2010_DHS_10022017_1149_89151/bfhr62dt/BFHR62FL.DTA'))


# Import children's data, 2010.
# No stunting/wasting data collected in 2014 MIS
bf_kids = read_stata(paste0(data_dir, "BF_2010_DHS_10022017_1149_89151/bfkr62dt/BFKR62FL.DTA"))

# Based on http://www.who.int/water_sanitation_health/monitoring/oms_brochure_core_questionsfinal24608.pdf
# Improved water codes
impr_water_codes = c(11,12,13,14,21,31,41,51)

# 11                                  Piped into dwelling Improved
# 12                                   Piped to yard/plot Improved
# 13                                 Public tap/standpipe Improved
# 14                              Piped from the neighbor Improved
# 21                                Tube well or borehole Improved
# 31                                       Protected well Improved
# 32                                     Unprotected well UNIMPROVED
# 41                                     Protected spring Improved
# 42                                   Unprotected spring UNIMPROVED
# 43 River/dam/lake/ponds/stream/canal/irrigation channel UNIMPROVED
# 51                                            Rainwater Improved
# 61                                        Tanker truck  UNIMPROVED
# 62                                 Cart with small tank UNIMPROVED
# 63                                         Water vendor UNIMPROVED
# 71                                        Bottled water UNIMPROVED
# 96 Other --> NA

impr_toilet_codes = c(11,12,13,15,21,22,41)

# 11           Flush to piped sewer system  Improved
# 12                  Flush to septic tank  Improved
# 13                  Flush to pit latrine  Improved
# 14                Flush to somewhere else UNIMPROVED
# 15                Flush, don't know where Improved
# 21 Ventilated Improved Pit latrine (VIP)  Improved
# 22                 Pit latrine with slab  Improved
# 23     Pit latrine without slab/open pit  UNIMPROVED
# 31                No facility/bush/field  UNIMPROVED
# 41                      Composting toilet Improved
# 42                         Bucket toilet  UNIMPROVED
# 43              Hanging toilet / latrine  UNIMPROVED
# 96                                Other   NA
# 99                               Unknown  NA

# clean hh data ---------------------------------------------------------

# View(pull_labels(bf))


bf = bf %>% 
  mutate(rural = ifelse(hv025 == 2, 1, 0)) %>% 
  select(
    cluster = hv001, hh_num = hv002, 
    psu = hv021, strata = hv022, region = hv024,
    svy_weight = hv005,
    rural,
    # wealth
    dhs_WI_cat = hv270,
    dhs_WI = hv271,
    
    # WASH:
    drinking_src = hv201, time2water = hv204, 
    toilet_src = hv205, shared_toilet = hv225
  ) 

# id_weirdos(bf)


bf = bf %>% 
  # Convert NA values -------------------------------------------------------
replace_missing(missing_codes = c(998), time2water) %>% 
  replace_missing(missing_codes = c(96, 99), toilet_src, drinking_src) %>% 
  # Factorize values --------------------------------------------------------
factorize('_lab', region, drinking_src, toilet_src) %>% 
  # Convert numbers to real decimals -----------------------------------------
mutate(
  svy_weight = svy_weight / 1e6,
  
  # Classify WASH vars --------------------------------------------------------
  
  # If facility is "on premises", assume it's w/i 15 min of hh
  time2water_2 = ifelse(time2water == 996, 1, time2water),
  time_cat = cut(time2water_2, breaks = c(seq(0, 60, by = 15), 899)),
  water_wi30 = ifelse(time2water_2 <= 30, 1, 0),
  
  
  # pull out open defecation
  od = ifelse(is.na(toilet_src_lab), NA,
              ifelse(toilet_src_lab == 'no facility/bush/field', 1, 0)),
  
  # fix shared toilet; if open defecating, assume shared.
  shared_toilet = ifelse(od == 1, 1, shared_toilet),
  
  # classify water and sanitation access
  impr_toilet_src = ifelse(is.na(toilet_src), NA,
                           ifelse(toilet_src %in% impr_toilet_codes, 1, 0)),
  impr_water_src = ifelse(is.na(drinking_src), NA,
                          ifelse(drinking_src %in% impr_water_codes, 1, 0)),
  
  impr_toilet = ifelse(impr_toilet_src == 1 & shared_toilet == 0, 1, 0),
  impr_water = ifelse(impr_water_src == 1 & water_wi30 == 1, 1, 0),
  x = 1
) 



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
# bf_kids = bf_kids %>% filter(dejure == 1)







# calculate percents ------------------------------------------------------
# national
# rural, national
# rural, centre-nord


# -- HH --
# National
lapply(c('od', 'impr_toilet', 'impr_water', 'impr_water_src'), function(x) calcPtEst(bf, var = x, use_weights = TRUE, 
                                                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))

# Urban / Rural
lapply(c('od', 'impr_toilet', 'impr_water', 'impr_water_src'), function(x) calcPtEst(bf, var = x, by_var = 'rural', use_weights = TRUE, 
                                                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))

# Rural, by regions
lapply(c('dhs_WI_cat'), function(x) calcPtEst(bf %>% filter(rural == 1), var = x, by_var = 'region_lab', use_weights = TRUE, 
                                                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))

# -- kids --

# National
lapply(c('stunted', 'wasted', 'diarrhea'), function(x) calcPtEst(bf_kids, var = x, use_weights = TRUE, 
                                                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))

# Urban / Rural
lapply(c('stunted', 'wasted', 'diarrhea'), function(x) calcPtEst(bf_kids, var = x, by_var = 'rural', use_weights = TRUE, 
                                                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))

# Rural, by regions
lapply(c('stunted', 'wasted', 'diarrhea'), function(x) calcPtEst(bf_kids %>% filter(rural == 1), var = x, by_var = 'region_lab', use_weights = TRUE, 
                                                                 psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight'))



# Plot water by WI -----------------------------------------------------
water = calcPtEst(bf %>% filter(rural == 1), var = 'impr_water', 
                     by_var = 'dhs_WI_cat', use_weights = TRUE, 
                     psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight')

ggplot(water, aes(x = avg, y = dhs_WI_cat, fill = avg)) + 
  geom_segment(aes(x = lb, xend = ub, y = dhs_WI_cat, yend = dhs_WI_cat), colour = grey15K, size = 2) +
  geom_point(size = 4, shape = 21, colour = grey90K) +
  scale_y_reverse(labels = c('lowest', 'low', 'middle', 'high', 'highest')) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + 
  scale_fill_gradientn(colours = llamar::YlGnBu) +
  coord_flip() +
  ggtitle('Improved water access within 30 minutes by asset quintile', subtitle = 'Rural Burkina Faso, 2014') +
  theme_ygrid()


# Plot stunting by WI -----------------------------------------------------
stunting = calcPtEst(bf_kids %>% filter(rural == 1, region_lab == 'Centre-Nord'), var = 'stunted', by_var = 'dhs_WI_cat', use_weights = TRUE, 
                     psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight')

ggplot(stunting, aes(x = avg, y = dhs_WI_cat, fill = avg)) + 
  geom_segment(aes(x = lb, xend = ub, y = dhs_WI_cat, yend = dhs_WI_cat), colour = grey15K, size = 2) +
  geom_point(size = 4, shape = 21, colour = grey90K) +
  scale_y_reverse(labels = c('lowest', 'low', 'middle', 'high', 'highest')) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.5)) + 
  scale_fill_gradientn(colours = llamar::inferno) +
  ggtitle('Stunting by asset quintile', subtitle = 'Rural Centre-Nord Burkina Faso, 2010') +
  theme_xgrid()

stunting = calcPtEst(bf_kids %>% filter(rural == 1), var = 'stunted', by_var = 'region_lab', use_weights = TRUE, 
                     psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight')

ggplot(stunting, aes(x = avg, y = fct_reorder(region_lab, avg), fill = avg)) + 
  geom_segment(aes(x = lb, xend = ub, y = fct_reorder(region_lab, avg) , yend = fct_reorder(region_lab, avg)), colour = grey15K, size = 2) +
  geom_point(size = 4, shape = 21, colour = grey90K) +
  # scale_y_reverse(labels = c('lowest', 'low', 'middle', 'high', 'highest')) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.5)) + 
  scale_fill_gradientn(colours = llamar::inferno) +
  ggtitle('Stunting by region', subtitle = 'Rural Burkina Faso, 2010') +
  theme_xgrid()

# Time to water bar graph -------------------------------------------------

ggplot(bf %>% filter(region == 5, !is.na(time_cat)), aes(x = time_cat, y = (..count..)/sum(..count..))) + 
  geom_bar() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5), name = NULL) +
  ggtitle('Distance to travel to aquire drinking water', subtitle = 'Percent of rural households in Centre-Nord, Burkina Faso (2014 DHS)') +
  theme_ygrid()
