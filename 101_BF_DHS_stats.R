
# Import, clean, and calculate stats for Burkina Faso data ----------------



# setup -------------------------------------------------------------------
library(tidyverse)
library(haven)
library(svywrangler)
library(llamar)

# Import household-level data, 2014
bf = read_stata('~/Documents/Burkina Faso/rawdata/BF_2014_MIS_10022017_1149_89151/bfhr70dt/BFHR70FL.DTA')


# Import household-level data, 2010
bf = read_stata('~/Documents/Burkina Faso/rawdata/BF_2010_DHS_10022017_1149_89151/bfhr62dt/BFHR62FL.DTA')

# Import household-level data, 2010

# Import children's data, 2010.
# No stunting/wasting data collected in 2014 MIS
bf_kids = read_stata("~/Documents/Burkina Faso/rawdata/BF_2010_DHS_10022017_1149_89151/bfkr62dt/BFKR62FL.DTA")


# clean hh data ---------------------------------------------------------

View(pull_labels(bf))


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
  mutate(time_cat = cut(time2water, breaks = c(seq(0, 60, by = 15), 899)))

bf = bf %>% 
  # Convert NA values -------------------------------------------------------
replace_missing(missing_codes = c(9996, 9997, 9998, 9999), stunting, wasting, bmi) %>% 
  replace_missing(missing_codes = c(8, 9), diarrhea) %>% 
  # Convert numbers to real decimals -----------------------------------------
mutate(
  svy_weight = svy_weight / 1e6,
  
  # Classify WASH vars --------------------------------------------------------
  
  od = ifelse()
) %>% 
  # Factorize values --------------------------------------------------------
factorize('_lab', region)




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



# Plot stunting by WI -----------------------------------------------------
stunting = calcPtEst(bf_kids %>% filter(rural == 1), var = 'stunted', by_var = 'dhs_WI_cat', use_weights = TRUE, 
                     psu_var = 'psu', strata_var = 'strata', weight_var = 'svy_weight')

ggplot(stunting, aes(x = avg, y = dhs_WI_cat, fill = avg)) + 
  geom_segment(aes(x = lb, xend = ub, y = dhs_WI_cat, yend = dhs_WI_cat), colour = grey15K, size = 2) +
  geom_point(size = 4, shape = 21, colour = grey90K) +
  scale_y_reverse(labels = c('lowest', 'low', 'middle', 'high', 'highest')) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.5)) + 
  scale_fill_gradientn(colours = llamar::inferno) +
  ggtitle('Stunting by asset quintile', subtitle = 'Rural Centre-Nord Burkina Faso, 2010') +
  theme_xgrid()


# Time to water bar graph -------------------------------------------------

ggplot(bf %>% filter(region == 5, !is.na(time_cat)), aes(x = time_cat, y = (..count..)/sum(..count..))) + 
  geom_bar() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5), name = NULL) +
  ggtitle('Distance to travel to aquire drinking water', subtitle = 'Percent of rural households in Centre-Nord, Burkina Faso (2014 DHS)') +
  theme_ygrid()
