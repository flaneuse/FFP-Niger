
# Import and clean Niger children's DHS data -----------------------------------------
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter

# setup -------------------------------------------------------------------

library(haven)
library(tidyverse)
library(svywrangler)
library(modelr)
library(llamar)

# data_dir = '~/Documents/USAID/Niger/NER_2012_DHS/'
data_dir = '~/Documents/Niger/data/NER_2012_DHS/'

# Based on http://www.who.int/water_sanitation_health/monitoring/oms_brochure_core_questionsfinal24608.pdf
# Improved water codes
impr_water_codes = c(11,12,13,21,31,41,51)

# 11                                  Piped into dwelling Improved
# 12                                   Piped to yard/plot Improved
# 13                                 Public tap/standpipe Improved
# 21                                Tube well or borehole Improved
# 31                                       Protected well Improved
# 32                                     Unprotected well UNIMPROVED
# 41                                     Protected spring Improved
# 42                                   Unprotected spring UNIMPROVED
# 43 River/dam/lake/ponds/stream/canal/irrigation channel UNIMPROVED
# 51                                            Rainwater Improved
# 62                                 Cart with small tank UNIMPROVED
# 63                                         Water vendor UNIMPROVED
impr_toilet_codes = c(11,12,21,22)

# 11           Flush to piped sewer system Improved
# 12                  Flush to septic tank  Improved
# 21 Ventilated Improved Pit latrine (VIP)  Improved
# 22                 Pit latrine with slab  Improved
# 23     Pit latrine without slab/open pit  UNIMPROVED
# 31                No facility/bush/field  UNIMPROVED
# 42                         Bucket toilet  UNIMPROVED
# 99                               Unknown  NA

# Import children’s data -------------------------------------------------
kids_raw = read_stata(paste0(data_dir,'nikr61dt/NIKR61FL.DTA'))



# Pull relevant vars ------------------------------------------------------

# In general, ignoring potentially relevant vars if exclude ~ 2000+ obs. --> all under 2 obs.

kids = kids_raw %>% 
  
  mutate(rural = ifelse(v025 == 2, 1, 0)) %>% 
  
  # grab 119-129, 153, 161: hh assets
  select(cluster = v001, hh_num = v002, 
         svywt = v005,
         interview_month = v006,
         age_mom = v447a, # check not v012 age_mom_cat = v013,
         psu = v021, strata = v022, region = v024,
         rural,
         dejure = v135,
         # ed
         highest_ed = v106, v017, v133, v149, lit = v155,
         husband_ed = v701, v702, v729, v715,
         husband_occup = v704, v705,
         husband_age = v730,
         mom_working = v714, mom_occup = v716, v717, v731, 
         
         # WASH
         drinking_src = v113,
         time2drinking = v115,
         toilet_src = v116,
         shared_toilet = v160,
         # hh characteristics
         elec = v119,
         hhsize = v136,
         kids_under5 = v137,
         femhead = v151,
         agehead = v152,
         freq_radio = v158,
         freq_tv = v159,
         # wealth
         dhs_WI = v191,
         dhs_WI_rural = v191r,
         
         dhs_WI_cat = v190,
         dhs_WI_ruralcat = v190r,
         
         mos_net = ml101, mps_net = v459, kid_slept_mosquito = v460, mom_slept_mosquito = v461,
         # children ever born, ratios v201-207, v218, v219, v220
         births_5y = v208,
         births_1y = v209,
         age_first_birth = v212,
         curr_preg = v213,
         mcu = v313,
         want_child = v367,
         fp_radio = v384a,
         fp_tv = v384b,
         fp_news = v384c,
         fp_worker = v393,
         visit_healthfac = v394,
         heard_ors = v416,
         mom_stunting = v440,
         mom_bmi = v445,
         mom_rohrer = v446,
         hemoglobin = v456, # adj for alt, smoking
         anemic = v457,
         poo_disposal = v465,
         # why hard to go to med fac v467b-f
         live_wpartner = v504,
         num_otherwives = v505,
         age_firstcohab = v511,
         unmet_need = v624,
         ideal_boys = v627, ideal_girls = v628, ideal_either = v629,
         # fem empower
         healthcare_decider = v743a, purchases_decider = v743b, visits_decider = v743d, money_decider = v743f,
         beating_womanout = v744a, beating_kidneglect = v744b, beating_arguing = v744c, beating_nosex = v744d, beating_burnsfood = v744e,
         woman_ownshouse = v745a, woman_ownsland = v745b,
         
         bidx, birth_order = bord, birth_month = b1, birth_year = b2,
         sex = b4, prev_birthinterval = b11, subseq_birthinterval = b12,
         # prenatal
         m2a, m2b, m2g, m2h, m2k, #2057 NAs
         doc_assist = m3a, nurse_assist = m3b, trad_assist = m3g, friend_assist = m3h, chw_assist = m3i, neighbor_assist = m3j, other_assist = m3k, no_assist = m3n,
         first_antenatal = m13, num_antenatal = m14, place_delivery = m15,
         birth_size = m18,
         
         # breastfeeding
         breastfeed_dur = m4, breastfeeding_months = m5,
         
         # vac
         vac_tb = h2, vac_dpt1 = h3, vac_polio1 = h4, vac_dpt2 = h5, vac_polio2 = h6, vac_dpt3 = h7, 
         vac_polio3 = h8, vac_measles = h9, vac_polio0 = h0, vac_yellowfever = s506y,
         diarrhea = h11, fever = 22, cough = h31, vitA = h33, vitA2 = h34, Fe = h42, int_parasites = h43,
         
         age_months = hw1, weight = hw2, height = hw3, 
         
         # malnutrition
         stunting = hw70, underweight = hw71, wasting = hw72, bmi = hw73, 
         
         kid_hemoglobin = hw56, kid_anemia = hw57,
         mosqnet_type = ml0,
         sz_unicef,
         
         # mother interaction
         kidactivity_read = s223ca, kidactivity_stories = s223cb, kidactivity_sing = s223cc, kidactivity_walk = s223cd, 
         kidactivity_play = s223ce, kidactivity_count = s223cf, kidactivity_morals = s223cg, kidactivity_islam = s223ch,
         
         # symptoms hospitalization: knowledge --> hospital
         # s562a-o
         washknow_foodprep = s563a, washknow_food = s563b, washknow_feed = s563c, washknow_aftereat = s563d, washknow_toilets = s563e, washknow_diaper = s563f,
         handwashing = s564,
         mom_noncommunicable = s1210aa
  ) %>% 
  # filter non-dejure residents; have no hh level info about them
  filter(dejure == 1)


# Merge in some missing hh-level vars -------------------------------------
source('03_DHS_wealthidx.R')
# TLUs and water shortage w/i past 2 weeks

kids = hh %>% select(cluster, hh_num, TLU, water_shortage) %>% 
  right_join(kids, by = c('cluster', 'hh_num'))


# cleanup data ------------------------------------------------------------
# Clean up procedure is as follows:
# 1) id and convert any NA codes to NAs
# 2) convert decimal values to proper decimals (since DHS multiplies all decimals by 10^x)
# 3) convert categorical values to factors
# 4) classify those factors into groups where appropriate (lump and collapse, or improved/not) and set base for regression
# 5) If neccessary, convert continuous variables to categories, e.g. TLU
# 6) center and scale the data for regression


# ID values that are missing. ---------------------------------------------

View(id_weirdos(kids))

# common missing values, often untagged
kids %>% count_value()


kids = kids %>% 
  # Converting "other" and "not dejure resident" to NA
  replace_missing(missing_codes = c(96, 97, 99), drinking_src, toilet_src) %>% 
  replace_missing(missing_codes = c(997, 998), time2drinking) %>% 
  replace_missing(missing_codes = c(98, 99), agehead, num_otherwives) %>% 
  replace_missing(missing_codes = 9996:9999, mom_stunting, mom_bmi, mom_rohrer,
                  stunting, wasting, underweight, bmi) %>% 
  replace_missing(missing_codes = c(8, 9), num_otherwives, birth_size, diarrhea, cough, fever,
                  vac_tb, vac_dpt1, vac_dpt2, vac_dpt3, vac_polio1, vac_polio2, vac_polio3, vac_polio0,
                  vac_measles, vac_yellowfever, shared_toilet,
                  washknow_foodprep, washknow_food, washknow_feed, washknow_aftereat, washknow_toilets, washknow_diaper)

kids %>% count_value(7) # not dejure
kids %>% count_value(8)
kids %>% count_value(9)
kids %>% count_value(98)
kids %>% count_value(99)
kids %>% count_value(998)
kids %>% count_value(999)
kids %>% count_value(9998)
kids %>% count_value(9999)

# decimalize --------------------------------------------------------------
kids = kids %>% 
  mutate(svywt = svywt / 1e6,
         dhs_WI = dhs_WI/1e5,
         dhs_WI_rural = dhs_WI_rural/1e5,
         stunting = stunting/1e2,
         wasting = wasting/1e2,
         underweight = underweight/1e2,
         mom_rohrer = mom_rohrer/1e2,
         mom_bmi = mom_bmi/1e2,
         stunted = as.numeric(stunting < -2),
         wasted = as.numeric(wasting < -2)
  )


# factorize and lump factors ----------------------------------------------
# Also bin numeric values
kids = kids %>% 
  factorize('_lab', toilet_src, drinking_src, region, sex, femhead) %>% 
  
  # lump factors
  mutate(impr_toilet = ifelse(is.na(toilet_src), NA,
                              ifelse(toilet_src %in% impr_toilet_codes, 1, 0)),
         impr_water = ifelse(is.na(drinking_src), NA, 
                             ifelse(drinking_src %in% impr_water_codes, 1, 0)),
         # bin water access; code 996 == "on premise".  Assuming that's within 30 min.
         water_wi30min = as.numeric(time2drinking <= 30 | time2drinking == 996),
         # fix shared toilet; assuming if you poop in the bushes that it's shared.
         shared_toilet = ifelse(toilet_src_lab == "No facility/bush/field", 1, shared_toilet),
         # discarding "wash hands after eating"; not common question
         wash_knowl = washknow_foodprep + washknow_food + washknow_feed + washknow_toilets + washknow_diaper,
         
         # calc squared age
         age_months_sq = age_months^2,
         # convert to factor
         interview_month = factor(interview_month, levels = c(5,2:4,6, 7))
  )



# Stunting / wealth index -------------------------------------------------

ggplot(kids %>% filter(region_lab == 'Zinder'), aes(x = as.factor(dhs_WI_ruralcat), y = stunting)) +
  geom_boxplot()

ggplot(kids %>% filter(region_lab == 'Zinder'), aes(x = as.factor(dhs_WI_cat), y = impr_water)) +
  stat_summary(fun.y = 'mean', geom = 'point')


# center and scale data ---------------------------------------------------
# filter out relevant kids ------------------------------------------------
# children universe: (1) all rural Niger; (2) all rural Zinder

all_stunting = kids  %>% 
  # ignore children lacking stunting data and from urban areas
  filter(!is.na(stunting),
         rural == 1) %>% 
  center_scale(center = F)

zinder = kids %>% 
  filter(region_lab == 'Zinder', 
         !is.na(stunting),
         rural == 1) %>% 
  center_scale(center = F)

# create basic models -----------------------------------------------------
# Note: DHS wealth index includes WASH vars.
models = formulas(~stunting,
                  wash = 
                    ~ # WASH
                    impr_toilet + shared_toilet + 
                    impr_water + wash_knowl + diarrhea +
                    water_shortage # within last 2 weeks
                  ,
                  basic = 
                    # basic demographics
                    ~ sex_lab + age_months_sq +
                    birth_order +
                    interview_month +
                    # hh demographics
                    region_lab + hhsize + kids_under5 +
                    femhead_lab +
                    
                    # mother
                    mom_rohrer + # corpulence idx, similar to BMI but maybe better
                    num_otherwives +
                    
                    # Wealth 
                    dhs_WI_rural + TLU
                  ,
                  combo = add_predictors(basic, wash))


zinder_models = formulas(~stunting,
                         wash = 
                           ~ # WASH
                           impr_toilet + shared_toilet + 
                           impr_water + wash_knowl + diarrhea +
                           water_shortage # within last 2 weeks
                         ,
                         basic = 
                           # basic demographics
                           ~ sex_lab + age_months_sq +
                           birth_order +
                           interview_month +
                           # hh demographics
                           hhsize + kids_under5 +
                           femhead_lab +
                           
                           # mother
                           mom_rohrer + # corpulence idx, similar to BMI but maybe better
                           num_otherwives +
                           
                           # Wealth 
                           dhs_WI_rural + TLU
                         ,
                         combo = add_predictors(basic, wash))

# Stunting z-score
all_z = all_stunting %>% fit_with(lm, models)
zinder_z = zinder %>% fit_with(lm, zinder_models)

# Binary stunted
# all_stunted = all_stunting %>% fit_with(glm, models_stunted, family = binomial)
# zinder_stunted = zinder %>% fit_with(glm, models_stunted, family = binomial)


# summary -----------------------------------------------------------------
summary(all_z$combo)
summary(zinder_z$combo)

plot_coef(all_z$combo)
plot_coef(zinder_z$combo)


# quick plot: stunting by wealth ------------------------------------------
x = kids %>% calcPtEst(var = 'stunted', by_var = 'dhs_WI_ruralcat',
                       use_weights = TRUE, weight = 'svywt',
                       strata = 'strata', psu = 'psu')

ggplot(x, aes(x = dhs_WI_ruralcat, y = stunted)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = 'identity') +
  ggtitle("Rural Niger stunting by asset quintile", subtitle = '2012 DHS') +
  theme_ygrid()


# Calculations ------------------------------------------------------------

# -- Kids --
# National
lapply(c('stunted', 'wasted'), function(x) calcPtEst(kids, var = x, use_weights = TRUE, 
                                                     psu_var = 'psu', strata_var = 'strata', weight_var = 'svywt'))

# Urban / Rural
lapply(c('stunted', 'wasted'), function(x) calcPtEst(kids, var = x, by_var = 'rural', use_weights = TRUE, 
                                                                                     psu_var = 'psu', strata_var = 'strata', weight_var = 'svywt'))

# Rural, by regions
lapply(c('stunted', 'wasted'), function(x) calcPtEst(kids %>% filter(rural == 1), var = x, by_var = 'region_lab', use_weights = TRUE, 
                                                                                     psu_var = 'psu', strata_var = 'strata', weight_var = 'svywt'))



# export data for Tim. ----------------------------------------------------
write.csv(all_stunting, '~/Documents/USAID/Niger/data/NER_DHS_semicleankidsdata_2017-09-22.csv')

