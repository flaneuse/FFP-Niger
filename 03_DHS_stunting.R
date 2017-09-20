
# Import and clean Niger children's DHS data -----------------------------------------
# Laura Hughes, lhughes@usaid.gov, USAID | GeoCenter

# setup -------------------------------------------------------------------

library(haven)
library(tidyverse)
library(svywrangler)
library(modelr)

kids_raw = read_stata('~/Documents/USAID/Niger/NER_2012_DHS/nikr61dt/NIKR61FL.DTA')


# In general, ignoring potentially relevant vars if exclude ~ 2000+ obs. --> all under 2 obs.

kids = kids_raw %>% 

  mutate(rural = ifelse(v025 == 2, 1, 0),
         stunting = hw70 / 1e2
         ) %>% 
  
  # grab 119-129, 153, 161: hh assets
  select(cluster_num = v001, hh_num = v002, 
         interview_month = v006,
         age_mom = v447a, # check not v012 age_mom_cat = v013,
         psu = v021, strata = v022, region = v024,
         rural,
         # ed
         highest_ed = v106, v017, v133, v149, lit = v155,
         husband_ed = v701, v702, v729, v715,
         husband_occup = v704, v705,
         husband_age = v730,
         mom_working = v714, mom_occup = v716, v717, v731, 
         drinking_src = v113,
         time2drinking = v115,
         toilet_src = v116,
         shared_toilet = v160,
         elec = v119,
         dejure = v135,
         hhsize = v136,
         kids_under5 = v137,
         femhead = v151,
         agehead = v152,
         freq_radio = v158,
         freq_tv = v159,
         dhs_WI = v191,
         dhs_WI_rural = v191r,
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
         polio3 = h8, measles = h9, vac_polio0 = h0, vac_yellowfever = s506y,
         diarrhea = h11, fever = 22, cough = h31, vitA = h33, vitA2 = h34, Fe = h42, int_parasites = h43,
         
         age_months = hw1, weight = hw2, height = hw3, stunting = hw70, underweight = hw71, wasting = hw72, bmi = hw73, kid_hemoglobin = hw56, kid_anemia = hw57,
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
  )


# center and scale data ---------------------------------------------------


# filter out relevant kids ------------------------------------------------
# children universe: (1) all rural Niger; (2) all rural Zinder

all_kids = kids  %>% 
  # ignore children lacking stunting data and from urban areas
  filter(!is.na(stunting), stunting < 99,
         rural == 1)

zinder = kids %>% filter(region == 'Zinder')

# create basic models -----------------------------------------------------

models = formulas(~stunting,
                  basic = 
                    # basic demographics
                    ~ sex + age_months*age_months +
                    # hh demographics
                     region + hh_size,
                  new = add_predictors(basic, ~vac_tb))

# Stunting z-score
all_z = all_kids %>% fit_with(lm, models)
zinder_z = zinder %>% fit_with(lm, models)

# Binary stunted
all_stunted = all_kids %>% fit_with(glm, models_stunted, family = binomial)
zinder_stunted = zinder %>% fit_with(glm, models_stunted, family = binomial)

