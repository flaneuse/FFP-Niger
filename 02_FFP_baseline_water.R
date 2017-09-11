
# Import, clean, and cross-reference water access data --------------------
# Laura Hughes, lhughes@usaid.gov
# 11 September 2017


# survey characteristics / questions --------------------------------------

# -- Pertinent WASH questions: --
# F4: source of drinking water
# F5: water source location
# F6: time to water
# F7: water available all year
# F8: water available last 2 weeks
# F9: treat water
# F10's: treat water with X
# F11: toilet type
# F12: toilet shared
# F13: # hh sharing toilet
# F14: where wash hands
# F15: water at handwashing station
# F16: soap at handwashing station
# F17's: when need to wash hands

# -- Other important vars --
# HH: hh id
# PVO: IP
# strata: strata
# cluster: cluster
# HHWT: household sampling weight
# VN: village number
# A04a: commune (admin3; 265 nationally)
# A04b: department (admin2; 36 nationally)
# REG: region (admin1; 7 nationally. FFP in 2)
# gendered_hh: fem/male headed
# improved_source: binary for water
# improved_sanitation: binary for toilets
# soap_water: soap + water at handwashing station
# critical_handwashing: # important times to wash hands known
# critical_handwashing_cat: knows 3/5 times to wash hands
# critical_handwashing_all: knows 5/5 times to wash hands

# -- IMPROVED WATER definition --
impr_water_codes = c(11,12,13,21,31,41,51)
# defined here: https://github.com/flaneuse/FFP-Niger/wiki/Crosswalk-for-Improved-Sanitation-and-Drinking-Water
# ** Must be within 30 min of the household.**
# | Qnum | Type                | Details                                                                            | Code | Classification |
#   |------|---------------------|------------------------------------------------------------------------------------|------|----------------|
#   | F04  | Running Water       | water supply network within the house                                              | 11   | Improved       |
#   | F04  | Running Water       | water supply network in the yard/premises                                          | 12   | Improved       |
#   | F04  | Running Water       | public taps and standpipes                                                         | 13   | Improved       |
#   | F05  | Wells and boreholes | Wells and boreholes                                                                | 21   | Improved       |
#   | F04  | Dug Wells           | protected wells                                                                    | 31   | Improved       |
#   | F04  | Dug Wells           | non protected wells                                                                | 32   | Unimproved     |
#   | F04  | Spring Water        | protected spring                                                                   | 41   | Improved       |
#   | F04  | Spring Water        | unprotected spring                                                                 | 42   | Unimproved     |
#   | F04  | Rain Water          | rain water                                                                         | 51   | Improved       |
#   | F04  | Tankers             | tankers                                                                            | 61   | Unimproved     |
#   | F05  | Trucks              | trucks equipped with small barrels                                                 | 71   | Unimproved     |
#   | F06  | Surface             | surface water river / dam / lake / ponds / spring / channel / (irrigation channel) | 81   | Unimproved     |
#   | F07  | Bottled             | bottled water                                                                      | 91   | Unimproved     |
#   | F07  | Others              | (specify)                                                                          | 96   |                |  

# -- IMPROVED SANITATION definition --
# defined here: https://github.com/flaneuse/FFP-Niger/wiki/Crosswalk-for-Improved-Sanitation-and-Drinking-Water
# ** Must not be shared with other houses **

impr_toilet_codes = c(11,12,13,14,15,21,22,31)

#   | Qnum | Type                                   | Details                            | Code | Classification |
#   |------|----------------------------------------|------------------------------------|------|----------------|
#   | F11  | Flush Toilet/Manual Flushing Connected | to a sewer system                  | 11   | Improved       |
#   | F11  | Flush Toilet/Manual Flushing Connected | to a septic tank                   | 12   | Improved       |
#   | F11  | Flush Toilet/Manual Flushing Connected | to a cesspit                       | 13   | Improved       |
#   | F11  | Flush Toilet/Manual Flushing Connected | to something else                  | 14   | Improved       |
#   | F11  | Flush Toilet/Manual Flushing Connected | don't know where                   | 15   | Improved       |
# | F11  | Cesspit                                | Improved cesspits self-ventilated  | 21   | Improved       |
# | F11  | Cesspit                                | cesspits with slab                 | 22   | Improved       |
# | F11  | Cesspit                                | cesspits without slabs / open hole | 23   | Unimproved     |
# | F11  | Compost                                | compost toilets                    | 31   | Improved       |
# | F11  | Pail                                   | pail / latrine bucket              | 41   | Unimproved     |
# | F11  | Suspended toilet                       | supended toilets / latrines        | 51   | Unimproved     |
# | F11  | No toilet                              | no toilets / nature                | 61   | Unimproved     |
# | F11  | Others                                 | other                              | 96   |                |
# 
# _To be classified as improved, the facility cannot be shared._
# 
# | Qnum | Type                                            | Details | Code | Classification |
# |------|-------------------------------------------------|---------|------|----------------|
# | F12  | Do you share your toilet with other households? | Yes     | 1    | Unimproved     |
# | F12  | Do you share your toilet with other households? | No      | 2    |                |


# Import data, pkgs -------------------------------------------------------
data_dir = '~/Documents/Niger/data/'

library(tidyverse)
library(llamar)
library(svywrangler)

# Data downloaded 8 September 2017 from USAID Open Data portal
# https://www.usaid.gov/data/dataset/08ee315f-c339-4c2f-8ae8-8988c9ef05ff
base = read_csv(paste0(data_dir, 'Niger_Sanitation and Maternal Health_Data.csv'))

# pull out the coded values for the variables
base_labels = read_csv(paste0(data_dir, 'Niger_Sanitation and Maternal Health_Codebook_Values.csv'),
                       skip = 1) %>% 
  fill(`Value`) %>% 
  rename(variable = Value, value = X2, label = Label)



# Data cleanup ------------------------------------------------------------
wash = base %>% 
  select(hhid = HH, ip = PVO, strata, cluster, weight = HHWT,
         village = VN, admin3 = A04a, admin2 = A04b,
         admin1 = REG, gendered_hh, 
         improved_source, improved_sanitation, soap_water,
         contains('critical_handwashing'),
         F04, F05, F06, F07, F08, F09, contains('F10'),
         F11, F12, F13, F14, F15, F16, contains('F17')) %>% 
  mutate(
    # create binaries for whether toilet/water source is improved
    impr_watersrc = ifelse(F04 %in% impr_water_codes, 1, 0),
    impr_toiletsrc = ifelse(F11 %in% impr_toilet_codes, 1, 0),
    
    # binary for whether water is within 30 min.
    # Either time (F06) is <= 30, or location is within dwelling or yard (F05 == 1 | 2)
    water_wi30min = ifelse(F06 == 998, NA, ifelse(F06 <= 30 | F05 < 3, 1, 0)),
    water_elsewhere = ifelse(F05 == 3, 1, 0),
    
    # binary for if water is treated
    water_treated = ifelse(F09 == 8, NA, ifelse(F09 == 1, 1, 0)),
    
    # binary for whether toilet is shared
    shared_toilet = ifelse(F12 == 1, 1, 
                           ifelse(F12 == 2, 0, NA))
  )


# Improved water/toilet calculations ---------------------------------------------
wash = wash %>% 
  mutate(impr_water_lh = ifelse(impr_watersrc == 1 & water_wi30min == 1, 1,
                                0),
         check_water = impr_watersrc == improved_source,
         
         impr_toilet_lh = ifelse(impr_toiletsrc == 1 & shared_toilet == 0, 1, 0),
         check_toilet = impr_toilet_lh == improved_sanitation,
         
         water1 = ifelse(impr_watersrc == 1, 1, 0),
         water2 = ifelse(impr_watersrc == 1 & water_wi30min == 1, 1, 0),
         water3 = ifelse(impr_watersrc == 1 & water_elsewhere == 0, 1, 0),
         water4 = ifelse(impr_watersrc == 1 & water_elsewhere == 1 & water_treated == 1, 1, 0),
         water5 = ifelse(impr_watersrc == 1 & F07 == 1 & F08 == 2, 1, 0)
         )


# Calculate improved toilets ----------------------------------------------
# Double checking numbers
# According to the ICF Endline report, the numbers should be: Save the Children 0.105; CRS = 0.055; Mercy Corps = 0.132
# Numbers check out (11 September 2017)
svywrangler::calcPtEst(wash, 'improved_sanitation', by_var = 'ip', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')

svywrangler::calcPtEst(wash, 'impr_toilet_lh', by_var = 'ip', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')

svywrangler::calcPtEst(wash, 'impr_toilet_lh', by_var = 'admin2', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')