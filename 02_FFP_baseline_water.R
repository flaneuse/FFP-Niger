
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



base %>% group_by(REG) %>% summarise(mean(improved_source, na.rm = T))

base %>% count(is.na(improved_source))

base %>% count(F04) %>%  mutate(pct = n/sum(n)) %>% arrange(desc(pct))

base %>% count(F06 > 30)

qplot(data = base, x = F06)

  