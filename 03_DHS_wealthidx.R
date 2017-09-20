# Recreate DHS Wealth Index for Niger, 2012 -------------------------------
# Niger DHS Wealth Index is a PCA combination of 115 separate variables (or dummies)
# It also includes WASH components, including improved sanitation and water source.
# To be able to probe the influence of wealth on WASH access, need to recreate a new, standalone wealth index.

# Laura Hughes, lhughes@usaid.gov, 20 September 2017

# Goal: get water and sanitation access rates for Zinder province of Niger, broken down by wealth quintiles.

# DHS data downloaded from DHS website around 15 September 2017


# setup -------------------------------------------------------------------

library(haven)
library(tidyverse)
library(svywrangler)
library(forcats)

data_dir = '~/Documents/USAID/Niger/NER_2012_DHS/'


# read in individual level DHS data ---------------------------------------
# gotta use individual module to pull whether anyone in the household owns a house alone or jointly
# Doesn't seem to be in hh roster; using based on guidance from https://www.measureevaluation.org/resources/publications/tr-16-131a
womens_raw = read_dta(paste0(data_dir, 'niir61dt/NIIR61FL.DTA'))
mens_raw = read_dta(paste0(data_dir, 'nimr61dt/NIMR61FL.DTA'))

# calculate whether anyone in the household owns a house alone or jointly.
w = womens_raw %>% 
  group_by(cluster = v001, hh_num = v002) %>%
  mutate(owns_house = ifelse(v745a %in% 1:3, 1, 0)) %>% 
  summarise(women_houses = sum(owns_house, na.rm = TRUE)) 

m = mens_raw %>% 
  group_by(cluster = mv001, hh_num = mv002) %>% 
  mutate(owns_house = ifelse(mv745a %in% 1:3, 1, 0)) %>% 
  summarise(men_houses = sum(owns_house, na.rm = TRUE)) 


houses = full_join(m,w, by = c('cluster', 'hh_num')) %>% 
  ungroup() %>% 
  mutate(owns_house = as.numeric(men_houses > 0 | women_houses > 0)) %>% 
  select(cluster, hh_num, owns_house)


# read in household level DHS data --------------------------------------------------------
hh_raw = read_dta(paste0(data_dir, 'nihr61dt/NIHR61FL.DTA'))


hh = hh_raw %>% 
  filter(hv025 == 2) %>% 
  mutate(
    # calculate number sleeping as number of de jure members, unless de jure = 0.  Then go w/ defacto.
    num_sleep = ifelse(hv012 == 0, hv013, hv012),
    
    # convert to proper decimals
    land_size = hv245/10,
    hh_wt = hv005 / 1e6,
    WI_DHS_rural = hv271r / 1e5 
  ) %>% 
  select(
    # hh sampling attributes
    cluster = hv001, hh_num = hv002, psu = hv021, strata = hv022,
    hh_wt, int_month = hv006, head_sex = hv219,
    
    # geography
    region = hv024, urban = hv025,
    
    # infrastructure:
    hh_size = hv009, num_sleep, num_rooms = hv216, elec = hv206, 
    where_cook = hv241,
    floor_type = hv213, wall_type = hv214, roof_type = hv215,
    
    # WASH:
    drinking_src = hv201, time2water = hv204, 
    toilet_src = hv205, shared_toilet = hv225,
    
    # ag assets:
    owns_land = hv244, land_size,
    # assuming "horses/ donkeys/ mules" is an equal distribution of all 3 animals, when calculating TLUs
    cows = hv246b, equines = hv246c, goats = hv246d,
    sheep = hv246e, chickens = hv246f, camels = hv246g, ducks = hv246h,
    
    # durable assets:
    radio = hv207, tv = hv208, refrigerator = hv209, 
    bicycle = hv210, motorcycle = hv211, car = hv212,
    canoe = sh118e, cyclomotor = sh118f,
    telephone = hv221, mobile = hv243a, vcr = sh110d, computer = sh110k,
    ac = sh110h, antenna = sh110j,
    oven = sh110i, cooking_fuel = hv226, 
    watch = hv243b, 
    animal_cart = hv243c, plow = sh110m, motor_pump = sh110n,
    num_bednets = hml1, owns_bednet = hv227,
    
    # banking 
    bank_acct = hv247,
    
    WI_DHS_rural) 

# Check got all the decimal-ed data:
type_of(hh) %>% filter(type == 'numeric')

# merge in owns house -----------------------------------------------------
# Merging on v001 and v002, as per http://dhsprogram.com/data/Merging-Datasets.cfm
hh = hh %>% left_join(houses, by = c("cluster", "hh_num"))


# replace missing codes w/ NAs --------------------------------------------
View(id_weirdos(hh))

# common missing values, often untagged
hh %>% count_value()


hh = hh %>% 
  replace_missing(missing_codes = 9, bank_acct, owns_land, elec,
                  shared_toilet, radio, tv, refrigerator, bicycle, 
                  car, motorcycle, telephone, owns_land,
                  mobile, watch, animal_cart, where_cook, separate_kitchen,
                  # note: these are Niger-specific variables and are not listed as being NA in survey.
                  # However, seems only logical choice, since they are binaries.
                  vcr, canoe, cyclomotor, computer, ac, antenna, motor_pump, oven, plow
  ) %>%
  
  replace_missing(missing_codes = c(98, 99), cows, equines, goats, sheep, 
                  chickens, camels, ducks, 
                  toilet_src, drinking_src, num_rooms,
                  roof_type, floor_type, wall_type, num_bednets) %>% 
  replace_missing(missing_codes = c(998, 999), land_size, time2water) %>% 
  # Calculate ratio of people to rooms
  mutate(ppl_room = ifelse(num_rooms > 0, trunc(num_sleep / num_rooms), num_sleep))

hh %>% count_value(8)
hh %>% count_value(9)
hh %>% count_value(98)
hh %>% count_value(99)
hh %>% count_value(998)
hh %>% count_value(999)



# classify animals --------------------------------------------------------

# Breaks according to DHS: http://www.dhsprogram.com/programming/wealth%20index/Niger%20DHS%202012/niger%202012%20sps.pdf
hh = hh %>% 
  # create a copy
  mutate(cow_cat = cows, 
         equine_cat = equines,  
         goat_cat = goats,
         sheep_cat = sheep, 
         camel_cat = camels,
         chicken_cat = chickens,
         duck_cat = ducks) %>% 
  # large animals
  mutate_at(funs(cut(., breaks = c(-1, 0, 4, 9, 100),
                     labels = c('0', '1-4', '5-9', '10+'))), 
            .vars = vars(cow_cat, equine_cat, goat_cat, sheep_cat, camel_cat)) %>% 
  # small animals
  mutate_at(funs(cut(., breaks = c(-1, 0, 9, 29, 100),
                     labels = c('0', '1-9', '10-29', '30+'))), 
  .vars = vars(chicken_cat, duck_cat)) %>% 
  mutate(where_cook_cat = fct_collapse(where_cook,
                                       indoors = c("In the house", "In a separate building"),
                                       outdoors = "Outdoors")) %>% 
  calc_tlu()


# Factorize factors -------------------------------------------------------


# Create dummy variables for categoricals ---------------------------------

# fix things that should be 0 ---------------------------------------------
hh = hh %>% 
  mutate(land_size = ifelse(owns_land == 0, 0, land_size),
         shared_toilet = ifelse(toilet_src %in% c(30, 31), 1, shared_toilet))

# Deal with NAs (replace all NAs by 0 for PCA) -----------------------------------------------------------
count_NA(hh)

# 1 separate_kitchen 7488
# 2    shared_toilet 6558
# 3       men_houses 5560
# 4       owns_house 3770
# 5     women_houses 1638
# 6        land_size  980
# 7       where_cook  158

hh_pca = hh %>% mutate_all(funs(coalesce(., 0)))

count_NA(hh_pca)

# Run PCAs -----------------------------------------------------------------
# 1) Recreate DHS PCA to double check
# 2) Create a combined PCA sans WASH, with animals broken out
# 3) removing anything < 1%
# 3) Create a combined PCA sans WASH, with animals as TLUs
# 4) Create 3 separate PCAs: infrastructure, ag assets, durable goods
calc_pct(hh) %>% filter(pct < 0.01)
