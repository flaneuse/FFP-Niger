# Recreate DHS Wealth Index for Niger, 2012 -------------------------------
# Niger DHS Wealth Index is a PCA combination of 115 separate variables (or dummies)
# It also includes WASH components, including improved sanitation and water source.
# To be able to probe the influence of wealth on WASH access, need to recreate a new, standalone wealth index.

# Secondary purpose: calculate WASH indicators at rural / Zinder level.

# Laura Hughes, lhughes@usaid.gov, 20 September 2017

# Goal: get water and sanitation access rates for Zinder province of Niger, broken down by wealth quintiles.

# DHS data downloaded from DHS website around 15 September 2017


# setup -------------------------------------------------------------------

library(haven)
library(tidyverse)
library(svywrangler)
library(forcats)
library(llamar)
library(ggplot2)

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
  # only rural houses
  # filter(hv025 == 2) %>%
  mutate(
    # calculate number sleeping as number of de jure members, unless de jure = 0.  Then go w/ defacto.
    num_sleep = ifelse(hv012 == 0, hv013, hv012),
    
    # convert to proper decimals
    hh_wt = hv005 / 1e6,
    
    rural = ifelse(hv025 == 2, 1, 0)
  ) %>% 
  select(
    # hh sampling attributes
    cluster = hv001, hh_num = hv002, psu = hv021, strata = hv022,
    hh_wt, int_month = hv006, head_sex = hv219,
    
    # geography
    region = hv024, rural,
    
    # infrastructure:
    hh_size = hv009, num_sleep, num_rooms = hv216, elec = hv206, 
    where_cook = hv241,
    floor_type = hv213, wall_type = hv214, roof_type = hv215,
    
    # WASH:
    drinking_src = hv201, time2water = hv204, 
    toilet_src = hv205, shared_toilet = hv225,
    water_shortage = sh106b,
    
    # ag assets:
    owns_land = hv244, land_size = hv245,
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
    
    WI_DHS_rural = hv271r,
    WI_DHS = hv271) 

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
                  mobile, watch, animal_cart, where_cook,
                  water_shortage,
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
  mutate(ppl_room = ifelse(num_rooms > 0, trunc(num_sleep / num_rooms), num_sleep),
         
         # water is within the premises --> within 30 min.
         water_wi30 = as.numeric(time2water <= 30 | time2water == 996))

hh %>% count_value(8)
hh %>% count_value(9)
hh %>% count_value(98)
hh %>% count_value(99)
hh %>% count_value(998)
hh %>% count_value(999)



# classify animals, convert to decimals --------------------------------------------------------

# Breaks according to DHS: http://www.dhsprogram.com/programming/wealth%20index/Niger%20DHS%202012/niger%202012%20sps.pdf
hh = hh %>% 
  # convert to decimals
  mutate(land_size = land_size/10,
         WI_DHS_rural = WI_DHS_rural / 1e5 ) %>% 
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
  calc_tlu()


# Factorize factors -------------------------------------------------------
hh = hh %>% 
  factorize('_lab', toilet_src, drinking_src, wall_type, roof_type, floor_type, cooking_fuel, where_cook, region)

# Lump factors ------------------------------------------------------------
lump_thresh = 0.05

# fix things that should be 0 ---------------------------------------------
hh = hh %>% 
  mutate(land_size = ifelse(owns_land == 0, 0, land_size),
         shared_toilet = ifelse(toilet_src %in% c(30, 31), 1, shared_toilet))

hh = hh %>% 
  # Clumped together based on frequency, with oversight to make sure the categories roughly make sense
  mutate(wall_type_clumped = fct_lump(wall_type_lab, n = 3),
         roof_type_clumped = fct_lump(roof_type_lab, prop = lump_thresh),
         floor_type_clumped = fct_lump(floor_type_lab, prop = lump_thresh),
         cooking_fuel_clumped = fct_lump(cooking_fuel_lab, prop = lump_thresh),
         where_cook_clumped = fct_lump(where_cook_lab, n = 1),
         
         # DHS PCA groupings: don't group together classes
         toilet_src_dhs = toilet_src_lab,
         drinking_src_dhs = drinking_src_lab,
         cooking_fuel_dhs = cooking_fuel_lab,
         floor_type_dhs = floor_type_lab,
         
         # combine thatch/palm/leaves with sod
         roof_type_dhs = fct_collapse(roof_type_lab,
                                      `Thatch/Palm/Leaves/Sod` = c('Thatch/Palm/Leaves', 'Sod')),
         
         # combine Bamboo/Cane/Palm/Trunks with Dirt, Straw
         wall_type_dhs = fct_collapse(wall_type_lab,
                                      Cane_Dirt_Straw = c('Bamboo/Cane/Palm/Trunks', 'Dirt', 'Straw')),
         
         # DHS combines shared toilet + toilet_src BUT without "no facility", since all shared.
         shared_toilet_dhs = ifelse(toilet_src_lab == 'No facility/bush/field', NA, 
                                    ifelse(shared_toilet == 1, 
                                           paste0(toilet_src_dhs, ' shared'), NA)),
         
         # create improved water, toilet variables
         # classify water and sanitation access
         impr_toilet_src = ifelse(is.na(toilet_src), NA,
                                  ifelse(toilet_src %in% impr_toilet_codes, 1, 0)),
         impr_water_src = ifelse(is.na(drinking_src), NA,
                                 ifelse(drinking_src %in% impr_water_codes, 1, 0)),
         
         impr_toilet = ifelse(impr_toilet_src == 1 & shared_toilet == 0, 1, 0),
         impr_water = ifelse(impr_water_src == 1 & water_wi30 == 1, 1, 0),
         
         # create open defecation variable
         od = ifelse(is.na(toilet_src_lab), NA, 
                     ifelse(toilet_src_lab == 'No facility/bush/field', 1, 0))
  )

# Create dummy variables for categoricals ---------------------------------
hh = hh %>% 
  dummize(remove_factors = FALSE, 
          floor_type_clumped, wall_type_clumped, roof_type_clumped,
          where_cook_clumped, cooking_fuel_clumped,
          floor_type_dhs, wall_type_dhs, roof_type_dhs,
          drinking_src_dhs, toilet_src_dhs, shared_toilet_dhs,
          cooking_fuel_dhs,
          cow_cat, camel_cat, equine_cat, goat_cat, 
          sheep_cat, chicken_cat, duck_cat)




# Deal with NAs (replace all NAs by 0 for PCA) -----------------------------------------------------------
count_NA(hh)

# 1 separate_kitchen 7488
# 2    shared_toilet 6558
# 3       men_houses 5560
# 4       owns_house 3770
# 5     women_houses 1638
# 6        land_size  980
# 7       where_cook  158

hh_pca = hh %>% mutate_if(funs(coalesce(., 0)), .predicate = is.numeric)

count_NA(hh_pca)




# Run PCAs -----------------------------------------------------------------

# # 1) Recreate DHS PCA to double check
# # 2) Create a combined PCA sans WASH, with animals broken out
# # 3) removing anything < 1%
# # 3) Create a combined PCA sans WASH, with animals as TLUs
# # 4) Create 3 separate PCAs: infrastructure, ag assets, durable goods
# 
# pca1_vars = hh_pca %>% 
#   select(  # infrastructure + WASH:
#     owns_house, ppl_room, elec, 
#     contains('_dhs'), -WI_DHS_rural, -WI_DHS, -toilet_src_dhs,
#     -drinking_src_dhs, -cooking_fuel_dhs, -floor_type_dhs,
#     -roof_type_dhs, -wall_type_dhs, -shared_toilet_dhs,
#     
#     # ag assets:
#     owns_land, land_size,
#     # assuming "horses/ donkeys/ mules" is an equal distribution of all 3 animals, when calculating TLUs
#     contains('cat'), -cow_cat, -camel_cat, -chicken_cat, -equine_cat, -goat_cat, -duck_cat,-sheep_cat,
#     
#     # durable assets:
#     radio, tv, refrigerator, 
#     bicycle, motorcycle, car,
#     canoe, cyclomotor,
#     telephone, mobile, vcr, computer,
#     ac, antenna,
#     oven, 
#     watch, 
#     animal_cart, plow,motor_pump,
#     # banking 
#     bank_acct)
#   
#   pca1 = pca1_vars %>% 
#   calc_idx(save_params = T, var_name = 'WI_DHS_calc', center = T, scale = T)
# 
#   
#   bind_cols(hh, pca1$data) %>% 
#     ggplot(., aes(x = WI_DHS_rural, y = WI_DHS_calc)) +
#     geom_point() +
#     theme_xygrid() +
#     coord_equal()
#   
#   # ggplot(x, aes(x = `pca_new.scores...1.`)) + 
#   #   geom_histogram(fill = 'dodgerblue', binwidth = 0.25) + 
#   #   geom_histogram(aes(x = WI_DHS_rural), fill = 'coral', data = hh, alpha = 0.5, binwidth = 0.25) + 
#   #   theme_xgrid()
#   # 
# pca2 = hh_pca %>% 
#   select(  # infrastructure:
#     owns_house, ppl_room, elec, 
#     # where_cook = hv241,
#     contains('floor_type'), -floor_type, 
#     contains('wall_type'), -wall_type, 
#     contains('roof_type'), -roof_type, 
#     
#     
#     # # WASH:
#     # drinking_src = hv201, time2water = hv204, 
#     # toilet_src = hv205, shared_toilet = hv225,
#     
#     # ag assets:
#     owns_land, land_size,
#     # assuming "horses/ donkeys/ mules" is an equal distribution of all 3 animals, when calculating TLUs
#     contains('cat'), -cow_cat, -camel_cat, -chicken_cat, -equine_cat, -goat_cat, -duck_cat,-sheep_cat,
#     
#     # durable assets:
#     radio, tv, refrigerator, 
#     bicycle, motorcycle, car,
#     canoe, cyclomotor,
#     telephone, mobile, vcr, computer,
#     ac, antenna,
#     oven, contains('cooking_fuel'), -cooking_fuel,
#     watch, 
#     animal_cart, plow,motor_pump,
#     # num_bednets, owns_bednet
#     # 
#     # banking 
#     bank_acct) %>% 
#   calc_idx(save_params = T)
# 
# 
# 
# pca3 = hh_pca %>% 
#   select(  # infrastructure:
#     owns_house, ppl_room, elec, 
#     # where_cook = hv241,
#     contains('floor_type'), -floor_type, 
#     contains('wall_type'), -wall_type, 
#     contains('roof_type'), -roof_type, 
#     
#     
#     # # WASH:
#     # drinking_src = hv201, time2water = hv204, 
#     # toilet_src = hv205, shared_toilet = hv225,
#     
#     # ag assets:
#     owns_land, land_size,
#     # assuming "horses/ donkeys/ mules" is an equal distribution of all 3 animals, when calculating TLUs
#     TLU,
#     
#     # durable assets:
#     radio, tv, refrigerator, 
#     bicycle, motorcycle, car,
#     canoe, cyclomotor,
#     telephone, mobile, vcr, computer,
#     ac, antenna,
#     oven, contains('cooking_fuel'), -cooking_fuel,
#     watch, 
#     animal_cart, plow,motor_pump,
#     # num_bednets, owns_bednet
#     # 
#     # banking 
#     bank_acct) %>% 
#   calc_idx(save_params = T)
# 
# 
# # Export for TE to run in Stata -------------------------------------------
# 
# 
pca4 = hh %>%
  select(
    hh_num, cluster,
    # infrastructure:
    # owns_house, # ignoring b/c 3,770 NAs
    ppl_room, elec,
    contains('clumped'), -wall_type_clumped, -roof_type_clumped, floor_type_clumped,
    -cooking_fuel_clumped, -where_cook_clumped,


    # ag assets:
    owns_land, 
    # land_size, 
    # assuming "horses/ donkeys/ mules" is an equal distribution of all 3 animals, when calculating TLUs
    TLU,

    # durable assets:
    radio, tv,
    bicycle, motorcycle,
    mobile, vcr,
    watch,
    animal_cart, plow,motor_pump,
    owns_bednet) 
  

# 
# count_NA(pca4)
# 
# calc_pct(pca4) %>% filter(pct< 0.01)
# 
# # write.csv(pca4, '~/Documents/Niger/data/NER_DHS_2012_PCA_LDH.csv')

# Time to water bar graph -------------------------------------------------
hh = hh %>% 
  mutate(time_cat = cut(time2water, breaks = c(seq(0, 60, by = 15), 899)))

ggplot(hh %>% filter(region == 7, !is.na(time_cat)), aes(x = time_cat, y = (..count..)/sum(..count..))) + 
  geom_bar() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5), name = NULL) +
  ggtitle('Distance to travel to aquire drinking water', subtitle = 'Percent of rural households in Zinder, Niger (2012 DHS)') +
  theme_ygrid()



# Calculations ------------------------------------------------------------

# -- HH --
# National
lapply(c('od', 'impr_toilet', 'impr_water', 'impr_water_src'), function(x) calcPtEst(hh, var = x, use_weights = TRUE, 
                                                                                     psu_var = 'psu', strata_var = 'strata', weight_var = 'hh_wt'))

# Urban / Rural
lapply(c('od', 'impr_toilet', 'impr_water', 'impr_water_src'), function(x) calcPtEst(hh, var = x, by_var = 'rural', use_weights = TRUE, 
                                                                                     psu_var = 'psu', strata_var = 'strata', weight_var = 'hh_wt'))

# Rural, by regions
lapply(c('od', 'impr_toilet', 'impr_water', 'impr_water_src'), function(x) calcPtEst(hh %>% filter(rural == 1), var = x, by_var = 'region_lab', use_weights = TRUE, 
                                              psu_var = 'psu', strata_var = 'strata', weight_var = 'hh_wt'))

