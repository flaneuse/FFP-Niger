
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
# based on http://www.who.int/water_sanitation_health/monitoring/oms_brochure_core_questionsfinal24608.pdf
# ** Must not be shared with other houses **

impr_toilet_codes = c(11,12,13,15,21,22,31)

#   | Qnum | Type                                   | Details                            | Code | Classification |
#   |------|----------------------------------------|------------------------------------|------|----------------|
#   | F11  | Flush Toilet/Manual Flushing Connected | to a sewer system                  | 11   | Improved       |
#   | F11  | Flush Toilet/Manual Flushing Connected | to a septic tank                   | 12   | Improved       |
#   | F11  | Flush Toilet/Manual Flushing Connected | to a cesspit                       | 13   | Improved       |
#   | F11  | Flush Toilet/Manual Flushing Connected | to something else                  | 14   | Unimproved       |
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

# -- Household wealth values --


# Import data, pkgs -------------------------------------------------------
data_dir = '~/Documents/USAID/Niger/data/'
# data_dir = '~/Documents/Niger/data/'

library(tidyverse)
library(llamar)
library(svywrangler)
library(stringr)

# Data downloaded 8 September 2017 from USAID Open Data portal
# https://www.usaid.gov/data/dataset/08ee315f-c339-4c2f-8ae8-8988c9ef05ff
# WASH/Maternal health modules
base = read_csv(paste0(data_dir, 'Niger_Sanitation and Maternal Health_Data.csv'))

# Food/household consumption modules
wlth = read_csv(paste0(data_dir, 'Niger_Food Consumption_Data.csv'))


# Deal with labels --------------------------------------------------------


# pull out the coded values for the variables
base_labels = read_csv(paste0(data_dir, 'Niger_Sanitation and Maternal Health_Codebook_Values.csv'),
                       skip = 1) %>% 
  fill(`Value`) %>% 
  rename(variable = Value, value = X2, label = Label) %>% 
  mutate(label = str_to_title(label))

# create dictionaries for each:
ip_dict = base_labels %>% filter(variable == 'PVO')
sex_dict = base_labels %>% filter(variable == 'gendered_hh')
a1_dict = base_labels %>% filter(variable == 'REG')
a2_dict = base_labels %>% filter(variable == 'A04b')
a3_dict = base_labels %>% filter(variable == 'A04a')
vill_dict = base_labels %>% filter(variable == 'VN')
water_dict = base_labels %>% filter(variable == 'F04')
toilet_dict = base_labels %>% filter(variable == 'F11')


# Data cleanup ------------------------------------------------------------

wash = base %>% 
  select(hhid = HH, PVO, strata, cluster, weight = HHWT,
         VN, A04a, A04b,
         REG, gendered_hh, 
         improved_source, improved_sanitation, soap_water,
         contains('critical_handwashing'),
         F04, F05, F06, F07, F08, F09, contains('F10'),
         F11, F12, F13, F14, F15, F16, contains('F17')) %>% 
  mutate(
    # create binaries for whether toilet/water source is improved
    impr_watersrc = ifelse(is.na(F04), NA, ifelse(F04 %in% impr_water_codes, 1, 0)),
    impr_toiletsrc = ifelse(is.na(F11), NA, ifelse(F11 %in% impr_toilet_codes, 1, 0)),
    
    # binary for whether water is within 30 min.
    # Either time (F06) is <= 30, or location is within dwelling or yard (F05 == 1 | 2)
    water_wi30min = ifelse(F06 == 998, NA, ifelse(F06 <= 30 | F05 < 3, 1, 0)),
    water_elsewhere = ifelse(F05 == 3, 1, 0),
    
    # binary for if water is treated
    water_treated = ifelse(F09 == 8, NA, ifelse(F09 == 1, 1, 0)),
    
    # binary for whether toilet is shared
    shared_toilet = ifelse(F12 == 1, 1, 
                           ifelse(F12 == 2, 0, NA)),
    
    # convert to labels
    ip = plyr::mapvalues(PVO, from = ip_dict$value, to = ip_dict$label),
    
    hh_gender = plyr::mapvalues(gendered_hh, from = sex_dict$value, to = sex_dict$label),
    
    admin1 = plyr::mapvalues(REG, from = a1_dict$value, to = a1_dict$label),
    admin2 = plyr::mapvalues(A04b, from = a2_dict$value, to = a2_dict$label),
    admin3 = plyr::mapvalues(A04a, from = a3_dict$value, to = a3_dict$label),
    village = plyr::mapvalues(VN, from = vill_dict$value, to = vill_dict$label),
    
    water_src = plyr::mapvalues(F04, from = water_dict$value, to = water_dict$label),
    toilet_src = plyr::mapvalues(F11, from = toilet_dict$value, to = toilet_dict$label)
  )

wlth = wlth %>% 
  select(hhid = HH, PVO, strata, cluster, weight = HHWT,
         VN, A04b, A04a,
         REG, gendered_hh, 
         total_consumption_2010, poverty, assets_total) %>% 
  mutate(wlth_quint = ntile(total_consumption_2010, 5),
         wlth_quint_label = factor(wlth_quint, levels = 1:5, labels = c('lowest', 'low', 'middle', 'high', 'highest')))


wash = left_join(wash, wlth)

# Improved water/toilet calculations ---------------------------------------------
wash = wash %>% 
  mutate(impr_water_lh = ifelse(impr_watersrc == 1 & water_wi30min == 1, 1,
                                0),
         check_water = impr_watersrc == improved_source,
         
         impr_toilet_lh = ifelse(impr_toiletsrc == 1 & shared_toilet == 0, 1, 0),
         check_toilet = impr_toilet_lh == improved_sanitation,
         
         water_ffp = ifelse(impr_watersrc == 1 & F07 == 1 & F08 == 2, 1, 0)
  )


wash %>% count(check_toilet)

# View(wash %>% filter(is.na(check_toilet)) %>% select(improved_sanitation, impr_toilet_lh, shared_toilet, F11, F12))
# View(wash %>% filter(check_toilet == F) %>% select(improved_sanitation, impr_toilet_lh, shared_toilet, F11, F12))


# Dot plot ----------------------------------------------------------------

plot_avg = function(df, var, grp, title, subtitle, palette = YlGnBu, pal_limits = c(0, 0.65)){
  
  avgs = svywrangler::calcPtEst(df, var, by_var = grp, use_weights = TRUE, 
                                psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')
  yvar =  paste0('forcats::fct_reorder(', grp, ', avg)')
  
  ggplot(avgs, aes_string(x = 'avg', y = yvar, fill = 'avg')) +
    geom_segment(aes_string(x = 'lb', xend = 'ub', y = yvar, yend = yvar), 
                 size = 2, colour = grey20K) +
    geom_point(size = 6, colour = grey90K, stroke = 0.25, shape = 21) +
    scale_fill_gradientn(colours = palette, limits = pal_limits) +
    scale_x_continuous(labels = scales::percent, limits = pal_limits) +
    ggtitle(title, subtitle = subtitle) +
    theme_xgrid()
}

# Calculate improved toilets ----------------------------------------------
# Double checking numbers
# According to the ICF Endline report, the numbers should be: Save the Children 0.105; CRS = 0.055; Mercy Corps = 0.132
# Numbers check out (11 September 2017)

# -- by IP (FFP def) --
# FFP definition of improved sanitation, pre-calculated for us.
# Virtually indistinguishable; difference is in the 2 people who have a piped system to "don't know where"
# JMP/WHO classifies as improved; going w/ JMP definition.
svywrangler::calcPtEst(wash, 'improved_sanitation', by_var = 'ip', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')

# -- by IP (JMP def) --
svywrangler::calcPtEst(wash, 'impr_toilet_lh', by_var = 'ip', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')
plot_avg(wash, 'impr_toilet_lh', 'ip', 'Improved toilet access by IP', 'FFP Baseline data', palette = YlOrBr, pal_limits = c(0, 0.25))

# -- by gender of head --
# plot_avg(wash, 'impr_toilet_lh', 'hh_gender', 'Improved toilet access by gender of head', 'FFP Baseline data', palette = YlOrBr, pal_limits = c(0, 0.4))

# -- by Wealth --
plot_avg(wash, 'impr_toilet_lh', 'wlth_quint_label', 'Improved toilet access by wealth quintiles', 'FFP Baseline data; wealth is based on estimated consumption', palette = YlOrBr, pal_limits = c(0, 0.25))


# -- by geo --
plot_avg(wash, 'impr_toilet_lh', 'admin1', 'Improved toilet access by region', 'FFP Baseline data', palette = YlOrBr, pal_limits = c(0, 0.25))
plot_avg(wash, 'impr_toilet_lh', 'admin2', 'Improved toilet access by region', 'FFP Baseline data', palette = YlOrBr, pal_limits = c(0, 0.25))

wash = wash %>% 
  mutate(OD = ifelse(toilet_src == 'No Facility/Bush/Field', 1, 0))

svywrangler::calcPtEst(wash, 'OD', by_var = 'admin1', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')


svywrangler::calcPtEst(wash, 'impr_water_lh', by_var = 'admin1', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')


# dot matrix of toilets ---------------------------------------------------

toilet_src = wash %>% 
  filter(!is.na(toilet_src)) %>% 
  count(toilet_src, shared_toilet, impr_toilet_lh, impr_toiletsrc) %>% 
  mutate(shared_toilet = coalesce(shared_toilet, -1))


ggplot(toilet_src, aes(y = forcats::fct_reorder(toilet_src, n),
                       x = shared_toilet, size = n, colour = as.factor(impr_toiletsrc))) + 
  geom_point() + scale_size_continuous(range = c(1, 40)) +
  geom_text(aes(label = n), size = 2, colour = grey90K) +
  scale_x_continuous(labels = c('unknown', 'unshared', 'shared'), breaks = c(-1, 0, 1), 
                     position = 'top',
                     expand = c(0.2, 0)) +
  scale_y_discrete(expand = c(0.1, 0), name = '') +
  theme_ygrid() +
  ggtitle("Type of toilets in baseline data")


toilet_src = wash %>% 
  filter(!is.na(toilet_src)) %>% 
  count(toilet_src, shared_toilet, impr_toilet_lh, impr_toiletsrc, admin1) %>% 
  mutate(shared_toilet = coalesce(shared_toilet, -1))


ggplot(toilet_src, aes(y = forcats::fct_reorder(toilet_src, n),
                       x = shared_toilet, size = n, colour = as.factor(impr_toiletsrc))) + 
  geom_point() + scale_size_continuous(range = c(1, 40)) +
  geom_text(aes(label = n), size = 2, colour = grey90K) +
  scale_x_continuous(labels = c('unknown', 'unshared', 'shared'), breaks = c(-1, 0, 1), 
                     position = 'top',
                     expand = c(0.2, 0)) +
  scale_y_discrete(expand = c(0.1, 0), name = '') +
  theme_ygrid() +
  ggtitle("Type of toilets in baseline data") +
  facet_wrap(~admin1)

# Calculate improved water sources ----------------------------------------------

# Using JMP definition: improved and within 30 min.
# -- by IP --
plot_avg(wash, 'impr_water_lh', 'ip', 'Improved water access by IP', 'FFP Baseline data')
svywrangler::calcPtEst(wash, 'impr_water_lh', by_var = 'ip', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')

# -- by gender of head --
plot_avg(wash, 'impr_water_lh', 'hh_gender', 'Improved water access by gender of head', 'FFP Baseline data')


# -- by Wealth --
plot_avg(wash, 'impr_water_lh', 'wlth_quint_label', 'Improved water access by by wealth quintiles', 'FFP Baseline data; wealth is based on estimated consumption')

# -- by geo --
plot_avg(wash, 'impr_water_lh', 'admin1', 'Improved water access by region', 'FFP Baseline data')
plot_avg(wash, 'impr_water_lh', 'admin2', 'Improved water access by region', 'FFP Baseline data')

svywrangler::calcPtEst(wash, 'impr_water_lh', by_var = 'admin1', use_weights = TRUE, 
                       psu_var = 'cluster', strata_var = 'strata', weight_var = 'weight')




# dot plot of water sources -----------------------------------------------

water_src = wash %>% 
  filter(!is.na(water_src)) %>% 
  count(water_src, water_wi30min, impr_water_lh, impr_watersrc) %>% 
  mutate(water_wi30min = coalesce(water_wi30min, -1))

ggplot(water_src, aes(y = forcats::fct_reorder(water_src, n),
                      x = water_wi30min, size = n, colour = as.factor(impr_watersrc))) + 
  geom_point() + scale_size_continuous(range = c(1, 20)) +
  geom_text(aes(label = n), size = 2, colour = grey90K) +
  scale_x_continuous(labels = c('unknown', '> 30 min. away', 'within 30 min.'), 
                     position = 'top',
                     breaks = c(-1, 0, 1), expand = c(0.2, 0)) +
  scale_y_discrete(expand = c(0.1, 0), name = '') +
  theme_ygrid() +
  ggtitle("Water sources in baseline data") 
water_src = wash %>% 
  filter(!is.na(water_src)) %>% 
  count(water_src, water_wi30min, impr_water_lh, impr_watersrc, admin1) %>% 
  mutate(water_wi30min = coalesce(water_wi30min, -1))

ggplot(water_src, aes(y = forcats::fct_reorder(water_src, n),
                       x = water_wi30min, size = n, colour = as.factor(impr_watersrc))) + 
  geom_point() + scale_size_continuous(range = c(1, 20)) +
  geom_text(aes(label = n), size = 2, colour = grey90K) +
  scale_x_continuous(labels = c('unknown', '> 30 min. away', 'within 30 min.'), 
                     position = 'top',
                     breaks = c(-1, 0, 1), expand = c(0.2, 0)) +
  scale_y_discrete(expand = c(0.1, 0), name = '') +
  theme_ygrid() +
  ggtitle("Water sources in baseline data") +
  facet_wrap(~admin1)


# look stunting by wealth -------------------------------------------------
ch_ffp = read_csv(paste0(data_dir, '/Niger_Child Health_Data.csv'))
library(lubridate)

ch_ffp = ch_ffp %>% mutate(int_date2 = mdy(int_date))

ggplot(ch_ffp %>% filter(!is.na(diarrhea)), aes(x = int_date2, y = diarrhea)) +
  stat_summary(fun.y = 'mean', geom = 'point', size = 5)

ch_ffp2 = left_join(ch_ffp, wlth,  by = c("HH" = "hhid", "cluster" = "cluster"))


ggplot(ch_ffp2, aes(x = total_consumption_2010)) + geom_density()
# zwhz
# DDseven

wlth_breaks = ch_ffp2 %>% 
  group_by(wlth_quint) %>% 
  summarise(min = min(total_consumption_2010),
            max = max(total_consumption_2010))

stunted_colour = '#a50026'

ggplot(ch_ffp2, aes(x = total_consumption_2010, y = zhaz)) + 
  geom_rect(aes(xmin = 0, xmax = 10, ymin = -3, ymax = -2), 
            data = ch_ffp2 %>% slice(c(1,8797)),
            fill = stunted_colour, alpha = 0.2) + 
  
  geom_vline(aes(xintercept = min), data = wlth_breaks,
             colour = grey90K, size = 0.25) +
  geom_smooth(colour = stunted_colour) +
  scale_x_continuous(labels = scales::dollar) +
  # scale_y_continuous(limits = c(-3, -1)) +
  facet_wrap(~REG.x) +
  theme_xylab()


ggplot(ch_ffp2, aes(x = as.factor(wlth_quint), y = zhaz, fill = wlth_quint)) +
geom_violin()
