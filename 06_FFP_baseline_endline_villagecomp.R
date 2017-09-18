library(tidyverse)
library(readxl)
library(stringr)

endline_coords = read_excel('~/Documents/Niger/geodata/20170424_Niger Baseline Survey Listing and GPS Data for Villages_Geocenter.xlsx')


source('02_FFP_baseline_water')

baseline_coords = wash %>% group_by(admin1, admin2, admin3, village, ip) %>% count() %>% rename(n_hh = n) %>% 
  mutate(communes_std = ifelse(admin3 == 'Kanambakache', 'Kanen Bakche',
                               ifelse(admin3 == 'Kourmi', 'Kourni',
                                      ifelse(admin3 == 'Matamaye', 'Matamey',
                                             ifelse(admin3 == 'Sarkin Houssa', 'Sherken Haoussa',
                                                    ifelse(admin3 == 'Tsaouni', 'Tsouni', admin3))))))

# range of codes
range(baseline_coords$village)
range(endline_coords$village_number)

# count admin2
baseline_coords %>% ungroup() %>% count(admin2)

# count number of villages by admin1 (region) + admin 
bl = baseline_coords %>% ungroup() %>% group_by(admin1, communes_std) %>% summarise(baseline = n(), n_hh = sum(n_hh)) %>% mutate(pct = n_hh/sum(n_hh))
# bl = baseline_coords %>% ungroup() %>% group_by(admin1, communes_std, ip) %>% summarise(n_village = n(), n_hh = sum(n_hh)) %>% mutate(pct = n_hh/sum(n_hh))
el = endline_coords %>% count(regions_std, communes_std) %>% rename(endline = n)
# el = endline_coords %>% count(regions_std, communes_std, implementing_partner) %>% rename(n_endline = n)

merged = full_join(bl, el, by = c("admin1" = "regions_std", "communes_std" = "communes_std")) %>% select(-n_hh, -pct) %>% 
  gather(survey, n_villages, -admin1, -communes_std)



# dodged bar plot ---------------------------------------------------------
ggplot(merged, aes(x = forcats::fct_reorder(communes_std, n_villages), y = n_villages, fill = survey)) +
  coord_flip() +

  geom_bar(position = 'dodge', width = 0.6,
           stat = 'identity', alpha = 0.6) +
  annotate(x = 6, y = 21, label = 'DRAFT', geom = 'text',
           size = 15, alpha = 0.1) +
  annotate(y = 0, x = 1.2, label = 'endline', geom = 'text', colour = PRGn[11], family = 'Lato', hjust = 0) +
  annotate(y = 0, x = 0.85, label = 'baseline', geom = 'text', colour = PRGn[1], family = 'Lato', hjust = 0) +
  theme_xgrid() +
  
  ylab('number of villages') +
  facet_wrap(~admin1, scales = 'free_y') +
  scale_fill_manual(values = c(PRGn[3], PRGn[9])) +
  ggtitle("Number of villages sampled in each commune shifts between baseline and endline")



# check village numbers ---------------------------------------------------

bl = baseline_coords %>% ungroup() %>% select(admin1, communes_std, village) %>% mutate(rank = dense_rank(communes_std))
el = endline_coords %>% select(regions_std, communes_std, village_number) %>% mutate(rank = dense_rank(communes_std))

codes = full_join(bl, el, by = c("admin1" = "regions_std", "communes_std" = "communes_std", "rank" = "rank")) 

codes %>% 
  mutate(diff = village_number - as.numeric(village))
  

