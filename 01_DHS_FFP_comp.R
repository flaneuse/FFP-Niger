library(svywrangler)
library(tidyverse)
library(llamar)
library(stringr)

# FFP indicators pulled from prelim report, Sept 2017
ffp = tribble(
  ~ip, ~program, ~region, ~year, ~Indicator, ~pct,
  "Save the Children", "Lahia", "Maradi", 2017, "Households using an improved water source", 49.7,
  "Save the Children", "Lahia", "Maradi", 2012, "Households with improved, non-shared toilet facilities", 10.5,
  "Save the Children", "Lahia", "Maradi", 2017, "Households with improved, non-shared toilet facilities", 18.2,
  "Save the Children", "Lahia", "Maradi", 2012, "soap", 10,
  "Save the Children", "Lahia", "Maradi", 2017, "soap", 12.4,
  
  "CRS", "Pasam Tai", "Maradi", 2017, "Households using an improved water source", 22.7,
  "CRS", "Pasam Tai", "Maradi", 2012, "Households with improved, non-shared toilet facilities", 5.5,
  "CRS", "Pasam Tai", "Maradi", 2017, "Households with improved, non-shared toilet facilities", 7.5,
  "CRS", "Pasam Tai", "Maradi", 2012, "soap", 15.1,
  "CRS", "Pasam Tai", "Maradi", 2017, "soap", 16.9,
  
  "Mercy Corps", "Sawki", "Maradi", 2017, "Households using an improved water source", 48.2,
  "Mercy Corps", "Sawki", "Maradi", 2012, "Households with improved, non-shared toilet facilities", 13.2,
  "Mercy Corps", "Sawki", "Maradi", 2017, "Households with improved, non-shared toilet facilities", 13.6,
  "Mercy Corps", "Sawki", "Maradi", 2012, "soap", 22.6,
  "Mercy Corps", "Sawki", "Maradi", 2017, "soap", 35.9,

  "CRS", "Pasam Tai", "Zinder", 2017, "Households using an improved water source", 22.7,
  "CRS", "Pasam Tai", "Zinder", 2012, "Households with improved, non-shared toilet facilities", 5.5,
  "CRS", "Pasam Tai", "Zinder", 2017, "Households with improved, non-shared toilet facilities", 7.5,
  "CRS", "Pasam Tai", "Zinder", 2012, "soap", 15.1,
  "CRS", "Pasam Tai", "Zinder", 2017, "soap", 16.9,
  
  "Mercy Corps", "Sawki", "Zinder", 2017, "Households using an improved water source", 48.2,
  "Mercy Corps", "Sawki", "Zinder", 2012, "Households with improved, non-shared toilet facilities", 13.2,
  "Mercy Corps", "Sawki", "Zinder", 2017, "Households with improved, non-shared toilet facilities", 13.6,
  "Mercy Corps", "Sawki", "Zinder", 2012, "soap", 22.6,
  "Mercy Corps", "Sawki", "Zinder", 2017, "soap", 35.9
) %>% 
  mutate(pct = pct/100) %>% 
  filter(Indicator != 'soap')


indic =  c('Households using an improved water source', 
           'Households with improved, non-shared toilet facilities',
           'Households with soap and water')

san = loadDHS(indicators = indic, countries = 'Niger', breakdown = 'subnational') 
  
san = san %>% filter(CharacteristicLabel %in% c('Maradi', '..Zinder')) %>% 
  mutate(region = str_replace_all(CharacteristicLabel, '\\.', ''))

natl = loadDHS(indicators = indic, countries = 'Niger', breakdown = 'background') %>% 
  select(SurveyYear, Indicator, total = Percent)

san = left_join(san, natl, by = c('SurveyYear', 'Indicator')) %>% 
  mutate(year = SurveyYear, pct = Percent)

# Toilets

plot_san = function(san, ffp, indicator, ylim, break_lim){
  san = san %>% filter(Indicator == !!indicator)
  ffp = ffp %>% filter(Indicator == !!indicator)
  
ggplot(san, aes(x = year, y = pct)) +
  # geom_line(aes(y = total), size = 1.5, colour = grey20K) +
  geom_line(colour = grey75K, size = 0.5) +
  geom_point(size = 2.5, shape = 21, fill = grey25K, colour = grey75K) +
  
  geom_line(aes(colour = ip), size = 0.5, linetype = 2, data = ffp, alpha = 0.4) +
  geom_point(fill = 'white', size = 3.5, shape = 21, data = ffp, stroke = 0) +
  geom_point(aes(fill = ip), size = 3.5, shape = 21, data = ffp, alpha = 0.4) +
  
  geom_text(aes(x = 2017, label = program, colour = ip), data = ffp %>% filter(year == 2017), 
            family = 'Lato Light', size = 4, nudge_x = 0.5,
            hjust = 0) +
  
  geom_text(aes(x = 2012), label = 'DHS', data = san %>% filter(year == 2012),
            colour = grey40K,
            family = 'Lato Light', size = 4, nudge_x = 0.5,
            hjust = 0) +
  scale_y_continuous(limits = c(0, ylim), breaks = seq(0, ylim, by = break_lim), labels = scales::percent, name = ' ') +
  scale_x_continuous(limits = c(1992, 2023), breaks = c(1992, 1998, 2006, 2012, 2017)) +
  theme_ygrid() +
  facet_wrap(~Indicator + region)
}

extrafont::loadfonts()

plot_san(san, ffp, indic[1], 0.75, 0.15)
ggsave('~/Desktop/FFP_water.pdf',            useDingbats = FALSE, width = 12, height = 7)
plot_san(san, ffp, indic[2], 0.2, 0.05)
ggsave('~/Desktop/FFP_toilets.pdf',            useDingbats = FALSE, width = 12, height = 7)
