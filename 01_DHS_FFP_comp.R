library(svywrangler)
library(tidyverse)
library(llamar)

# FFP indicators pulled from prelim report, Sept 2017
ffp = tribble(
  ~ip, ~program, ~region, ~year, ~indicator, ~pct,
  "Save the Children", "Lahia", "Maradi", 2017, "water", 49.7,
  "Save the Children", "Lahia", "Maradi", 2012, "toilet", 10.5,
  "Save the Children", "Lahia", "Maradi", 2017, "toilet", 18.2,
  "Save the Children", "Lahia", "Maradi", 2012, "soap", 10,
  "Save the Children", "Lahia", "Maradi", 2017, "soap", 12.4,
  
  "CRS", "Pasam Tai", "Maradi", 2017, "water", 22.7,
  "CRS", "Pasam Tai", "Maradi", 2012, "toilet", 5.5,
  "CRS", "Pasam Tai", "Maradi", 2017, "toilet", 7.5,
  "CRS", "Pasam Tai", "Maradi", 2012, "soap", 15.1,
  "CRS", "Pasam Tai", "Maradi", 2017, "soap", 16.9,
  
  "Save the Children", "Lahia", "Maradi", 2017, "water", 48.2,
  "Save the Children", "Lahia", "Maradi", 2012, "toilet", 13.2,
  "Save the Children", "Lahia", "Maradi", 2017, "toilet", 13.6,
  "Save the Children", "Lahia", "Maradi", 2012, "soap", 22.6,
  "Save the Children", "Lahia", "Maradi", 2017, "soap", 35.9,

  "CRS", "Pasam Tai", "Zinder", 2017, "water", 22.7,
  "CRS", "Pasam Tai", "Zinder", 2012, "toilet", 5.5,
  "CRS", "Pasam Tai", "Zinder", 2017, "toilet", 7.5,
  "CRS", "Pasam Tai", "Zinder", 2012, "soap", 15.1,
  "CRS", "Pasam Tai", "Zinder", 2017, "soap", 16.9,
  
  "Save the Children", "Lahia", "Zinder", 2017, "water", 48.2,
  "Save the Children", "Lahia", "Zinder", 2012, "toilet", 13.2,
  "Save the Children", "Lahia", "Zinder", 2017, "toilet", 13.6,
  "Save the Children", "Lahia", "Zinder", 2012, "soap", 22.6,
  "Save the Children", "Lahia", "Zinder", 2017, "soap", 35.9
)

san = loadDHS(indicators = c('Households using an improved water source', 
                             'Households with improved, non-shared toilet facilities',
                             'Households with soap and water'), countries = 'Niger', breakdown = 'subnational') 
  
  san = san %>% filter(CharacteristicLabel %in% c('Maradi', '..Zinder'))

natl = loadDHS(indicators = 'improved', countries = 'Niger', breakdown = 'national')




# Toilets
ggplot(san, aes(x = SurveyYear, y = Percent)) +
  geom_line() +
  # geom_line(data = natl%>% filter(Indicator == 'Households using an improved water source')) +
  scale_y_continuous(limits = c(0, 0.7), labels = scales::percent) +
  theme_xygrid() +
  facet_wrap(~CharacteristicLabel + Indicator)


ggplot(san %>% filter(Indicator == ), aes(x = SurveyYear, y = Percent)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 0.2), labels = scales::percent) +
  theme_xygrid() +
  facet_wrap(~CharacteristicLabel)
