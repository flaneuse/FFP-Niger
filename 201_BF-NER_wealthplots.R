
# Plot stunting and water access by “wealth" ------------------------------


# setup -------------------------------------------------------------------
library(tidyverse)
library(haven)
library(llamar)
library(svywrangler)
library(gridExtra)
library(grid)

data_dir = '~/Documents/USAID/Burkina Faso/processeddata/'


# Import data -------------------------------------------------------------
# https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=BM-BF
# Official 2014 World Bank exchange rate
exchange_rate = 494.415

# LSMS 2014 Burkina Faso stunting data
# Processed by Tim Essam, 4 October 2017
bfls = read_dta(paste0(data_dir, '20171004_BF_AnthroProcessed.dta')) %>% 
  filter(milieu == 2) %>% 
  mutate(pcexp_usd = pcexp / exchange_rate,
         daily_pcexp_usd = pcexp_usd / 365,
    lnpcexp_usd = log(daily_pcexp_usd),
    poor = pcexp < zref
    )

bfls = bfls %>% 
  mutate(
    stunted_poor = case_when(stunted == 1 & poor == 1 ~ 'poor and stunted',
                             stunted == 0 & poor == 1 ~ 'poor',
                             stunted == 1 & poor == 0 ~ 'stunted',
                             stunted == 0 & poor == 0 ~ 'okay',
                               TRUE ~ NA_character_)
  )
# %>% 
#   factorize('_lab', region)


# Official poverty rate = zref
pov_line = 153530 / exchange_rate / 365

dollar_breaks = c(0, 0.25, 0.5, 1, 2, 5, 10, 100)

poor_colour = '#306EA7'
stunted_colour = '#BA2932'
ps_colour = '#53205e'
okay_colour = grey50K

plot_scatter =  function(df, title, sub, alpha = 0.05) 
  {ggplot(df, aes(x = lnpcexp_usd, y = stunting)) +
  # -- data --
  geom_point(aes(fill = stunted_poor, color = stunted_poor), size = 3, 
             alpha = alpha, 
             size = 0.1, shape = 21) +
  geom_smooth(colour = grey75K, size = 1.5) +
  
  # -- quadrant annotations --
  annotate(geom = 'text', x = log(0), y = 5.75, 
           hjust = 0, vjust = 0, 
           colour = poor_colour, 
           family = 'Lato Light',
           size = 5, alpha = 0.5,
           label = stringr::str_to_upper('below poverty line')) +
  
  annotate(geom = 'text', x = log(0), y = -5.75, 
           hjust = 0, vjust = 1, 
           colour = ps_colour, 
           family = 'Lato Light',
           size = 5, alpha = 0.5,
           label = stringr::str_to_upper('below poverty line & stunted')) +
  
  annotate(geom = 'text', x = log(5), y = -5.75, 
           hjust = 1, vjust = 1, 
           colour = stunted_colour, 
           family = 'Lato Light',
           size = 5, alpha = 0.5,
           label = stringr::str_to_upper('stunted')) +
  
  annotate(geom = 'text', x = log(1), y = -1.75, 
           hjust = 0.5, vjust = 0, 
           colour = grey90K, 
           family = 'Lato',
           size = 5, 
           label = ('Fitted line')) +
  
  # -- titles --
  ggtitle(title, 
          subtitle = sub) +
  
  # -- scales --
  scale_x_continuous(breaks = log(dollar_breaks), 
                     labels = scales::dollar(dollar_breaks),
                     name = 'Daily per capita expenditure (USD 2014)') +
  
  scale_y_continuous(limits = c(-6, 6), name = 'stunting z-score') +
  
  scale_fill_manual(values = c('poor' = poor_colour, 
                               'okay' = okay_colour, 
                               'stunted' = stunted_colour,  
                               'poor and stunted' = ps_colour)) +
  
  scale_colour_manual(values = c('poor' = poor_colour, 
                               'okay' = okay_colour, 
                               'stunted' = stunted_colour,  
                               'poor and stunted' = ps_colour)) +
  
  # -- themes --
  theme_xygrid()
}

p1 = plot_scatter(bfls, 'Stunting values show little variation with household expenditures', 'Rural Burkina Faso (2014)')

p2 = plot_scatter(bfls %>% filter(region == 6), 
             alpha = 0.1,
             ' ', 'Rural Centre-Nord, Burkina Faso (2014)')

grid.arrange(p1, p2, ncol = 2)
# hist: pcexp -------------------------------------------------------------


e = ggplot(bfls, aes(x = lnpcexp_usd)) +
  
  # -- breaks --
  annotate(geom = 'rect', xmin = -Inf, xmax = log(pov_line),
           ymin = -Inf, ymax = Inf, fill = poor_colour,
           alpha = 0.25) +
  annotate(geom = 'rect', xmax = Inf, xmin = log(pov_line),
           ymin = -Inf, ymax = Inf, fill = okay_colour, alpha = 0.25) +
  # -- data --
  geom_density(colour = grey75K, size = 0.5) +
  
  
  # -- scales --
  scale_x_continuous(breaks = log(dollar_breaks), 
                     labels = scales::dollar(dollar_breaks),
                     name = 'Daily per capita expenditure (USD 2014)') +
  
  scale_y_reverse(breaks = c(0,0.5), labels = c(0,-6)) +  
  scale_fill_manual(values = c('poor' = poor_colour, 
                               'okay' = okay_colour, 
                               'stunted' = stunted_colour,  
                               'poor and stunted' = ps_colour)) +
  
  scale_colour_manual(values = c('poor' = poor_colour, 
                                 'okay' = okay_colour, 
                                 'stunted' = stunted_colour,  
                                 'poor and stunted' = ps_colour)) +
  
  # -- themes --
  theme_xygrid() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())



# hist: stunting ----------------------------------------------------------

s = ggplot(bfls, aes(x = stunting)) +
  
  # -- breaks --
  annotate(geom = 'rect', xmin = -Inf, xmax = -2,
           ymin = -Inf, ymax = Inf, fill = stunted_colour,
           alpha = 0.25) +
  annotate(geom = 'rect', xmax = Inf, xmin = -2,
           ymin = -Inf, ymax = Inf, fill = okay_colour, alpha = 0.25) +
  # -- data --
  geom_density(colour = grey75K, size = 0.5) +
  
  
  # -- scales --
  scale_x_continuous(limits = c(-6, 6),
                     name = NULL) +
  
  ggtitle('Stunting values show little variation with household expenditures', 
          subtitle = 'Rural Burkina Faso (2014)') +
  
coord_flip() +
    scale_fill_manual(values = c('poor' = poor_colour, 
                               'okay' = okay_colour, 
                               'stunted' = stunted_colour,  
                               'poor and stunted' = ps_colour)) +
  
  scale_colour_manual(values = c('poor' = poor_colour, 
                                 'okay' = okay_colour, 
                                 'stunted' = stunted_colour,  
                                 'poor and stunted' = ps_colour)) +
  
  # -- themes --
  theme_xygrid() +
  theme(axis.text.y = element_blank())

lay = rbind(c(1,1,1,1,1,2),
            c(1,1,1,1,1,2),
            c(1,1,1,1,1,2),
            c(1,1,1,1,1,2),
            c(1,1,1,1,1,2),
            c(3,3,3,3,3,4))

# blank plot
b = qplot() + theme_blank()

grid.arrange(p, s, e, b, layout_matrix = lay)


# NER: water access -------------------------------------------------------

ggplot(wash %>% filter(REG == 2), aes(x = log(total_consumption_2010), y = impr_watersrc)) +
  geom_smooth() + 
  scale_x_continuous(breaks = log(dollar_breaks), 
                     labels = scales::dollar(dollar_breaks),
                     name = 'Daily per capita expenditure (USD 2010)') +
  
  scale_y_continuous(labels = scales::percent) +
  theme_xygrid()


# NER: water access -------------------------------------------------------
ch_ffp2 = ch_ffp2 %>% 
  mutate(lnpcexp_usd = log(total_consumption_2010),
         poor = ifelse(total_consumption_2010 < 1.4, 1, 0),
         stunting = zhaz,
         stunted = stunting < -2
  ) %>% 
  mutate(
    stunted_poor = case_when(stunted == 1 & poor == 1 ~ 'poor and stunted',
                             stunted == 0 & poor == 1 ~ 'poor',
                             stunted == 1 & poor == 0 ~ 'stunted',
                             stunted == 0 & poor == 0 ~ 'okay',
                             TRUE ~ NA_character_))

  plot_scatter(ch_ffp2 %>% filter(region == 2), alpha = 0.1,
             ' ', 'Rural Zinder, Niger (2010)')
  
  plot_scatter(ch_ffp2, alpha = 0.1,
               ' ', 'Rural Zinder and Maradi, Niger (2010)')
  
  ggplot(ch_ffp2, aes(x = zhaz, y = log(total_consumption_2010))) +
           geom_point() +
           coord_flip()
