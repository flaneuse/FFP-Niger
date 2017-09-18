stats = wash %>% 
  group_by(wlth_quint_label) %>% 
  summarise(min = min(total_consumption_2010),
            max = max(total_consumption_2010),
            median = median(total_consumption_2010)) %>% 
  filter(!is.na(wlth_quint_label))

wlth = wash %>% left_join(stats) %>% filter(!is.na(wlth_quint))

label = data.frame(wlth_quint_label = c('lowest', 'low', 'middle', 'high', 'highest'), 
                   label = c('poverty line ($1.40 in 2010 USD)', rep('', 4)),
                   x = 1.5, y = 3)

annotation_colour = RdYlGn[10]

ggplot(wlth, aes(x = total_consumption_2010)) + 
  geom_density(fill = grey20K, size = 0.2, colour = grey90K) + 
  # Break point in dataset for poverty line; $1.25 in 2005 PPP is $1.40 in 2010.
  geom_vline(xintercept = 1.40, colour = annotation_colour, size = 0.25, linetype = 2) + 
  geom_text(aes(label = paste0("median: $", round(median,2)), x = median, y = 3), data = stats, 
            family = 'Lato Light') +
  geom_text(aes(label = label, x = x, y = y), data = label, 
            family = 'Lato Light', colour = annotation_colour, hjust = 0) +
  facet_wrap(~wlth_quint_label, ncol = 1) +
  theme_ygrid() +
  theme(axis.title.x = element_text(size = 14, colour = grey60K)) +
  scale_x_continuous(limits = c(0, 8), name = 'daily household consumption (2010 USD)', labels = scales::dollar) +
  ggtitle("Total household consumption, by wealth quintiles", subtitle = 'FFP Niger Household Survey Baseline')

library(data.table)
save_plot('~/Documents/Niger/exported/NER_FFPbaseline_wealthquints.pdf')
