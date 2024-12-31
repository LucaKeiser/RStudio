
# Setup -------------------------------------------------------------------

# Packages
library(tidyverse)
theme_set(theme_minimal())

# Data
emissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv') %>% 
  filter(year >= 1900)




# EDA ---------------------------------------------------------------------

emissions %>% 
  glimpse()

emissions %>% 
  count(parent_entity,
        sort = TRUE)

emissions %>% 
  group_by(year) %>% 
  summarise(total_emissions_year_MtCO2e = sum(total_emissions_MtCO2e ),
            n_parent_entity = n_distinct(parent_entity),
            .groups = "drop") %>% 
  ggplot(aes(year, total_emissions_year_MtCO2e)) + 
  geom_line(linewidth = 1.25) +
  geom_point(aes(size = n_parent_entity)) + 
  labs(title = "Total emissions over time",
       subtitle = "In megatons of carbon dioxide equivalent (MtCO2e)",
       x = "Year",
       y = "MtCO2e",
       size = "Total number of\nparent entities:") + 
  scale_y_continuous(labels = scales::comma_format(big.mark = "\\'")) +
  scale_size(range = c(0.5, 4))



emissions %>% 
  group_by(year, commodity) %>% 
  summarise(avg_emisssion = mean(total_emissions_MtCO2e, 
                                 na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(commodity = fct_reorder(commodity, avg_emisssion, 
                                 .fun = last)) %>% 
  ggplot(aes(year, avg_emisssion,
             colour = commodity)) + 
  geom_line(linewidth = 1.25) +
  geom_point() +
  labs(title = "Average emission per commodity over time",
       subtitle = "In megatons of carbon dioxide equivalent (MtCO2e)",
       x = "Year",
       y = "MtCO2e",
       color = "Commodity:") + 
  guides(color = guide_legend(reverse = TRUE)) + 
  scale_size(range = c(0.5, 4)) 
