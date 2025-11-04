library(tidyverse)
theme_set(theme_bw())

data_file <- readr::read_csv('gapminder_merged_long.csv') %>%
  dplyr::filter(year <= 2025 & year >= 1947)

data_Ind <- data_file %>% 
  dplyr::filter(name == 'India') 

plot_Ind_gdppc <- ggplot(data = data_Ind, 
                         mapping = aes(x = year, y = gdp_pcap_21)) +
  geom_point() +
  geom_line() 

plot_Ind_lifeexp <- ggplot(data = data_Ind, 
                           mapping = aes(x = year, y = life_expectancy)) +
  geom_point() +
  geom_line() 

plot_Ind_gdppc_lifeexp <- ggplot(data = data_Ind, 
                                 mapping = aes(x = gdp_pcap, 
                                               y = life_expectancy)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm', linetype = 'dotdashed') +
  labs(x = 'GDP/capita (2021 USD)',
       y = 'Life Expectancy',
       title = 'India after independence',
       )

country_set <- c('India', 'China', 'France', 'Germany', 'UK', 'USA',
                 'Brazil', 'Russia')

plot_lifeexp_country_set <- ggplot(data = data_file %>% 
                                     dplyr::filter(name %in% c('India', 'China')), 
                                   mapping = aes(x = year, 
                                                 y = life_expectancy,
                                                 color = name)
                                   ) +
  geom_point(mapping = aes(shape = name)) +
  geom_line(mapping = aes(linetype = name)) +
  geom_smooth(method = 'lm')
  