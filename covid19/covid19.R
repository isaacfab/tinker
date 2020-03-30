######################################################################################
# Basic COVID-19 data analysis using the R package available on Github
# by Isaac J Faber
# Note: This package is mostly focused on China but there is 
# some global data available as well but not at the same resolution.
# R Package: https://github.com/GuangchuangYu/nCov2019
# Vingette is much better than the README: https://guangchuangyu.github.io/nCov2019/
#####################################################################################

remotes::install_github("GuangchuangYu/nCov2019")

library(nCov2019)
library(tidyverse)

#get the latest data which returns a complex list
covid_data_current <- get_nCov2019()

covid_data_today <- covid_data_current$areaTree$today %>% 
  mutate(name = covid_data_current$areaTree$name) %>% 
  filter(confirm > 0)

#data updates from today
covid_data_today

#just the global level data
covid_data_current <- covid_data_currentv$global %>% 
  filter(deadRate > 0) %>% 
  mutate(deadRate = as.numeric(deadRate)) %>% 
  arrange(desc(deadRate)) 

covid_data_current

p1 <- ggplot(covid_data_current, aes(x=deadRate, y=reorder(name,deadRate))) +
  geom_point(size=3, aes(colour='red')) +
  ylab("") +
  xlab("Death Rate (Percent)") +
  ggtitle("Death Rate by Country",subtitle = "Percent who die with positive test") +
  theme_bw() +
  theme(legend.position = "none")

p1

#load historical data which is more useful for global time series
covid_data <- load_nCov2019(lang = 'en', source='github')

#just get the global dataframe
covid_data <- covid_data$global

#find the current death count by country
covid_global_max <- covid_data %>% 
  group_by(country) %>% 
  summarise(current_dead = max(cum_dead)) %>% 
  arrange(desc(current_dead)) %>% 
  filter(current_dead > 0)

#current death count by country
covid_global_max

#create a dataframe for global time series
covid_global_daily <- covid_data %>% 
  group_by(time) %>% 
  summarise(current_dead = max(cum_dead)) %>% 
  arrange(desc(time))

ggplot(covid_global_daily, aes(x=time,y=current_dead, color = "red")) +
  geom_line(stat="identity") + 
  theme_bw()

covid_data_filtered <- covid_data %>% filter(cum_dead > 5)

p2 <- ggplot(covid_data_filtered, aes(x=time,y=cum_dead, fill = country)) +
  geom_area() + 
  ggtitle('Confirmed Global Deaths From COVID 19', subtitle = 'running total by countries with > 5') +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(legend.title = element_blank())

p2

library(cowplot)

plot_grid(p2, p1, labels = c('', ''))

##########################################################################################################################################################
# Basic COVID-19 data analysis using scraped data
# by Isaac J. Faber
# Note: This data is scraped from https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
##########################################################################################################################################################

rm(list=ls())

file_path = paste0('https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-',Sys.Date(),'.xls')

download.file(file_path , '~/covid.xls')

#clean some things up
covid_data <- readxl::read_xls('~/covid.xls') %>% 
  group_by(DateRep, CountryExp) %>% 
  summarize(ConfCases = sum(NewConfCases),
            Deaths = sum(NewDeaths)) %>% 
  arrange(desc(DateRep))

covid_data_global <- covid_data %>% 
 group_by(DateRep) %>% 
  summarize(ConfCases = sum(ConfCases),
            Deaths = sum(Deaths)) %>% 
  mutate(CumsumDeaths = cumsum(Deaths)) %>% 
  mutate(CumsumConfCases = cumsum(ConfCases))

covid_data_country <- covid_data %>% 
  group_by(CountryExp) %>% 
  summarize(ConfCases = sum(ConfCases),
            Deaths = sum(Deaths))

library(maps)

world_data <- map_data("world") %>% filter(region != 'Antarctica')

covid_data_country$in_world_data <- covid_data_country$CountryExp %in% world_data$region

t <- covid_data_country[covid_data_country$in_world_data == FALSE,]

covid_data_country <- covid_data_country %>% 
  mutate(CountryExp = ifelse(CountryExp == "United States of America","USA",
                             ifelse(CountryExp == "United Kingdom", "UK", CountryExp)))

world_data <- world_data %>% 
  left_join(covid_data_country, by = c("region" = "CountryExp"))


ggplot(world_data, aes(x = long, y = lat, group=group, fill = Deaths)) +
  geom_polygon(colour="#bdbdbd") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#without China

world_data_no_china <- world_data %>% 
  filter(region != "China")

ggplot(world_data_no_china, aes(x = long, y = lat, group=group, fill = Deaths)) +
  geom_polygon(colour="#bdbdbd") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

covid_data_deaths <- covid_data[covid_data$Deaths > 1 ,]

p1 <- ggplot(covid_data_deaths, aes(x=DateRep,y=Deaths, fill = CountryExp)) +
  geom_area() + 
  ggtitle('Confirmed Global Deaths From COVID 19', subtitle = 'running total by countries with > 5') +
  ylab("") +
  xlab("") +
  theme_bw() +
  theme(legend.title = element_blank())

p1
