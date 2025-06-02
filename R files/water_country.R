

library(tidyverse)
library(readxl)
library(ggplot2)
library(foreign)
library(haven)
library(readstata13)
library(zoo)
library(lubridate)


fao_country <- read_excel('Water_Country.xlsx', sheet = 'Sheet1') %>% rename(ISO3 = 'ISO3 CODE', Country='FAO_Country')
water_country <- read_excel('Water_Country.xlsx', sheet = 'Sheet2') %>% rename(Country='Water_Country')

country_merge <- fao_country %>% right_join(water_country) %>% filter(is.na(ISO3))

write.csv(country_merge, 'water_country_iso3.csv', row.names = FALSE)