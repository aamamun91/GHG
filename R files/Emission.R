

library(tidyverse)
library(xlsx)
library(ggplot2)


yr=2015

# Emission data load ------------------------------------------------------
{
emissiondata <- read.csv('FAOSTAT_data_8-28-2020.csv') %>% select(Area, Element, Item, Year, Value) %>% filter(Year<=2015) 

emissionvolume <- emissiondata %>% filter(Element=="Emissions (CO2eq)") %>% 
                  mutate(mapitem=case_when(Item=='Cereals excluding rice'~'Other cereals',
                                           Item=='Eggs, hen, in shell'~'Eggs',
                                           Item=='Meat, pig'~'Pig meat', 
                                           Item=='Milk, whole fresh sheep'~'Milk',
                                           Item=='Milk, whole fresh buffalo'~'Milk',
                                           Item=='Milk, whole fresh camel'~'Milk',
                                           Item=='Milk, whole fresh cow'~'Milk',
                                           Item=='Milk, whole fresh goat'~'Milk',
                                           Item=='Rice, paddy'~'Rice',
                                           Item=='Meat, buffalo'~'Bovine meat',
                                           Item=='Meat, chicken'~'Poultry meat',
                                           Item=='Meat, goat'~'Bovine meat',
                                           Item=='Meat, cattle'~'Bovine meat',
                                           Item=='Meat, sheep'~'Bovine meat',
                                           TRUE~Item
                                           )) %>% select(-Item, -Element) %>% rename(emission=Value)


emissionintensity <- emissiondata %>% filter(Element=="Emissions intensity")


production <- emissiondata %>% filter(Element=="Production") %>% 
              mutate(mapitem=case_when(Item=='Cereals excluding rice'~'Other cereals',
                                       Item=='Eggs, hen, in shell'~'Eggs',
                                       Item=='Meat, pig'~'Pig meat', 
                                       Item=='Milk, whole fresh sheep'~'Milk',
                                       Item=='Milk, whole fresh buffalo'~'Milk',
                                       Item=='Milk, whole fresh camel'~'Milk',
                                       Item=='Milk, whole fresh cow'~'Milk',
                                       Item=='Milk, whole fresh goat'~'Milk',
                                       Item=='Rice, paddy'~'Rice',
                                       Item=='Meat, buffalo'~'Bovine meat',
                                       Item=='Meat, chicken'~'Poultry meat',
                                       Item=='Meat, goat'~'Bovine meat',
                                       Item=='Meat, cattle'~'Bovine meat',
                                       Item=='Meat, sheep'~'Bovine meat',
                                       TRUE~Item
              )) %>% select(-Item, -Element) %>% rename(production=Value)

emission <- emissionvolume %>% left_join(production) %>% rename(Country=Area)


# First version: developed and developing according to OECD list ----------

countrylist <- read.xlsx('Country_list_developed_developing.xlsx', sheetName = 'Sheet1') %>% rename(Country=Country.Region, economy=Economy.type)

emission_oecd <- emission %>% right_join(countrylist) %>% mutate(kgco2=emission*1000000, kgproduction=production*1000) %>% 
                 select(-emission, -production, -Country) %>% 
                 group_by(economy, mapitem, Year) %>% 
                 summarise(kgco2=sum(kgco2, na.rm = T), kgproduction=sum(kgproduction)) %>% 
                 ungroup()

table2_oecd <- emission_oecd %>% filter(Year==2015) %>% select(economy, mapitem, kgco2) %>% 
               left_join(emission_oecd %>% filter(Year==2015) %>% select(economy, mapitem, kgco2) %>% 
               group_by(economy) %>% summarise(kgco2_tot = sum(kgco2))) %>% mutate(emission_share=round(kgco2*100/kgco2_tot, 2)) %>% 
               select(economy, mapitem, emission_share) %>% 
              rbind(emission_oecd %>% filter(Year==2015) %>% select(mapitem, kgco2) %>% group_by(mapitem) %>% summarise(kgco2=sum(kgco2)) %>% ungroup() %>% 
               merge(emission_oecd %>% filter(Year==2015) %>% select(mapitem, kgco2)%>% 
                 summarise(kgco2_tot = sum(kgco2))
                 ) %>% mutate(emission_share=round(kgco2*100/kgco2_tot, 2)) %>% mutate(economy='World') %>% 
              select(economy, mapitem, emission_share)
              ) %>% 
             spread(economy, emission_share)

write.csv(table2_oecd, 'Table2.csv', row.names = F)


intensity_oecd <- emission_oecd %>% group_by(economy, mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                  ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2)) %>% select(economy, mapitem, intensity)  %>% 
                  rbind(
                   emission_oecd %>% group_by(mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                    ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2), economy='World') %>% select(economy, mapitem, intensity)
                  ) %>% 
                 spread(economy, intensity)

write.csv(intensity_oecd, 'Table3.csv', row.names = F)


# Alternate version of developed and developing ---------------------------

countrylist_alt <- read.xlsx('Country_list_developed_developing_alt.xlsx', sheetName = 'Sheet1') %>% rename(Country=Country.Region, economy=Economy.type)


emission_alt <- emission %>% left_join(countrylist_alt) %>% mutate(economy=ifelse(is.na(economy), 'Developing', economy)) %>% 
                mutate(kgco2=emission*1000000, kgproduction=production*1000) %>% 
                 select(-emission, -production, -Country) %>% 
                 group_by(economy, mapitem, Year) %>% 
                 summarise(kgco2=sum(kgco2, na.rm = T), kgproduction=sum(kgproduction)) %>% 
                 ungroup()

table2_alt <- emission_alt %>% filter(Year==2014) %>% select(economy, mapitem, kgco2) %>% 
               left_join(emission_alt %>% filter(Year==2014) %>% select(economy, mapitem, kgco2) %>% 
                          group_by(economy) %>% summarise(kgco2_tot = sum(kgco2))) %>% mutate(emission_share=round(kgco2*100/kgco2_tot, 2)) %>% 
               select(economy, mapitem, emission_share) %>% 
               rbind(emission_alt %>% filter(Year==2014) %>% select(mapitem, kgco2) %>% group_by(mapitem) %>% summarise(kgco2=sum(kgco2)) %>% ungroup() %>% 
                      merge(emission_alt %>% filter(Year==2014) %>% select(mapitem, kgco2)%>% 
                             summarise(kgco2_tot = sum(kgco2))
                      ) %>% mutate(emission_share=round(kgco2*100/kgco2_tot, 2)) %>% mutate(economy='World') %>% 
                      select(economy, mapitem, emission_share)
               ) %>% 
               spread(economy, emission_share)

write.csv(table2_alt, 'Table2_alt.csv', row.names = F)


intensity_alt <- emission_alt %>% group_by(economy, mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                   ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2)) %>% select(economy, mapitem, intensity)  %>% 
                   rbind(
                    emission_alt %>% group_by(mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                     ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2), economy='World') %>% select(economy, mapitem, intensity)
                   ) %>% 
                   spread(economy, intensity)

write.csv(intensity_alt, 'Table3_alt.csv', row.names = F)

}
# Emission data: second set of data ---------------------------------------

emissiondataII <- read.csv('FAOSTAT_data_8-31-2020 (1).csv') %>% select(Area, Element, Item, Year, Value) %>% filter(Year<=2015) %>% 
                  mutate(mapitem=case_when(Item=='Cereals excluding rice'~'Other cereals',
                                           Item=='Eggs, hen, in shell'~'Eggs',
                                           Item=='Meat, pig'~'Pig meat', 
                                           Item=='Milk, whole fresh sheep'~'Milk',
                                           Item=='Milk, whole fresh buffalo'~'Milk',
                                           Item=='Milk, whole fresh camel'~'Milk',
                                           Item=='Milk, whole fresh cow'~'Milk',
                                           Item=='Milk, whole fresh goat'~'Milk',
                                           Item=='Rice, paddy'~'Rice',
                                           Item=='Meat, buffalo'~'Bovine meat',
                                           Item=='Meat, chicken'~'Poultry meat',
                                           Item=='Meat, goat'~'Bovine meat',
                                           Item=='Meat, cattle'~'Bovine meat',
                                           Item=='Meat, sheep'~'Bovine meat',
                                           TRUE~Item
                  )) %>% 
                 filter(Element!='Emissions intensity') %>% 
                rename(Country=Area) %>% select(-Item) %>% 
                group_by(Country, mapitem, Element, Year) %>% 
                summarise(Value=sum(Value)) %>% 
                ungroup() %>% 
                spread(Element, Value) %>% rename(Emission="Emissions (CO2eq)")
                  
emissiondataII %>% filter(Year==yr) %>% group_by(mapitem) %>% summarise(ems=sum(Emission))

emission_oecdII <- emissiondataII %>% right_join(countrylist) %>% mutate(kgco2=Emission*1000000, kgproduction=Production*1000) %>% 
                   select(-Emission, -Production, -Country) %>% 
                   group_by(economy, mapitem, Year) %>% 
                   summarise(kgco2=sum(kgco2, na.rm = T), kgproduction=sum(kgproduction)) %>% 
                   ungroup()

table2_oecdII <- emission_oecdII %>% filter(Year==2015) %>% select(economy, mapitem, kgco2) %>% 
               left_join(emission_oecdII %>% filter(Year==2015) %>% select(economy, mapitem, kgco2) %>% 
                          group_by(economy) %>% summarise(kgco2_tot = sum(kgco2))) %>% mutate(emission_share=round(kgco2*100/kgco2_tot, 2)) %>% 
               select(economy, mapitem, emission_share) %>% 
               rbind(emission_oecdII %>% filter(Year==2015) %>% select(mapitem, kgco2) %>% group_by(mapitem) %>% summarise(kgco2=sum(kgco2)) %>% ungroup() %>% 
                      merge(emission_oecdII %>% filter(Year==2015) %>% select(mapitem, kgco2)%>% 
                             summarise(kgco2_tot = sum(kgco2))
                      ) %>% mutate(emission_share=round(kgco2*100/kgco2_tot, 2)) %>% mutate(economy='World') %>% 
                      select(economy, mapitem, emission_share)
               ) %>% 
               spread(economy, emission_share)

write.csv(table2_oecdII, 'Table2.csv', row.names = F)


intensity_oecdII <- emission_oecdII %>% group_by(economy, mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                  ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2)) %>% select(economy, mapitem, intensity)  %>% 
                  rbind(
                   emission_oecdII %>% group_by(mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                    ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2), economy='World') %>% select(economy, mapitem, intensity)
                  ) %>% 
                  spread(economy, intensity)

write.csv(intensity_oecdII, 'Table3.csv', row.names = F)


emission_altII <- emissiondataII %>% left_join(countrylist_alt) %>% mutate(economy=ifelse(is.na(economy), 'Developing', economy)) %>% 
                  mutate(kgco2=Emission*1000000, kgproduction=Production*1000) %>% 
                  select(-Emission, -Production, -Country) %>% 
                  group_by(economy, mapitem, Year) %>% 
                  summarise(kgco2=sum(kgco2, na.rm = T), kgproduction=sum(kgproduction)) %>% 
                  ungroup()

intensity_countryII <- emissiondataII %>% left_join(countrylist_alt) %>% mutate(economy=ifelse(is.na(economy), 'Developing', economy)) %>% 
                      mutate(kgco2=Emission*1000000, kgproduction=Production*1000) %>% select(Country, mapitem, Year, kgco2, kgproduction) %>% 
                      group_by(Country, mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                      ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2)) %>% select(Country, mapitem, intensity) %>% 
                      spread(mapitem, intensity)

write.csv(intensity_countryII, 'Table3-Emission Intensity by Country.csv', row.names = F)

table2_altII <- emission_altII %>% filter(Year==yr) %>% select(economy, mapitem, kgco2) %>% 
              left_join(emission_altII %>% filter(Year==yr) %>% select(economy, mapitem, kgco2) %>% 
                         group_by(economy) %>% summarise(kgco2_tot = sum(kgco2))) %>% mutate(emission_share=round(kgco2*100/kgco2_tot, 2)) %>% 
              select(economy, mapitem, emission_share) %>% 
              rbind(emission_altII %>% filter(Year==2014) %>% select(mapitem, kgco2) %>% group_by(mapitem) %>% summarise(kgco2=sum(kgco2)) %>% ungroup() %>% 
                     merge(emission_altII %>% filter(Year==yr) %>% select(mapitem, kgco2)%>% 
                            summarise(kgco2_tot = sum(kgco2))
                     ) %>% mutate(emission_share=round(kgco2*100/kgco2_tot, 2)) %>% mutate(economy='World') %>% 
                     select(economy, mapitem, emission_share)
              ) %>% 
              spread(economy, emission_share)

write.csv(table2_altII, 'Table2_alt.csv', row.names = F)


intensity_altII <- emission_altII %>% group_by(economy, mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2)) %>% select(economy, mapitem, intensity)  %>% 
                rbind(
                 emission_altII %>% group_by(mapitem) %>% summarise(kgco2=mean(kgco2), kgproduction=mean(kgproduction)) %>% 
                  ungroup() %>% mutate(intensity=round(kgco2/kgproduction,2), economy='World') %>% select(economy, mapitem, intensity)
                ) %>% 
                spread(economy, intensity)

write.csv(intensity_altII, 'Table3_alt.csv', row.names = F)


# GSSE --------------------------------------------------------------------

gsse <- read.xlsx('GSSE_data.xlsx', sheetName = 'Sheet1') %>% select(Country, Economy, Year, GSSE) %>% mutate(GSSE=GSSE*1000000) %>% 
        group_by(Economy, Year) %>% summarise(GSSE=sum(GSSE, na.rm = T)) %>% ungroup() %>% group_by(Economy) %>% 
        summarise(GSSE=mean(GSSE, na.rm=T)/1000000000) %>% 
        rbind(data.frame(Economy='World', GSSE=106.4164))

write.csv(gsse, 'GSSE.csv', row.names = F)



