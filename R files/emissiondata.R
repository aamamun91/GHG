
library(tidyverse)
library(readxl)
library(ggplot2)
library(FAOSTAT)
library(WDI)

region.fao<-FAOSTAT::FAOregionProfile%>%
 select(FAOST_CODE,ISO3_CODE,OFFICIAL_FAO_NAME,UNSD_SUB_REG_CODE,UNSD_SUB_REG,SOFI_SUB_REG_CODE,SOFI_SUB_REG)%>%
 mutate(UNSD_SUB_REG_CODE=ifelse( ISO3_CODE %in% c("CHN","TWN","MAC","SGP","HKG")  , "5302", UNSD_SUB_REG_CODE),
        UNSD_SUB_REG=ifelse( ISO3_CODE %in% c("CHN","TWN","MAC","SGP","HKG")  , "Eastern Asia", UNSD_SUB_REG),
        SOFI_SUB_REG_CODE=ifelse( ISO3_CODE %in% c("CHN","TWN","MAC","SGP","HKG")  , "5834", SOFI_SUB_REG_CODE),
        SOFI_SUB_REG=ifelse( ISO3_CODE %in% c("CHN","TWN","MAC","SGP","HKG")  , "East Asia", SOFI_SUB_REG),
        ISO3_CODE=ifelse( FAOST_CODE=="206", "SSD", ISO3_CODE),
        UNSD_SUB_REG_CODE=ifelse(FAOST_CODE=="206",  "5102", UNSD_SUB_REG_CODE),
        UNSD_SUB_REG=ifelse( FAOST_CODE=="206", "Middle Africa", UNSD_SUB_REG)
 )%>%
 distinct()


fstat.population <- FAOSTAT::read_faostat_bulk("./Data/Population/Population_E_All_Data_(Normalized).zip") %>%
                    filter(element=="total_population___both_sexes") %>% filter(year>=1990 & year<=2019) %>% 
                    mutate(value=value*1000) %>% select(area, year, value) %>% 
                      rename(Year=year, Area=area, Population=value) %>% 
                    filter(Area %in% c('South America', 'Central America',
                           'Caribbean',"Northern America","Europe","Oceania","Asia","Africa")) %>% 
                    mutate(Area=case_when(Area %in% c('South America', 'Central America', 'Caribbean')~
                                           'Latin America and Caribbean',
                            Area=='Northern America'~'North America',
                            TRUE~Area)) %>% 
                    group_by(Area, Year) %>% summarise(Population=sum(Population, na.rm = T)) %>% ungroup() 

population.region <- read_excel('./Data/Population/Population_by_Regions.xlsx')
                    
# population.region <- WDI(country = 'all', indicator = c("SP.POP.TOTL" ), extra = T ) %>% 
#                      rename(POPTT="SP.POP.TOTL") %>% 
#                      select(country, iso3c, year, POPTT) %>%
#  
#  "North America","Latin America & Caribbean"
#                      rename(CountryName=country, CountryCode=iso3c, Year=year) %>% 
#                      mutate(CountryCode=case_when(CountryCode=='EUU'~'EUR', 
#                                                   TRUE~CountryCode))



region.lac <- region.fao %>% filter(UNSD_SUB_REG %in% c('Central America', 'South America','Caribbean'))

region.lacfour <- read.csv('./Mapping/Region_LAC_subregion.csv') %>% select(FAOST_CODE, OFFICIAL_FAO_NAME,SUB_REGION) %>% 
                  distinct()

write.csv(region.lac, file = './Mapping/Region_LAC_subregion.csv', row.names = F)


lac.population <- read_excel('./Data/Population/LAC.xlsx')
emission.total <- read.csv('./Data/Emission/Total/Emissions_Totals_E_All_Data_(Normalized).csv')

emission.byregion <- emission.total %>% filter(Source=='FAO TIER 1') %>% 
                     filter(Area %in% c('South America', 'Central America',
                                        'Caribbean',"Northern America","Europe","Oceania","Asia","Africa")) %>% 
                    select('Area', 'Item','Element','Year','Value') %>% 
                    mutate(Area=case_when(Area %in% c('South America', 'Central America', 'Caribbean')~
                                           'Latin America and the Caribbean',
                                          Area=='Northern America'~'North America',
                                          TRUE~Area)) %>% 
                    group_by(Area, Item, Element, Year) %>% summarise(Value=sum(Value, na.rm = T)) %>% ungroup() %>% 
                    filter(Element=='Emissions (CO2eq) (AR5)') %>% filter(Item=='Emissions on agricultural land') %>%
                    select(-Item,-Element) %>% mutate(Value=Value*1000/10e8) %>% 
                    filter(!Year %in% c(2030, 2050)) %>% rename(Total.Emission=Value)

emission.region.pcap <- emission.total %>% filter(Source=='FAO TIER 1') %>% 
                        filter(Area %in% c('South America', 'Central America',
                                           'Caribbean',"Northern America","Europe","Oceania","Asia","Africa")) %>% 
                        select('Area', 'Item','Element','Year','Value') %>% 
                        mutate(Area=case_when(Area %in% c('South America', 'Central America', 'Caribbean')~
                                               'Latin America and the Caribbean',
                                              Area=='Northern America'~'North America',
                                              TRUE~Area)) %>% 
                        group_by(Area, Item, Element, Year) %>% summarise(Value=sum(Value, na.rm = T)) %>% ungroup() %>% 
                        filter(Element=='Emissions (CO2eq) (AR5)') %>% filter(Item=='Emissions on agricultural land') %>%
                        select(-Item,-Element) %>% mutate(Value=Value*1000) %>% 
                        filter(!Year %in% c(2030, 2050)) %>% rename(Total.Emission=Value) %>% 
                        left_join(population.region) %>% mutate(Emission_PerCapita = Total.Emission/Population)

emission.LAC <- emission.total %>% filter(Source=='FAO TIER 1') %>% 
                  filter(Area %in% c('South America', 'Central America', 'Caribbean')) %>% 
                select('Area', 'Item','Element','Year','Value') %>% mutate(Area='LAC') %>% 
                group_by(Area, Item, Element, Year) %>% summarise(Value=sum(Value, na.rm = T)) %>% ungroup() 

emission.LACF <- emission.total %>% filter(Source=='FAO TIER 1') %>% 
                 left_join(region.lacfour %>% select(-OFFICIAL_FAO_NAME), by=c('Area.Code'='FAOST_CODE')) %>% 
                 filter(!is.na(SUB_REGION)) %>% 
                 select('SUB_REGION', 'Item','Element','Year','Value') %>% 
                 group_by(SUB_REGION, Item, Element, Year) %>% summarise(Value=sum(Value, na.rm = T)) %>% ungroup() 


emission.lac.total <- emission.LAC %>% filter(Item=='Emissions on agricultural land') %>% 
                      filter(Element=='Emissions (CO2eq) (AR5)') %>% mutate(Value=Value*1000) %>% select(-Element, -Item) %>%
                      filter(!Year %in% c(2030, 2050)) %>% left_join(lac.population, by=c('Area'='Region', 'Year'='Year')) %>% 
                      mutate(Value=Value/Population) %>% 
                      mutate(Item='Emission per capita') %>% select(Area, Item, Year, Value)

lac.emission.evolve <- emission.LAC %>% filter(Item %in% c('Farm-gate emissions','Land Use change')) %>% 
                       filter(Element=='Emissions (CO2eq) (AR5)') %>% select(-Element) %>% mutate(Value=Value*1000/10e8) %>% 
                       filter(!Year %in% c(2030, 2050))


lacf.emission.evolve <- emission.LACF %>% filter(Item %in% c('Farm-gate emissions','Land Use change')) %>% 
                        filter(Element=='Emissions (CO2eq) (AR5)') %>% select(-Element) %>% mutate(Value=Value*1000/10e8) %>% 
                        filter(!Year %in% c(2030, 2050))


lac.emission.agprod <- emission.LAC %>% filter(Item %in% 
                                                c("Drained organic soils",'Synthetic Fertilizers','Crop Residues', 
                                                  'Manure left on Pasture','Manure applied to Soils',"Manure Management",
                                                  "Enteric Fermentation","Savanna fires",
                                                  "Burning - Crop residues",
                                                  "Rice Cultivation",
                                                  "On-farm energy use" )) %>% 
                       filter(Element=='Emissions (CO2eq) (AR5)') %>% select(-Element) %>% mutate(Value=Value*1000/10e5) %>% 
                      filter(!Year %in% c(2030, 2050)) %>% filter(Year>=1990)

lacf.emission.agprod <- emission.LACF %>% filter(Item %in% 
                                                c("Drained organic soils",'Synthetic Fertilizers','Crop Residues', 
                                                  'Manure left on Pasture','Manure applied to Soils',"Manure Management",
                                                  "Enteric Fermentation","Savanna fires",
                                                  "Burning - Crop residues",
                                                  "Rice Cultivation",
                                                  "On-farm energy use" )) %>% 
                        filter(Element=='Emissions (CO2eq) (AR5)') %>% select(-Element) %>% mutate(Value=Value*1000/10e5) %>% 
                        filter(!Year %in% c(2030, 2050)) %>% filter(Year>=1990)


lac.emission.luc <- emission.LAC %>% filter(Item %in% 
                                                c("Net Forest conversion",
                                                  "Fires in humid tropical forests","Fires in organic soils")) %>%  
                    filter(Element=='Emissions (CO2eq) (AR5)') %>% select(-Element) %>% mutate(Value=Value*1000/10e8) %>% 
                    filter(!Year %in% c(2030, 2050)) %>% filter(Year>=1990)

lac.emission.subgroup <- emission.LAC %>% filter(Item %in% 
                                                c("Manure Management","Burning - Crop residues","Crop Residues",              
                                                  "Drained organic soils (CO2)", "Drained organic soils (N2O)",
                                                      "On-farm energy use","Rice Cultivation" ,"Savanna fires" ,             
                                                  "Synthetic Fertilizers")) %>% 
                        filter(Element=='Emissions (CO2eq) (AR5)') %>% select(-Element) %>% mutate(Value=Value*1000/10e8) %>% 
                        filter(!Year %in% c(2030, 2050)) %>% filter(Year>=1990)


## emission from agriculture in LAC 
ggplot(lac.emission.evolve, aes(x=Year, y=Value, fill=Item))+
 geom_bar(stat = 'identity',position='stack', width = 0.75) +
 xlab('')+ ylab("Emissions (Gt CO2eq)") + 
    scale_x_continuous(breaks = seq(1990,2020,2))+
 scale_y_continuous(breaks = seq(0,3.5,0.5))+
 theme_minimal() + scale_fill_brewer(palette="Paired")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())

## per capita emission in LAC 
ggplot(emission.lac.total, aes(x=Year, y=Value, fill=Item))+
 geom_bar(stat = 'identity',position='stack', width = 0.75) +
 xlab('')+ ylab("Emissions (t CO2eq)") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 scale_y_continuous(breaks = seq(0,8,2))+
 theme_minimal() + 
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())

ggplot(lacf.emission.evolve[lacf.emission.evolve$Item=='Farm-gate emissions',], aes(x=Year, y=Value, fill=SUB_REGION))+
 geom_bar(stat = 'identity',position='stack', width = 0.75) +
 xlab('')+ ylab("Emissions (Gt CO2eq)") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 scale_y_continuous(breaks = seq(0,2.5,0.5))+
 theme_minimal() + scale_fill_brewer(palette="Paired")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())



ggplot(lacf.emission.evolve[lacf.emission.evolve$Item=='Land Use change',], aes(x=Year, y=Value, fill=SUB_REGION))+
 geom_bar(stat = 'identity',position='stack', width = 0.75) +
 xlab('')+ ylab("Emissions (Gt CO2eq)") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 scale_y_continuous(breaks = seq(0,2.5,0.5))+
 theme_minimal() + scale_fill_brewer(palette="Set2")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())




## overlaid graph with per capita emission 
ggplot(lac.emission.evolve, aes(x=Year, y=Value, fill=Item))+
 geom_bar(stat = 'identity',position='stack', width = 0.75) +
 geom_line(data=emission.lac.total, colour='red', aes(x=Year, y=Value))+
 xlab('')+ ylab("Emissions (Gt CO2eq)") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 scale_y_continuous(breaks = seq(0,3.5,0.5),sec.axis = sec_axis(~.*1, name="100t CO2eq per capita", breaks = seq (0, 7, 1)) )+
 theme_minimal() + scale_fill_brewer(palette="Paired")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())


ggplot(emission.region.pcap, aes(x=Year, y=Emission_PerCapita, fill=Area))+
 geom_bar(stat = 'identity',position='stack', width = 0.75) +
 xlab('')+ ylab("Emissions (t CO2eq) per capita") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 theme_minimal() + scale_fill_brewer(palette="Set3")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())

ggplot(emission.byregion, aes(x=Year, y=Total.Emission, fill=Area))+
 geom_bar(stat = 'identity',position='stack', width = 0.75) +
 xlab('')+ ylab("Emissions (Gt CO2eq) in Agriculture") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 theme_minimal() + scale_fill_brewer(palette="Paired")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())




## line graphs for LAC 
ggplot(lac.emission.evolve, aes(x=Year, y=Value, color=Item))+
 geom_line(size=1) +xlab('')+ ylab("Emissions (Gt CO2eq)") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 scale_y_continuous(breaks = seq(0,2.5,0.5))+
 theme_minimal() + scale_fill_brewer(palette="Paired")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())


## line graphs for LAC 

ggplot(lac.emission.agprod, aes(x=Year, y=Value, fill=Item))+
 geom_bar(stat='identity', position = 'stack', width = 0.75)+
 xlab('')+ ylab("Emissions (Mt CO2eq)") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 scale_y_continuous(breaks = seq(0,1200,200))+
 theme_minimal() + scale_fill_brewer(palette="Paired")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())


ggplot(lacf.emission.agprod, aes(x=Year, y=Value, fill=Item))+
 geom_bar(stat='identity', position = 'stack', width = 0.75)+
 facet_wrap(~SUB_REGION, scales = 'free')+
 xlab('')+ ylab("Emissions (Mt CO2eq)") + 
 scale_x_continuous(breaks = seq(1990,2020,5))+
 theme_minimal() + scale_fill_brewer(palette="Pastel1")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())




ggplot(lac.emission.luc, aes(x=Year, y=Value, fill=Item))+
 geom_bar(stat='identity', position = 'stack', width = 0.75)+
 xlab('')+ ylab("Emissions (Gt CO2eq)") + 
 scale_x_continuous(breaks = seq(1990,2020,2))+
 scale_y_continuous(breaks = seq(0,3.5,0.5))+
 theme_minimal() + scale_fill_brewer(palette="Accent")+
 theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.title =element_blank())

                       