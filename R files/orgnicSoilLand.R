
library(tidyverse)


# check emission from cropland organic soils between ag production and land use files ---------

reg_excl <- c("World", "Africa","Eastern Africa",          
 "Middle Africa",                           "Southern Africa" ,                        "Western Africa"  ,                       
 "Americas",                                "Northern America"   ,                     "Central America" ,                       
 "Caribbean",                               "South America" ,                          "Asia"       ,                            
 "Eastern Asia" ,                           "Southern Asia"  ,                         "South-Eastern Asia" ,                    
 "Western Asia"  ,                          "Europe" ,                                 "Eastern Europe"    ,                     
 "Northern Europe" ,                        "Southern Europe",                         "Western Europe"  ,                       
 "Oceania"  ,                               "Australia and New Zealand"  ,             "Melanesia"  ,                            
 "European Union"   ,                       "Least Developed Countries" ,              "Land Locked Developing Countries" ,      
 "Small Island Developing States",          "Low Income Food Deficit Countries" ,      "Net Food Importing Developing Countries",
 "Annex I countries",                       "Non-Annex I countries" ,                  "OECD")        



org.soil <- read_csv('Data/Emission_Data/Emissions_Agriculture_Cultivated_Organic_Soils_E_All_Data_(Normalized).csv') %>% 
            filter(Item=='Cropland organic soils') %>% filter(Year %in% c(2016,2017,2018)) %>% 
            select(Area, Element, Year, Unit, Value) %>% 
            filter(Element %in% c('Area', 'Emissions (CO2eq) from N2O (SAR)', 
                                  'Implied emission factor for N2O (Cultivation of organic soils)')) %>% 
            select(-Unit) %>% mutate(Element=case_when(Element=='Area'~'Area_Prod', 
                                                       Element=='Implied emission factor for N2O (Cultivation of organic soils)'~'EF_Prod', 
                                                       Element=='Emissions (CO2eq) from N2O (SAR)'~'CO2eq_Prod')) %>% 
            pivot_wider(id_cols = c('Area','Year'), names_from = 'Element', values_from = Value)


land.cropland <- read_csv('Data/Emission_Data/Emissions_Land_Use_Cropland_E_All_Data_(Normalized).csv') %>% 
                 select(Area, Element, Year, Unit, Value) %>% filter(Year %in% c(2016,2017,2018)) %>% 
                 filter(Element %in% c('Area', 'Net emissions/removal (CO2eq) (Cropland)', 'Implied emission factor for C (Cropland)') )%>% 
                 select(-Unit) %>% mutate(Element=case_when(Element=='Area'~'Area_Lnd', 
                                                                Element=='Implied emission factor for C (Cropland)'~'EF_Lnd', 
                                                                Element=='Net emissions/removal (CO2eq) (Cropland)'~'CO2eq_Lnd')) %>% 
                 pivot_wider(id_cols = c('Area','Year'), names_from = 'Element', values_from = Value)


cropland.comb <- org.soil %>% left_join(land.cropland) %>% filter(!(Area %in% reg_excl)) %>% 
                 mutate(ovlap_co2 = ifelse(CO2eq_Prod==CO2eq_Lnd, 'TRUE', 'FALSE'), unequal_co2prod = ifelse(CO2eq_Prod>CO2eq_Lnd, 'TRUE', 'FALSE'),
                        ovlap_area = ifelse(Area_Prod==Area_Lnd, 'TRUE', 'FALSE'),
                        match_EF = ifelse(EF_Prod==EF_Lnd, 'TRUE', 'FALSE'), 
                        multip_co2 = CO2eq_Lnd/CO2eq_Prod, 
                        lnco2_prod=log(CO2eq_Prod), 
                        lnco2_lnd = log(CO2eq_Lnd))

em_cropland <- cropland.comb %>% filter(Area %in% c('Ireland', 'India', 'China', 
                                                    'United States of America', 'South Africa')) %>% filter(Year==2017) %>% 
               rename(Country=Area) %>% select(-lnco2_prod,-lnco2_lnd, -unequal_co2prod)

write_csv(em_cropland, 'Emission_Cropland_LU_vs_AgProduction.csv' )

write.csv

cropland.comb %>% filter(Year==2017) %>% 
ggplot(aes(EF_Lnd))+
 geom_density()

cropland.comb %>% filter(ovlap_co2=='TRUE')
cropland.comb %>% filter(ovlap_area=='FALSE')
cropland.comb %>% filter(match_EF=='TRUE')
cropland.comb %>% filter(unequal_co2prod=='TRUE')

# density plot of CO2eq
dens1 <- cropland.comb %>% select(CO2eq_Prod, CO2eq_Lnd) %>% apply(2, density)
plot(NA, xlim=range(sapply(dens1, "[", "x")), ylim=range(sapply(dens1, "[", "y")))
mapply(lines, dens1, col=1:length(dens1))
legend("topright", legend=names(dens1), fill=1:length(dens1))


# density plot of CO2eq (in log)
dens2 <- cropland.comb %>% select(lnco2_prod, lnco2_lnd) %>% filter(lnco2_lnd>0) %>% apply(2, density)
plot(NA, xlim=range(sapply(dens2, "[", "x")), ylim=range(sapply(dens2, "[", "y")))
mapply(lines, dens2, col=1:length(dens2))
legend("topright", legend=names(dens2), fill=1:length(dens2))



# check with land-all file --------------------------------------------------------------------
land.all <- read_csv('Data/Emission_Data/Emissions_Land_Use_Land_Use_Total_E_All_Data_(Normalized).csv') %>% 
            filter(Item=='Cropland') %>% 
            select(Area, Element, Year, Unit, Value) %>% filter(Year %in% c(2016,2017,2018)) %>% 
            filter(Element=='Net emissions/removals (CO2eq)') %>% 
            select(-Element, -Unit) %>% rename(co2_croplnd=Value)

crplnd.comb <- land.all %>% left_join(land.cropland)%>% mutate(overlap = ifelse(CO2eq_Lnd==co2_croplnd, 'TRUE', 'FALSE'))

crplnd.comb %>% filter(overlap=='TRUE') %>% nrow()
crplnd.comb %>% filter(overlap=='FALSE') %>% nrow()
crplnd.comb %>% filter(is.na(overlap)) %>% nrow()

