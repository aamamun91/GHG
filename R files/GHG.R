
library(tidyverse)
library(FAOSTAT)

ghgdir = "./1_1_Inputs/FAOSTATData/Agriculture/"
intdir = "./1_1_Inputs/FAOSTATData/Intensities/"
totaldir = "./1_1_Inputs/FAOSTATData/Total/"
proddir = "./1_1_Inputs/FAOSTATData/Production/"
ifadir = "./1_1_Inputs/IFAData/"
otherdir = "./1_1_Inputs/FAOSTATData/Others/"
fertdir = "./1_1_Inputs/FAOSTATData/Fertilizer and Pesticides/"

region.fao <- FAOSTAT::FAOregionProfile%>%
              select(FAOST_CODE,ISO3_CODE)%>% filter(!is.na(ISO3_CODE))

# Accessing files ---------------------------------------------------------

ghgfiles = list.files(ghgdir)
otherfiles = list.files(otherdir)
intfiles = list.files(intdir)

names = gsub('.{28}$', '', ghgfiles)
names = str_replace(names, "Emissions_Agriculture_", "")
ghgdata = lapply(1:9, function(i) FAOSTAT::read_faostat_bulk(paste0(ghgdir, ghgfiles[i])))
# lapply(1:14, function(i) assign(names[i],  ghgdata[[i]]))

for(i in 1:length(ghgdata)){
  assign(names[i],  ghgdata[[i]])
}

names2 = gsub('.{28}$', '', otherfiles)
names2 = str_replace(names2, "Emissions_", "")
otherdata = lapply(1:2, function(i) FAOSTAT::read_faostat_bulk(paste0(otherdir, otherfiles[i])))
for(i in 1:length(otherdata)){
  assign(names2[i],  otherdata[[i]])
}


names3 = gsub('.{28}$', '', intfiles)
names3 = str_replace(names3, "Environment_Emissions_", "")
intdata = lapply(1:2, function(i) FAOSTAT::read_faostat_bulk(paste0(intdir, intfiles[i])))
for(i in 1:length(intdata)){
  assign(names3[i],  intdata[[i]])
}

# Mapping -----------------------------------------------------------------
mapdir = "./1_4_Mapping/"
mapfiles = list.files(mapdir)
mapnames = gsub('.{4}$', '', mapfiles)
mapdata = lapply(1:7, function(i) read.csv(paste0(mapdir, mapfiles[i])))

for(i in 1:length(mapdata)){
  assign(mapnames[i],  mapdata[[i]])
}


# Emission intensity ------------------------------------------------------

intensitydata <- intensities %>% dplyr::select(area, item, element,year, value) %>% 
                 spread(element, value) %>% rename(CO2="emissions__co2eq___ar5_", intensity="emissions_intensity")


# Emission - total --------------------------------------------------------
totalemission <- FAOSTAT::read_faostat_bulk(paste0(totaldir, 'Emissions_Totals_E_All_Data_(Normalized).zip')) %>% 
                 filter(source=='FAO TIER 1') %>% 
                 dplyr::select(area, item, element, unit, value)


# agricultural production -------------------------------------------------

production <- FAOSTAT::read_faostat_bulk(paste0(proddir,'Production_Crops_Livestock_E_All_Data_(Normalized).zip')) 



livestock <- production %>% filter(item_code>850 & item_code<1500)


# value of agricultural production ----------------------------------------

vop <- FAOSTAT::read_faostat_bulk(paste0(proddir,'Value_of_Production_E_All_Data_(Normalized).zip'))

live.vop <- vop %>% filter(item_code>850 & item_code<1500) %>% filter(element_code==152) %>% 
            select(area,item,item_code,year,value) %>% mutate(value=value*1000)

item.list <- live.vop %>% select(item, item_code) %>% distinct()

four.animals <- live.vop %>% filter(item_code %in% c(972,1137,1032,1012,982,1130,1020,951,987)) %>% 
                mutate(item_code=as.character(item_code)) %>% 
                mutate(animals = case_when(item_code==1137~'Camel1',
                                               item_code==1130~'Camel2',
                                               item_code==1032~'Goats1',
                                               item_code==1020~'Goats2',
                                               item_code==1012~'Sheep1',
                                               item_code==982~'Sheep2',
                                               item_code==987~'Sheep3',
                                               item_code==972~'Buffalo1',
                                               item_code==951~'Buffalo2',
                                               TRUE~item_code),
                       animalgroup = case_when( (animals %in% c('Camel1', 'Camel2'))~'Camels',
                                                (animals %in% c('Goats1', 'Goats2'))~'Goats',
                                                (animals %in% c('Buffalo1', 'Buffalo2'))~'Buffalo',
                                                (animals %in% c('Sheep1', 'Sheep2', 'Sheep3'))~'Sheep', 
                                                TRUE~animals)) %>% 
               select(area, animalgroup, animals, year, value)
                
four.aggregate <- four.animals %>% group_by(area, animalgroup, year) %>% summarise(value_agg=sum(value, na.rm = T)) %>% ungroup()  

four.ratio <- four.animals %>% left_join(four.aggregate) %>% mutate(ratio=value/value_agg) %>% dplyr::select(-value,-value_agg) %>% 
              mutate(animals= paste0('r', parse_number(animals))) %>% spread(animals, ratio) %>% mutate(r1=coalesce(r1,0),
                                                                                                        r2=coalesce(r2,0),
                                                                                                        r3=coalesce(r3,0)) %>% 
              rename(item=animalgroup)



# append all files ---------------------------------------------------

colnames = names(Burning_crop_residues)

Energy$source_code = 3050
Energy$source = "FAO TIER 1"
Energy$note = NA
Energy$item_code__cpc_= NA
Energy = Energy %>% select(colnames)

Synthetic_Fertilizers$item_code__cpc_= NA
Synthetic_Fertilizers = Synthetic_Fertilizers %>% select(colnames)

Burning_crop_residues$emissionsource='Burning_crop_residues'
Crop_Residues$emissionsource='Crop_Residues'
Enteric_Fermentation$emissionsource='Enteric_Fermentation'
Manure_applied_to_soils$emissionsource='Manure_applied_to_soils'
Manure_left_on_pasture$emissionsource='Manure_left_on_pasture'
Manure_Management$emissionsource='Manure_Management'
Rice_Cultivation$emissionsource='Rice_Cultivation'
Energy$emissionsource='Energy'
Synthetic_Fertilizers$emissionsource='Synthetic_Fertilizers'


cols <- c(1,3,6,8,10,12,13,14,17)

agriemission <- rbind(Burning_crop_residues,Crop_Residues,Enteric_Fermentation,Manure_applied_to_soils,
                Manure_left_on_pasture,Manure_Management,Rice_Cultivation, Energy, Synthetic_Fertilizers) %>% 
                select(cols) %>% filter(!item %in% c('All Crops','All Animals','Total Energy',
                                                    'Total Energy (excl.eletricity & heat)')) %>% 
                filter(source=='FAO TIER 1') %>% select(-source) %>% filter(year<2023)

colnamesorder = c('area_code','area','item','year','activityname','activity','CO2', 'CH4','N2O', 'emissionsource')


# 1 burning crop residues ---------------------------------------------------

BurningCR = agriemission %>% filter(emissionsource=='Burning_crop_residues') %>% dplyr::select(-unit) %>% 
            spread(element, value) %>% rename(activity = "biomass_burned__dry_matter_", N2O="emissions__n2o_",            
                                              CH4="emissions__ch4_") %>% 
            mutate(activityname='OUTPUT', CO2=N2O*265+CH4*28) %>% dplyr::select(colnamesorder) 
            


# 2 crop residues -----------------------------------------------------------
CropRes = agriemission %>% filter(emissionsource=='Crop_Residues') %>% dplyr::select(-unit) %>% 
          spread(element, value) %>% rename(activity = "residues__crop_residues_", N2O="emissions__n2o_",            
                                            N2O_direct="direct_emissions__n2o___crop_residues_", 
                                            N2O_indirect="indirect_emissions__n2o___crop_residues_") %>% 
          mutate(activityname='OUTPUT', CO2=N2O*265, CH4=0) %>% 
            dplyr::select(all_of(colnamesorder))


# 3 organic soils -----------------------------------------------------------------

OrgSoil = Drained_Organic_Soils %>% filter(item!='Drained organic soils') %>% filter(source=='FAO TIER 1') %>% 
          filter(element!='net_stock_change__c_') %>% 
          dplyr::select(area_code, area, item, element, year, value) %>% mutate(element=ifelse(element=='area','activity', element)) %>% 
          spread(element, value) %>% 
          rename(CO2="emissions__co2_", N2O="emissions__n2o_") %>% 
          mutate(activityname='LAND', emissionsource='Cultivation of organic soils', CH4=0) %>% 
          dplyr::select(all_of(colnamesorder))


# 4 enteric fermentation ----------------------------------------------------

four.list <- c('Camels','Goats','Sheep','Buffalo')

EntFerm = agriemission %>% filter(emissionsource=='Enteric_Fermentation') %>% dplyr::select(-unit) %>% 
          spread(element, value) %>% rename(activity = "stocks",CH4="emissions__ch4_") %>% 
          mutate(activityname='ANIMALS',) %>% dplyr::select(area_code, area, item, year, activityname, activity, CH4, emissionsource) %>% 
          filter(!item %in% c("Cattle", "Camels and Llamas","Sheep and Goats","Swine", "Mules and Asses" ))

Ent.four <- EntFerm %>% filter(item %in% four.list) %>% left_join(four.ratio) %>% filter(!is.na(r1)) %>% 
            mutate(activity_1=activity*r1,activity_2=activity*r2,activity_3=activity*r3, 
                   CH4_1=CH4*r1,CH4_2=CH4*r2,CH4_3=CH4*r3) %>% dplyr::select(-activity, -CH4, -r1,-r2,-r3) %>% 
            pivot_longer(cols=!c(area_code, area, item, year, activityname,emissionsource),names_to = c(".value", "set"), 
                         names_sep = "_"
            ) %>% filter(activity!=0) %>% mutate(item=paste0(item, set)) %>% 
          dplyr::select(area_code, area, item, year, activityname, activity, CH4, emissionsource)


Ent.rest <- EntFerm %>% filter(!item %in% four.list)

EntFerm <- rbind(Ent.rest, Ent.four) %>% mutate(N2O=0, CO2=CH4*28) %>% dplyr::select(all_of(colnamesorder))


# 5 manure applied to soils -------------------------------------------------
animals.exclusion <- c("Cattle", "Camels and Llamas","Sheep and Goats","Swine", "Mules and Asses", "Poultry Birds", "Chickens")
manure.cols <- c('area_code', 'area','item','year','head','activityname','activity', 'N2O', 'emissionsource')

ManureSoils = agriemission %>% filter(emissionsource=='Manure_applied_to_soils') %>% dplyr::select(-unit) %>% 
              spread(element, value) %>% rename(head ="stocks",N2O="emissions__n2o_",activity="manure_applied_to_soils__n_content_") %>% 
              mutate(activityname='Manure (N content)') %>% dplyr::select(all_of(manure.cols)) %>% 
              filter(!item %in% animals.exclusion)

soil.four <- ManureSoils %>% filter(item %in% four.list) %>% left_join(four.ratio) %>% filter(!is.na(r1)) %>% 
              mutate(activity_1=activity*r1,activity_2=activity*r2,activity_3=activity*r3,head_1=head*r1,head_2=head*r2,head_3=head*r3,
                     N2O_1=N2O*r1,N2O_2=N2O*r2,N2O_3=N2O*r3) %>% dplyr::select(-activity, -N2O,-head, -r1,-r2,-r3) %>% 
              pivot_longer(cols=!c(area_code, area, item, year, activityname,emissionsource),names_to = c(".value", "set"), 
                           names_sep = "_") %>% filter(activity!=0) %>% 
              mutate(item=paste0(item, set)) %>% dplyr::select(all_of(manure.cols))

soil.rest <- ManureSoils %>% filter(!item %in% four.list)

ManureSoils <- rbind(soil.rest, soil.four) %>% mutate(CH4=0, CO2=N2O*265) %>% dplyr::select(all_of(colnamesorder))


# 6 manure left on pasture --------------------------------------------------

ManurePast = agriemission %>% filter(emissionsource=='Manure_left_on_pasture') %>% dplyr::select(-unit) %>% 
              spread(element, value) %>% rename(head = "stocks",N2O="emissions__n2o_", activity="manure_left_on_pasture__n_content_") %>% 
              mutate(activityname='Manure (N content)') %>% dplyr::select(all_of(manure.cols)) %>% 
              filter(!item %in% animals.exclusion)

past.four <- ManurePast %>% filter(item %in% four.list) %>% left_join(four.ratio) %>% filter(!is.na(r1)) %>% 
              mutate(activity_1=activity*r1,activity_2=activity*r2,activity_3=activity*r3,head_1=head*r1,head_2=head*r2,head_3=head*r3, 
                     N2O_1=N2O*r1,N2O_2=N2O*r2,N2O_3=N2O*r3) %>% dplyr::select(-activity, -N2O,-head, -r1,-r2,-r3) %>% 
              pivot_longer(cols=!c(area_code, area, item, year, activityname,emissionsource),names_to = c(".value", "set"),names_sep = "_") %>% 
              filter(activity!=0) %>% mutate(item=paste0(item, set)) %>% dplyr::select(all_of(manure.cols))


past.rest <- ManurePast %>% filter(!item %in% four.list)

ManurePast <- rbind(past.rest, past.four) %>% mutate(CH4=0, CO2=N2O*265) %>% dplyr::select(all_of(colnamesorder))

# 7 manure management -------------------------------------------------------
mgtcols <- c('area_code','area','item','year','head','activityname','activity','CH4','N2O','emissionsource')

ManureMgt = agriemission %>% filter(emissionsource=='Manure_Management') %>% dplyr::select(-unit) %>% 
            spread(element,value) %>% rename(head ="stocks",CH4="emissions__ch4_",activity="manure_treated__n_content_",N2O="emissions__n2o_") %>% 
            mutate(activityname='Manure (N content)') %>% dplyr::select(all_of(mgtcols)) %>% 
            filter(!item %in% animals.exclusion)

mgt.four <- ManureMgt %>% filter(item %in% four.list) %>% left_join(four.ratio) %>% filter(!is.na(r1)) %>% 
            mutate(activity_1=activity*r1,activity_2=activity*r2,activity_3=activity*r3,head_1=head*r1,head_2=head*r2,head_3=head*r3, 
                   N2O_1=N2O*r1,N2O_2=N2O*r2,N2O_3=N2O*r3,CH4_1=CH4*r1,CH4_2=CH4*r2,CH4_3=CH4*r3) %>% 
            dplyr::select(-activity,-head, -CH4,-N2O, -r1,-r2,-r3) %>% 
            pivot_longer(cols=!c(area_code, area, item, year, activityname,emissionsource),names_to = c(".value", "set"), 
                         names_sep = "_") %>% filter(activity!=0) %>% 
            mutate(item=paste0(item, set)) %>% dplyr::select(all_of(mgtcols))

mgt.rest <- ManureMgt %>% filter(!item %in% four.list)

ManureMgt <- rbind(mgt.rest, mgt.four) %>% mutate(CO2=N2O*265+CH4*28) %>% dplyr::select(all_of(colnamesorder))

# 8 rice cultivation --------------------------------------------------------

RiceCult = agriemission %>% filter(emissionsource=='Rice_Cultivation') %>% dplyr::select(-unit) %>% 
            spread(element, value) %>% rename(activity="area_harvested", CH4= "emissions__ch4_") %>% 
            mutate(activityname='LAND', N2O=0, CO2=CH4*28) %>% 
            dplyr::select(area_code, area, item, year, activityname, activity, CO2, CH4, N2O, emissionsource)


# 9 energy ------------------------------------------------------------------
energy_cols <- c('area_code','area', 'item', 'year', 'activityname', 'activity', 'CO2', 'CH4','N2O', 'emissionsource')

Energy = agriemission %>% filter(emissionsource=='Energy') %>% dplyr::select(-unit) %>% 
         filter(!item %in% c('Electricity','Energy used in fishery',"Fuel oil used in fisheries",
                             "Gas-diesel oils used in fisheries", "Fuel oil","Gas-Diesel oil" )) %>% 
          spread(element, value) %>% rename(activity="use_in_agriculture",CO2="emissions__co2_",   
                                            CH4="emissions__ch4_", N2O="emissions__n2o_") %>% 
         mutate(activityname="ENERGY") %>% 
         dplyr::select(all_of(energy_cols))

fish.fuel1 <- agriemission %>% filter(emissionsource=='Energy') %>% dplyr::select(-unit) %>% 
              filter(item=="Fuel oil") %>% 
              spread(element, value) %>% rename(activity="use_in_agriculture",CO2="emissions__co2_",   
                                                CH4="emissions__ch4_", N2O="emissions__n2o_") %>% 
              mutate(activityname="ENERGY") %>% 
              dplyr::select(all_of(energy_cols))

fish.fuel2 <- agriemission %>% filter(emissionsource=='Energy') %>% dplyr::select(-unit) %>% 
              filter(item=="Fuel oil used in fisheries") %>% 
              spread(element, value) %>% rename(activity2="use_in_agriculture",CO22="emissions__co2_",   
                                                CH42="emissions__ch4_", N2O2="emissions__n2o_") %>% 
              mutate(activityname="ENERGY", item="Fuel oil") 

energy.fuel <- fish.fuel1 %>% left_join(fish.fuel2) %>% 
               mutate(CH4= coalesce(CH4,0)-coalesce(CH42,0), 
                      CO2= coalesce(CO2,0)-coalesce(CO22,0),
                      N2O= coalesce(N2O,0)-coalesce(N2O2,0),
                      activity= coalesce(activity,0)-coalesce(activity2,0),
                      ) %>% dplyr::select(all_of(energy_cols))


fish.diesel1 <- agriemission %>% filter(emissionsource=='Energy') %>% dplyr::select(-unit) %>% 
                filter(item=="Gas-Diesel oil")%>% 
                spread(element, value) %>% rename(activity="use_in_agriculture",CO2="emissions__co2_",   
                                                  CH4="emissions__ch4_", N2O="emissions__n2o_") %>% 
                mutate(activityname="ENERGY") %>% 
                dplyr::select(all_of(energy_cols))

fish.diesel2 <- agriemission %>% filter(emissionsource=='Energy') %>% dplyr::select(-unit) %>% 
                filter(item=="Gas-diesel oils used in fisheries")%>% 
                spread(element, value) %>% rename(activity2="use_in_agriculture",CO22="emissions__co2_",   
                                                  CH42="emissions__ch4_", N2O2="emissions__n2o_") %>% 
                mutate(activityname="ENERGY", item="Gas-Diesel oil")

energy.diesel <- fish.diesel1 %>% left_join(fish.diesel2) %>% 
                  mutate(CH4= coalesce(CH4,0)-coalesce(CH42,0), 
                         CO2= coalesce(CO2,0)-coalesce(CO22,0),
                         N2O= coalesce(N2O,0)-coalesce(N2O2,0),
                         activity= coalesce(activity,0)-coalesce(activity2,0),
                  ) %>% dplyr::select(all_of(energy_cols))

Energy <- rbind(Energy, energy.fuel, energy.diesel)


# 10 synthetic fertilizer --------------------------------------------------------------

SynFert <- agriemission %>% filter(emissionsource=='Synthetic_Fertilizers') %>% dplyr::select(-unit) %>% 
          spread(element, value) %>% 
          rename(activity="agricultural_use_in_nutrients",N2O="emissions__n2o_") %>% 
          mutate(activityname="agricultural use in nutrients") %>% 
          dplyr::select(area, item, year, activityname, activity, N2O, emissionsource)

fert_allocated <- read.csv(paste0(ifadir, 'fert_allocated.csv')) %>% mutate(year=2019) %>% rename(COUNTRY=ISO3, YEAR=year) %>% 
                  dplyr::select(COUNTRY, SECTOR, YEAR, N_Tons) %>% 
                  group_by(COUNTRY, YEAR) %>% 
                  mutate(PercentN=round(N_Tons/sum(N_Tons),4)) %>% mutate(SECTOR=gsub(" ","", SECTOR),
                                                                          COUNTRY=gsub(" ", "", COUNTRY))

fertilizer <- fert_allocated %>% mutate(N2O = ((N_Tons*1000*0.0132)*44/28)/10e5, CO2 = N2O*265, CH4=0,
                                        EMISSIONSOURCE='SyntFert', INDEX='FERTILIZER') %>% 
              rename(INDEXVALUE=N_Tons) %>% 
              dplyr::select(COUNTRY,SECTOR,YEAR,INDEX,INDEXVALUE,CO2,CH4,N2O,EMISSIONSOURCE)


# 11 Pesticides --------------------------------------------------------------

Pesti <- FAOSTAT::read_faostat_bulk(paste0(fertdir,'Inputs_Pesticides_Use_E_All_Data_(Normalized).zip')) %>% 
         filter(item=="Pesticides (total)") %>% 
         rename(INDEXVALUE=value, YEAR=year) %>% filter(YEAR==2019) %>% 
         left_join(region.fao, by=c('area_code'='FAOST_CODE')) %>% filter(!is.na(ISO3_CODE)) %>% rename(COUNTRY=ISO3_CODE) %>% 
         dplyr::select(COUNTRY, YEAR, INDEXVALUE) 

pesticides <- fert_allocated %>% dplyr::select(COUNTRY, SECTOR, YEAR, PercentN) %>% mutate(YEAR=as.integer(YEAR)) %>% 
              left_join(Pesti) %>% filter(!is.na(INDEXVALUE)) %>% 
              mutate(INDEXVALUE=INDEXVALUE*PercentN, CO2=INDEXVALUE*20*1000/10e5,CH4=0, N2O=CO2/265,
                     INDEX='CHEMICALS', EMISSIONSOURCE='Pesti') %>% 
              dplyr::select(COUNTRY, SECTOR, YEAR, INDEX, INDEXVALUE, CO2,CH4, N2O, EMISSIONSOURCE)


# Animal stock ------------------------------------------------------------

manure.app.stocks <- Manure_applied_to_soils %>% filter(element=='stocks') %>% 
                     dplyr::select(area_code,area,item,year,value) %>% mutate(emissionsource='ManureSoils')

manure.left.stocks <- Manure_left_on_pasture %>% filter(element=='stocks') %>% 
                      dplyr::select(area_code,area,item,year,value) %>% mutate(emissionsource='ManurePast')

manure.mgt.stocks <- Manure_Management %>% filter(element=='stocks') %>% 
                      dplyr::select(area_code,area,item,year,value) %>% mutate(emissionsource='ManureMgt')

manure.stocks <- rbind(manure.app.stocks, manure.left.stocks, manure.mgt.stocks) %>% filter(!item %in% animals.exclusion)

stocks.four <- manure.stocks %>% filter(item %in% four.list) %>% left_join(four.ratio) %>% filter(!is.na(r1)) %>% 
                mutate(value_1=value*r1,value_2=value*r2,value_3=value*r3) %>% dplyr::select(-value, -r1,-r2,-r3) %>% 
                pivot_longer(cols=!c(area_code, area, item, year, emissionsource),names_to = c(".value", "set"), 
                             names_sep = "_"
                ) %>% filter(value!=0) %>% mutate(item=paste0(item, set)) %>% 
                dplyr::select(area_code, area, item, year, value, emissionsource)

stocks.rest <- manure.stocks %>% filter(!item %in% four.list)

stock.all <- rbind(stocks.four, stocks.rest) %>% left_join(region.fao, by=c('area_code'='FAOST_CODE')) %>% 
              dplyr::select(-area_code) %>% filter(!is.na(ISO3_CODE)) %>% 
              rename(COUNTRY=ISO3_CODE) %>% left_join(gtap_map, by=c('item'='Commodity')) %>% 
              rename(SECTOR=GTAP,YEAR=year,INDEXVALUE=value,EMISSIONSOURCE=emissionsource) %>% mutate(INDEX='ANIMALS') %>% 
              dplyr::select(COUNTRY,EMISSIONSOURCE,SECTOR,YEAR,INDEX,INDEXVALUE) %>% 
              filter(YEAR %in% c(2018,2019,2020))
        

# append emission source --------------------------------------------------

ghg.nine <- rbind(BurningCR,CropRes,OrgSoil,RiceCult,EntFerm,ManureMgt,ManurePast,ManureSoils,Energy) %>% 
            left_join(region.fao, by=c('area_code'='FAOST_CODE')) %>% dplyr::select(-area_code) %>% filter(!is.na(ISO3_CODE)) %>% 
            rename(COUNTRY=ISO3_CODE) %>% left_join(gtap_map, by=c('item'='Commodity')) %>% 
            select(COUNTRY,GTAP,year,activityname,activity,CO2,CH4,N2O,emissionsource) %>% 
            rename(SECTOR=GTAP,YEAR=year,INDEX=activityname,INDEXVALUE=activity) %>% 
            left_join(emission_source_mapping, by=c('emissionsource'='Emission_Source')) %>% dplyr::select(-emissionsource) %>% 
            rename(EMISSIONSOURCE=Emi_Map) %>% mutate(EMISSIONSOURCE=gsub(" ", "", EMISSIONSOURCE), 
                                                      SECTOR=gsub(" ", "", SECTOR)) 

emission.complete <- ghg.nine %>% filter(YEAR %in% c(2018,2019,2020)) %>% 
                      rbind(fertilizer) %>% rbind(pesticides)

GHG <- emission.complete %>% dplyr::select(COUNTRY,EMISSIONSOURCE,SECTOR,YEAR,CO2,CH4,N2O) %>% 
       gather(GHG_TYPE,VALUE,-c(COUNTRY,EMISSIONSOURCE,SECTOR,YEAR)) %>% group_by(COUNTRY,EMISSIONSOURCE,SECTOR,YEAR,GHG_TYPE) %>% 
       summarise(VALUE=sum(VALUE)) %>% ungroup()

INDEXVAL <- emission.complete %>% dplyr::select(COUNTRY,EMISSIONSOURCE,SECTOR,YEAR,INDEX, INDEXVALUE) %>% 
            filter(!EMISSIONSOURCE %in% c("ManureMgt","ManurePast","ManureSoils")) %>% 
            rbind(stock.all) %>% mutate(EMISSIONSOURCE=gsub(" ", "", EMISSIONSOURCE), 
                                                                       SECTOR=gsub(" ", "", SECTOR)) %>% 
            group_by(COUNTRY,EMISSIONSOURCE,SECTOR,YEAR,INDEX) %>% 
            summarise(INDEXVALUE=sum(INDEXVALUE)) %>% ungroup()

write.csv(GHG, './1_3_Outputs/GHG.csv', row.names = F)
write.csv(INDEXVAL, './1_3_Outputs/INDEXVALUE.csv', row.names = F)

GHG.excludeEnergy <- GHG %>% filter(EMISSIONSOURCE!='Energy')
INDEXVAL.excludeEnergy <- INDEXVAL %>% filter(EMISSIONSOURCE!='Energy')

write.csv(GHG.excludeEnergy, './1_3_Outputs/GHG_Exclude_Energy.csv', row.names = F)
write.csv(INDEXVAL.excludeEnergy, './1_3_Outputs/INDEXVALUE_Exclude_Energy.csv', row.names = F)

GHG %>% filter(YEAR==2019) %>% filter(GHG_TYPE=='CO2') %>% summarise(VALUE=sum(VALUE,na.rm = T))

GHG %>% filter(!EMISSIONSOURCE %in% c('Pesti','Energy')) %>% 
  filter(YEAR==2019) %>% filter(GHG_TYPE=='CO2') %>% summarise(VALUE=sum(VALUE,na.rm = T))

GHG %>% filter(EMISSIONSOURCE!='OrgSoil') %>% 
  filter(YEAR==2019) %>% filter(GHG_TYPE=='CO2') %>% summarise(VALUE=sum(VALUE,na.rm = T))



GHG %>% filter(YEAR==2019) %>% filter(GHG_TYPE=='CO2') %>% 
  group_by(EMISSIONSOURCE) %>% summarise(CO2=sum(VALUE,na.rm = T))


