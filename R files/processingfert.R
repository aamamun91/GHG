library(tidyverse)
library(readxl)
library(ggplot2)

### Check for coding Etswaini
#data <- read.table

Fertilizer_IFA <- read_xlsx(path="./Data/Production_Data/fertilizers_IFA.xlsx", range="1000T!A2:P2000") %>%
                  filter(Type=="N") %>% select(-Type) %>% 
                  gather(Commodity_Group,N,-Country_Code) %>% na.omit()

Fertilizer_IFA_Total <- Fertilizer_IFA %>% group_by(Country_Code) %>%
                        summarize(N_Total=sum(N)) %>% ungroup

Fertilizer_IFA <- Fertilizer_IFA %>% left_join(Fertilizer_IFA_Total)

ProdStat <-  read_csv("./Data/Production_Data/ProdStat_201517.csv") %>% na.omit %>% select(-Country_Label) %>% filter(Year==2015)
prodstat_2017 <- read_csv("./Data/Production_Data/ProdStat_201517.csv") %>% na.omit %>% select(-Country_Label) %>% filter(Year==2017)

Fertilizer_FAO <- read_csv("Data/Production_Data/Fertilizer_FAO.csv") %>% na.omit %>% select(-Country_Label) %>% filter(Year==2015)
Fertilizer_FAO17 <- read_csv("Data/Production_Data/Fertilizer_FAO.csv") %>% na.omit %>% select(-Country_Label) %>% filter(Year==2017)

# 1] comparer FAO and IFA
IFA_Country<- Fertilizer_IFA %>% select(Country_Code,N_Total) %>% distinct()

# FAO_Country<-Fertilizer_FAO%>%spread(Year,N_Total)%>%
#   left_join(IFA_Country)%>%mutate(FAO=(`2014`+`2015`)/2/1e6)
# head(FAO_Country)
# FAO_Country2<-FAO_Country%>%select(-Country_Code)%>%na.omit()
# cor(FAO_Country2)
## closer from 2015

# 2] fixer le mapping prodstat
mapping_prodstat<- ProdStat %>% select(Item,Commodity_Group) %>% distinct %>%
                    mutate(Com2=case_when(
                      Item =="Millet" ~ "OthCer",
                      Item =="Olives" ~ "OTH OS",
                      Item =="Potatoes" ~ "R&T",
                      Item =="Cassava" ~ "R&T",
                      Item =="Taro (cocoyam)" ~ "R&T",
                      Item =="Yams" ~ "R&T",
                      Item =="Sisal" ~ "Fibre",
                      Item =="Sweet potatoes" ~ "R&T",
                      Item =="Lupins" ~ "Veg",
                      Item =="Currants" ~ "Fruits",
                      Item =="Triticale" ~ "OthCer",
                      Item =="Buckwheat" ~ "OthCer",
                      Item =="Yautia (cocoyam)" ~ "R&T",
                      Item =="Pepper (piper spp.)"~"Residual",
                      Item =="Mustard seed"~"Residual",
                      Item =="Ramie" ~ "Fibre",
                      Item =="Hempseed" ~ "Fibre",
                      Item =="Seed cotton" ~ "Fibre",
                      Item =="Quinoa" ~ "OthCer",
                      Item =="Pyrethrum, dried"~"Residual",
                      Item =="Vanilla"~"Residual",
                      TRUE ~ Commodity_Group
                      ),
                      Commodity_Group=ifelse(Com2=="OTH OS","OthOS",Com2))

mapping_Commodity_Group <- Fertilizer_IFA %>% select(Commodity_Group) %>% distinct%>% arrange()
gtap <- c("wht","pdr","gro","gro","osd","osd","osd","pfb","c_b","v_f", "v_f","v_f","ocr","ocr")
mapping_GTAP_I <- cbind(mapping_Commodity_Group,gtap)
mapping_GTAP_I

mapping_fao_gtap <- mapping_prodstat %>% left_join(mapping_GTAP_I, by = c("Com2" = "Commodity_Group"))


EUR <- c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HRV","HUN","IRL","ITA","LTU","LUX","LVA",
       "MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")

# 3] agreger les quantites et surface par crop aggregat, agreger l'UE
ProdStat.ag <- ProdStat %>% select(-Commodity_Group) %>% left_join(mapping_prodstat) %>%
                mutate(Quantity=Area*Yield,
                       Country_Code=ifelse(Country_Code %in% EUR, "EU28",Country_Code)) %>%
                group_by(Country_Code,Commodity_Group) %>%
                summarize(Q = sum(Quantity) , A=sum(Area) ) %>%
                ungroup() %>% 
                mutate(Y = log(Q/A)) %>%
                left_join(Fertilizer_IFA %>% mutate(Commodity_Group=ifelse(Commodity_Group=='Oth OS', 'OthOS', Commodity_Group))) %>% 
                mutate(NperHa=log(N/A)) %>% na.omit %>% 
                filter(A>1 & N>1 & Q>1)

# 4] faire la regression, par crop, estimer les manquants 
ProdStat.ag.5k<-ProdStat.ag%>%filter(A>5000)
# 5] preparer l'output pour GAMS, coefficient par pays et crops + Total, check pour les contraintes, split set IFA, non IFA
list.commodity <- ProdStat.ag %>% select(Commodity_Group) %>% distinct %>% flatten %>% unlist

check.nat<- prodstat_2017 %>% group_by(Country_Code) %>%
            summarize(SArea=sum(Area)) %>%
            ungroup() %>%
            left_join(IFA_Country) %>%
            rename(N_IFA=N_Total) %>%
            left_join(Fertilizer_FAO17) %>%
            rename(N_FAO=N_Total) %>%
            mutate(NperHaNAT=ifelse(is.na(N_IFA), N_FAO/SArea*1e6,N_IFA*1e6/SArea)) %>% 
            arrange(-NperHaNAT) %>% select(Country_Code, NperHaNAT) %>% na.omit()
  
  
            # mutate(NperHaNAT=ifelse(is.na(N_IFA), N_FAO/SArea*1e6,N_IFA*1e6/SArea))%>%arrange(-NperHaNAT)%>%
            # select(Country_Code,NperHaNAT)

# chec_na <- check.nat %>% filter(is.na(N_FAO))

check.ifa<-  ProdStat.ag %>% mutate(NperHa=N/A*1e6) %>% arrange(-NperHa)

coef<-NULL
cnst<-NULL
for(i in list.commodity)
{
  data.base<- ProdStat.ag %>% filter(Commodity_Group == i) %>% na.omit()
  
  model.a<- lm(NperHa ~ Y, data.base)
  s.a<-  summary(model.a)
  coef[[i]]<- s.a$coefficients[[2]]
  cnst[[i]]<- s.a$coefficients[[1]]
}

res <- data.frame(coef) %>% gather(Commodity_Group, coeff) %>% left_join(data.frame(cnst) %>% gather(Commodity_Group, cnst)) %>% 
       mutate(Commodity_Group = ifelse(Commodity_Group=='R.T', 'R&T', Commodity_Group))

data.base <- ProdStat.ag %>% na.omit()

model.a<-lm(NperHa ~ Y, data.base)

summary(model.a)

avg.yield <- ProdStat.ag %>% group_by(Commodity_Group) %>% summarize(Ymean=mean(Y,na.rm = TRUE), Nmean=mean(NperHa,na.rm=TRUE))

avg.application <- res %>% left_join(avg.yield) %>% mutate(Yield=exp(Ymean)/1e4, NperHaAVG = exp(coeff*Ymean+cnst)*1e6)

# for negative coef we use the regression of other cereals for Fibre: similar average yield
# Other oilseed for soybean
res <- res %>%
        mutate(cnst=case_when(Commodity_Group=="Fibre" ~ -9.120738-10.150369*0.6415952,
                              Commodity_Group=="Soy" ~ -10.857059-9.911035*0.2697191,
                              TRUE ~ cnst),
               coeff=case_when(Commodity_Group=="Fibre" ~ 0.6415952,
                              Commodity_Group=="Soy" ~ 0.2697191,
                              TRUE ~ coeff)
        )



# 6] 

Fertilizer_FAO <- Fertilizer_FAO %>% mutate(N_FAO=N_Total/1e6) %>% select(-N_Total)
Fertilizer_FAO17 <- Fertilizer_FAO17 %>% mutate(N_FAO=N_Total/1e6) %>% select(-N_Total)


ProdStat.base<- prodstat_2017 %>% select(-Commodity_Group) %>% left_join(mapping_prodstat) %>%
                mutate(Quantity=Area*Yield) %>%
                group_by(Country_Code,Commodity_Group) %>%
                summarize(Q=sum(Quantity) ,A=sum(Area) ) %>%
                ungroup() %>% 
                mutate(Y=log(Q/A)) %>%
                left_join(res) %>% 
                filter(Q>1 & A>1) %>% left_join(Fertilizer_IFA) %>%
                mutate(NperHaEst= exp(coeff*Y+cnst)) %>% left_join(Fertilizer_FAO17) %>% left_join(check.nat) %>%
                mutate(compare=NperHaEst*1e6/NperHaNAT) %>%
                mutate(NEst=ifelse(NperHaEst>15*NperHaNAT/1e6,NperHaNAT/1e6*A,
                                    ifelse(NperHaEst<.05*NperHaNAT/1e6,NperHaNAT/1e6*A, A*NperHaEst))) %>% 
                mutate(NEst = ifelse(is.na(N), NEst, ifelse(N==0, 0, NEst)))



total.fertilizer<- ProdStat.base %>% select(Country_Code,N_Total, N_FAO) %>% distinct %>%
                    gather(variable,value,-Country_Code) %>% na.omit %>%
                    mutate(output=paste0(Country_Code,".",variable,"  ",value)) %>% select(output) %>% distinct()

write.table(total.fertilizer,file="total_fertilizer.inc",row.names = FALSE, quote=FALSE,col.names = FALSE)

ProdStat.gtap <- ProdStat.base %>% left_join(mapping_GTAP_I) %>% group_by(Country_Code, gtap) %>%
                  summarize(Nest=sum(NEst,na.rm=TRUE)) %>% ungroup() %>% na.omit %>%
                  mutate(output=paste0(Country_Code,".",gtap,"  ",Nest)) %>% select(output)

write.table(ProdStat.gtap,file="estim_gtap.inc",row.names = FALSE, quote=FALSE,col.names = FALSE)

IFA.gtap <- Fertilizer_IFA %>% left_join(mapping_GTAP_I) %>% group_by(Country_Code, gtap) %>%
            summarize(Nifa=sum(N,na.rm=TRUE)) %>% ungroup() %>% na.omit%>%
            mutate(output=paste0(Country_Code,".",gtap,"  ",Nifa)) %>% select(output)
write.table(IFA.gtap,file="ifa_gtap.inc",row.names = FALSE, quote=FALSE,col.names = FALSE)

list.ifa <- Fertilizer_IFA %>% select(Country_Code)%>%distinct()%>%na.omit
write.table(list.ifa,file="list_ifa.inc",row.names = FALSE, quote=FALSE,col.names = FALSE)

list.fao <- ProdStat.base %>% select(Country_Code)%>%distinct() %>% na.omit
write.table(list.fao,file="list_fao.inc",row.names = FALSE, quote=FALSE,col.names = FALSE)


# data check ----------------------------------------------------------------------------------

N.world <- ProdStat.base%>%left_join(mapping_GTAP_I)%>%na.omit%>%group_by(gtap)%>%
          summarize(Nest=sum(NEst,na.rm=TRUE)*1e3)%>%ungroup()%>%rename(SECTOR=gtap)%>%arrange(Nest)
N.world

ggplot(N.world, aes(fill=SECTOR, y=Nest, x=SECTOR)) +
  geom_bar( stat="identity") 

fert_allocated <- read_csv("fert_allocated.csv")


fiber <- ProdStat.base %>% filter(Commodity_Group=='Fibre')

write_csv(fiber, 'Fibre.csv')

fert_allocated.ag<- fert_allocated %>% group_by(SECTOR)%>%summarize(N=sum(N_Tons))%>%ungroup%>%left_join(N.world)%>%arrange(N)%>%
                    mutate(ShareEst=Nest/sum(Nest,na.rm=TRUE),ShareAll=N/sum(N))
fert_allocated.ag

ggplot(fert_allocated.ag, aes(fill=SECTOR, y=N, x=SECTOR)) +
  geom_bar( stat="identity") 
sum(fert_allocated.ag$Nest,na.rm=TRUE)
sum(fert_allocated.ag$N)
sum(fert_allocated.ag$N)-sum(Fertilizer_IFA$N)*1000


