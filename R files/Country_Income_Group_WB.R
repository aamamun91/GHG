
library(xlsx)
library(tidyverse)


wb_class <- read.xlsx("CLASS.xlsx", sheetName = 'List of economies', startRow = 5, endRow = 224, colIndex = 3:7) %>% select(-'X') %>% 
            filter(Economy!='x')

country_class <- wb_class %>% select(Economy, Code, Income.group) %>% 
                   rename(Country_Label=Economy, Country_Code=Code, Income_Group=Income.group) %>% 
                 mutate(Income_mutate=case_when(Income_Group=='Low income'~'Low-income', 
                                                Income_Group=='Upper middle income'~'Upper-middle-income', 
                                                Income_Group=='Lower middle income'~'Lower-middle-income',
                                                Income_Group=='High income'~'High-income'
                                                ), Country_Income = paste0(Country_Code, '   .   ', Income_mutate)) 

current_class <- read.xlsx('current_class_compare.xlsx', sheetName = 'Sheet1') 

class_compare <- country_class %>% select(Country_Label, Country_Code, Income_mutate) %>% left_join(current_class) 

case_match <- class_compare %>% filter(Income_mutate==Income_Class_Used)

case_nomatch <- class_compare %>% filter(Income_mutate!=Income_Class_Used)

write.csv(case_nomatch, 'case_no_match.csv', row.names = FALSE)

write.xlsx(country_class, 'Country_Class.xlsx', row.names = FALSE)
 