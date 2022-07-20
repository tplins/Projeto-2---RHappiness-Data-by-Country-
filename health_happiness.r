library(readxl)
library(tidyverse)
library(readxl) 
library(janitor)
library(ggrepel)
library(countrycode)
library(wpp2019)


download.file("https://happiness-report.s3.amazonaws.com/2022/DataForTable2.1.xls", destfile = "happiness.xls")

data_happy<-read_excel("happiness.xls", sheet=1)
data_happy<-janitor::clean_names(data_happy)
data_happy<-data_happy %>% rename(life_expec = healthy_life_expectancy_at_birth)
data_happy<-data_happy %>% rename(country = country_name)
data_happy$country[data_happy$country == "Taiwan_Province_of_China"] <- "Taiwan"
glimpse(data_happy)

data_continent<-countrycode::codelist  %>%
  select(country.name.en, continent, country_code = iso3n) %>%
  filter(!is.na(continent))

data_continent<-data_continent %>% rename(country = country.name.en)
happy_data<-inner_join(data_happy, data_continent, by="country")

data(pop)
data_populacao<-pop %>%
  select(country_code, '2020') %>%
  rename(populacao_2020 = 2)

happy_data<-happy_data %>%
  left_join(data_continent, by="country_code")

dh_2022<-happy_data %>%
  filter(year==max(year))

ggplot(dh_2022)+
  (aes(x=log_gdp_per_capita,y=life_expec))+
  geom_point()+
  geom_label_repel(aes(label=country.x))

ggplot(dh_2022)+
  aes(x=log_gdp_per_capita,y=life_expec)+
  geom_point()+
  geom_label_repel(aes(label=country.x))
  






