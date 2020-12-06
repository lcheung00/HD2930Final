#-------Read Data--------
library(tidyverse)
suicidedata <- read_csv("master.csv") #make sure your working directory is to folder with master.csv
#-------Exploring Outliers/Errors in Suicide Data---------
suicidedata %>% 
  count(year) #remove 2016 with very low numbers (<200) <- outliers (found from raw suicide count)
suicidedata %>% 
  group_by(country) %>% 
  summarise(sum(suicides_no)) %>% 
  filter(`sum(suicides_no)`==0) #Dominica & Saint Kitts and Nevis have 0 over years (remove) <- outliers
str(suicidedata) #also change gdp to integer (from character)

#-------Cleaning Kaggle Suicide Data---------
suicidedataclean <- suicidedata %>% 
  select(-`country-year`) %>% #Remove country.year (redundant)
  filter(year!=2016,
         country!="Dominica",
         country!="Saint Kitts and Nevis") %>% 
  rename(gdp_year=`gdp_for_year ($)`,
         gdp_per_capita=`gdp_per_capita ($)`)

#-------Adding Other Datasets to Suicide Data----------
#Adding Continent Column#
library(gapminder)
countryregion <- gapminder %>% 
  group_by(country, continent) %>% 
  count() %>% 
  select(country,continent) #get all countries
suicidedataclean <- merge(suicidedataclean, countryregion) #assign continent value

#-------Creating Usable DataSets from Cleaned Data--------

#yearly_data: aggregated by country, year
aggregate(suicidedataclean$suicides_no, by=list(country=suicidedataclean$country, year=suicidedataclean$year, continent=suicidedataclean$continent), FUN=sum) %>%
  rename(c( "suicides_no" = "x")) -> data1
aggregate(suicidedataclean$population, by=list(country=suicidedataclean$country, year=suicidedataclean$year, continent=suicidedataclean$continent), FUN=sum) %>%
  rename(c( "population" = "x"))->data2
aggregate(suicidedataclean$gdp_per_capita, by=list(country=suicidedataclean$country, year=suicidedataclean$year, continent=suicidedataclean$continent), FUN='mean') %>%
  rename(c( "gdp_per_capita" = "x"))->data3
yearly_data <- left_join(data1, data2) %>%
  left_join(data3) %>%
  mutate("gdp" = gdp_per_capita*population) %>%
  mutate("suicide_per_100k" = suicides_no/population*100000)

#sex_data: aggregated by year, sex
aggregate(suicidedataclean$suicides_no, by=list(year=suicidedataclean$year, sex=suicidedataclean$sex),FUN=sum) %>%
  rename(c( "suicides_no" = "x")) -> data1
aggregate(suicidedataclean$population, by=list(year=suicidedataclean$year,sex=suicidedataclean$sex), FUN=sum) %>%
  rename(c( "population" = "x"))->data2
aggregate(suicidedataclean$gdp_per_capita, by=list(year=suicidedataclean$year,sex=suicidedataclean$sex), FUN='mean') %>%
  rename(c( "gdp_per_capita" = "x"))->data3
sex_data <- left_join(data1, data2) %>%
  left_join(data3) %>%
  mutate("gdp" = gdp_per_capita*population) %>%
  mutate("suicide_per_100k" = suicides_no/population*100000)

#age_data:aggregated by year, age
aggregate(suicidedataclean$suicides_no, by=list(year=suicidedataclean$year, age=suicidedataclean$age),FUN=sum) %>%
  rename(c( "suicides_no" = "x")) -> data1
aggregate(suicidedataclean$population, by=list(year=suicidedataclean$year,age=suicidedataclean$age), FUN=sum) %>%
  rename(c( "population" = "x"))->data2
aggregate(suicidedataclean$gdp_per_capita, by=list(year=suicidedataclean$year,age=suicidedataclean$age), FUN='mean') %>%
  rename(c( "gdp_per_capita" = "x"))->data3
age_data <- left_join(data1, data2) %>%
  left_join(data3) %>%
  mutate("gdp" = gdp_per_capita*population) %>%
  mutate("suicide_per_100k" = suicides_no/population*100000)

#generation_data: aggregated by year, generation
aggregate(suicidedataclean$suicides_no, by=list(year=suicidedataclean$year, generation=suicidedataclean$generation),FUN=sum) %>%
  rename(c( "suicides_no" = "x")) -> data1
aggregate(suicidedataclean$population, by=list(year=suicidedataclean$year,generation=suicidedataclean$generation), FUN=sum) %>%
  rename(c( "population" = "x"))->data2
aggregate(suicidedataclean$gdp_per_capita, by=list(year=suicidedataclean$year,generation=suicidedataclean$generation), FUN='mean') %>%
  rename(c( "gdp_per_capita" = "x"))->data3
gen_data <- left_join(data1, data2) %>%
  left_join(data3) %>%
  mutate("gdp" = gdp_per_capita*population) %>%
  mutate("suicide_per_100k" = suicides_no/population*100000)

#region_data: aggregated by year, world region
country_per_continent <- yearly_data %>% 
  group_by(continent, year) %>%
  count(continent) #low numbers in Oceania & Africa & Asia -> group then together
View(country_per_continent)
region_data <- yearly_data %>% 
  select(year, suicides_no, population, gdp, continent) %>% 
  mutate(continent = fct_lump(continent, n=2)) %>% 
  group_by(continent, year) %>% 
  summarise(sum(suicides_no),sum(population), sum(gdp)) %>% 
  rename(suicides_no=`sum(suicides_no)`, population=`sum(population)`, gdp=`sum(gdp)`) %>% 
  mutate(suicide_rate = (suicides_no/population*100000),
         gdp_per_capita = (gdp/population))
