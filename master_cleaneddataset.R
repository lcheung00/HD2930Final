#-------Read Data--------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(naniar)
library(modelr)
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
suicidedataclean$gdp_year <- as.integer(suicidedataclean$gdp_year)
suicidedataclean$gdp_per_capita <- as.integer(suicidedataclean$gdp_per_capita)

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

#-------Addition of Gender Datasets from World Bank--------

# column of female unemployment
gender3 <- read_csv("unemployment.csv")
gender3 %>% select(-'Series Code', -'Country Code') -> gender3
gender3 %>% pivot_longer(cols=contains("["), names_to="year", values_to="count") -> gender3
gender3 %>% rename(female_unemployment = "count", country = "Country Name") %>% select(-'Series Name') -> gender3
gender3$year <- substr(gender3$year, start=1, stop=4)
gender3$year <- as.integer(gender3$year)
gender3 %>% select(-country) -> gender3_1
full_join(suicidedataclean, gender3) -> suicidedataclean2
suicidedataclean2$female_unemployment <- substr(suicidedataclean2$female_unemployment, start=1, stop=4)
(suicidedataclean2 %>% replace_with_na(replace=list(female_unemployment = "..")) -> suicidedataclean2)

# column of male unemployment
gender4 <- read_csv("male_unem.csv")
gender4 %>% select(-'Series Code', -'Country Code') -> gender4
gender4 %>% pivot_longer(cols=contains("["), names_to="year", values_to="count") -> gender4
gender4 %>% rename(male_unemployment = "count", country = "Country Name") %>% select(-'Series Name') -> gender4
gender4$year <- substr(gender4$year, start=1, stop=4)
gender4$year <- as.integer(gender4$year)
full_join(suicidedataclean, gender4) -> suicidedataclean3
suicidedataclean3$male_unemployment <- substr(suicidedataclean3$male_unemployment, start=1, stop=4)
(suicidedataclean3 %>% replace_with_na(replace=list(male_unemployment = "..")) -> suicidedataclean3)

# column of male infant mortality rate (deaths per 1000 infant males)
gender5 <- read_csv("deaths.csv")
gender5 %>% select(-'Series Code', -'Country Code') -> gender5
gender5 %>% pivot_longer(cols=contains("["), names_to="year", values_to="count") -> gender5
gender5 %>% rename(infant_deaths_per_1000_male = "count", country = "Country Name") %>% select(-'Series Name') -> gender5
gender5$year <- substr(gender5$year, start=1, stop=4)
gender5$year <- as.integer(gender5$year)
full_join(suicidedataclean, gender5) -> suicidedataclean4
(suicidedataclean4 %>% replace_with_na(replace=list(infant_deaths_per_1000_male = "..")) -> suicidedataclean4)

# male and female unemployment datasets joined
full_join(suicidedataclean2, suicidedataclean3) -> suicidejoin1

# male and female unemployment and infant mortality (male) datasets joined into a final product
full_join(suicidejoin1, suicidedataclean4) -> suicidejoin2


#-------Exploring the Data for Gender Interactions with More Aggregated Datasets and then Graphing some Relationships--------

aggregate(suicidejoin2$`suicides/100k pop`, by=list(sex=suicidejoin2$sex, year=suicidejoin2$year, unemployment=suicidejoin2$female_unemployment, generation=suicidejoin2$generation), FUN=sum) %>%
  rename(c("suicides_rate" = "x")) -> unem_fem # aggregate by sex, year, female unemployment, generation

aggregate(suicidejoin2$`suicides/100k pop`, by=list(sex=suicidejoin2$sex, year=suicidejoin2$year, unemployment=suicidejoin2$male_unemployment, generation=suicidejoin2$generation), FUN=sum) %>%
  rename(c("suicides_rate" = "x")) -> unem_male # aggregate by sex, year, male unemployment, generation

unem_fem %>% ggplot(aes(x=unemployment, y=suicides_rate)) +
  geom_jitter(aes(color=year), alpha=0.5) +
  xlab("Female Unemployment Rate") +
  ylab("Suicide Rate") +
  ggtitle("No Visual Relationship Between Female Unemployment and Suicide Rate") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.title = element_text(face="bold", hjust=0.5)) 
# In a graph of female unemployment vs. suicide rate and colored by year, there is no relationship between female unemployment and suicide rate

unem_male %>% ggplot(aes(x=unemployment, y=suicides_rate)) +
  geom_jitter(aes(color=sex), alpha=0.5) +
  xlab("Male Unemployment Rate") +
  ylab("Suicide Rate") +
  ggtitle("No Strong Relationship Between Male Unemployment and Suicide Rate") +
  theme(plot.title = element_text(face="bold", hjust=0.5)) 
# In a graph of male unemployment vs. suicide rate and colored by sex, there is no relationship between male unemployment and suicide rate

suicidejoin2 %>% filter(!is.na(sex)) %>% 
  ggplot(aes(x=infant_deaths_per_1000_male, y=`suicides/100k pop`)) +
  geom_jitter(aes(color=sex)) +
  xlab("Infant Male Mortality Rate") +
  ylab("Suicide Rate") +
  ggtitle("No Strong Relationship Between Male Infant Mortality and Suicide Rates") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.title = element_text(face="bold", hjust=0.5)) 
# There is also no discernable relationship between infant male mortality rate and suicide rate

# Surprisingly, it seems that unemployment for either sex and suicide do not correlate,
# and neither do male infant mortality and suicide, although this will be modeled later 
# to be sure