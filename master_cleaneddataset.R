#-------Read Data--------
library(tidyverse)
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

<<<<<<< HEAD

#-------Creating Usable DataSets from Cleaned Data--------

#yearly_data: aggregated by country, year
aggregate(suicidedataclean$suicides_no, by=list(country=suicidedataclean$country, year=suicidedataclean$year), FUN=sum) %>%
  rename(c( "suicides_no" = "x")) -> data1
aggregate(suicidedataclean$population, by=list(country=suicidedataclean$country, year=suicidedataclean$year), FUN=sum) %>%
  rename(c( "population" = "x"))->data2
aggregate(suicidedataclean$gdp_per_capita, by=list(country=suicidedataclean$country, year=suicidedataclean$year), FUN='mean') %>%
  rename(c( "gdp_per_capita" = "x"))->data3
yearly_data <- left_join(data1, data2) %>%
  left_join(data3) %>%
  mutate("gdp" = gdp_per_capita*population) %>%
  mutate("suicide_per_100k" = suicides_no/population*100000)

#-------Adding Other Datasets to Suicide Data----------
#Adding Continent Column#
library(gapminder)
countryregion <- gapminder %>% 
  group_by(country, continent) %>% 
  count() %>% 
  select(country,continent) #get all countries
suicidedataclean <- merge(suicidedataclean, countryregion) #assign continent value

#yearly_data: aggregated by country, year, continent
aggregate(suicidedataclean$suicides_no, by=list(country=suicidedataclean$country, year=suicidedataclean$year, continent=suicidedataclean$continent), FUN=sum) %>%
=======

#-------Creating Usable DataSets from Cleaned Data--------

#yearly_data: aggregated by country, year
aggregate(suicidedataclean$suicides_no, by=list(country=suicidedataclean$country, year=suicidedataclean$year), FUN=sum) %>%
>>>>>>> b3ca1ae146b39deecd68385c484925ba5a57f013
  rename(c( "suicides_no" = "x")) -> data1
aggregate(suicidedataclean$population, by=list(country=suicidedataclean$country, year=suicidedataclean$year), FUN=sum) %>%
  rename(c( "population" = "x"))->data2
aggregate(suicidedataclean$gdp_per_capita, by=list(country=suicidedataclean$country, year=suicidedataclean$year), FUN='mean') %>%
  rename(c( "gdp_per_capita" = "x"))->data3
yearly_data_2 <- left_join(data1, data2) %>%
  left_join(data3) %>%
  mutate("gdp" = gdp_per_capita*population) %>%
  mutate("suicide_per_100k" = suicides_no/population*100000)

#-------Adding Other Datasets to Suicide Data----------
#Adding Continent Column#
library(gapminder)
countryregion <- gapminder %>% 
  group_by(country, continent) %>% 
  count() %>% 
  select(country,continent) #get all countries
suicidedataclean <- merge(suicidedataclean, countryregion) #assign continent value

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
country_per_continent <- yearly_data_2 %>% 
  group_by(continent, year) %>%
  count(continent) #low numbers in Oceania & Africa & Asia -> group then together
View(country_per_continent)
region_data <- yearly_data_2 %>% 
  select(year, suicides_no, population, gdp, continent) %>% 
  mutate(continent = fct_lump(continent, n=2)) %>% 
  group_by(continent, year) %>% 
  summarise(sum(suicides_no),sum(population), sum(gdp)) %>% 
  rename(suicides_no=`sum(suicides_no)`, population=`sum(population)`, gdp=`sum(gdp)`) %>% 
  mutate(suicide_rate = (suicides_no/population*100000),
         gdp_per_capita = (gdp/population))

yearly_data %>% 
  ggplot(aes(x = year, y =suicide_per_100k)) +
  stat_bin_hex()+
  scale_fill_gradient(low= "lightblue", high = "darkblue")+
  theme_minimal()+
  labs(y = "Suicides per 100k Population", x = "Year")+
  ggtitle("Suicide Rates over Time")
#One country has extremely high suicide rates in a few years
#That country is lithuania and the years are after the breakup of the soviet union

yearly_data %>% 
  filter(country == "Lithuania") %>% 
  ggplot(aes(x = gdp_per_capita, y = suicide_per_100k)) +
  geom_point()+
  theme_minimal()+
  labs(y = "Suicides per 100k Population", x = "GDP per Capita")+
  ggtitle("Lithuania GDP vs Suicide Rates")
#Plotting Lituania's GDP vs suicide rates

yearly_data %>% 
  ggplot(aes(x = gdp_per_capita, y = suicide_per_100k)) +
  stat_bin_hex()+
  scale_fill_gradient(low= "lightblue", high = "darkblue")+
  theme_minimal()+
  labs(y = "Suicides per 100k Population", x = "GDP per Capita")+
  ggtitle("GDP per Capita vs Suicide Rates")
#plotting the world's suicide rates vs gdp per capita

yearly_data %>% 
  lm(suicide_per_100k ~ gdp_per_capita, data = .)->yearly_model
#seeing if gdp per capita predicts suicide rates
yearly_data %>% 
  add_predictions(yearly_model) %>% add_residuals(yearly_model)->yearly_data

ggplot(yearly_data)+
  geom_point(aes(x = gdp_per_capita, y = suicide_per_100k))+
  geom_line(aes(x = gdp_per_capita, y = pred), color = "orange")+
  theme_minimal()+
  labs(y = "Suicides per 100k Population", x = "GDP per Capita")+
  ggtitle("GDP per Capita Linear Model")
#seeing if gdp per capita predicts suicide rates
#the model goes the opposite direction of our expectation

yearly_data %>% 
  lm(suicide_per_100k ~ gdp_per_capita + I(gdp_per_capita^2), data = .)->yearly_model2
#model with beta squared interaction
yearly_data %>% 
  add_predictions(yearly_model2) %>% add_residuals(yearly_model2)->yearly_data

ggplot(yearly_data)+
  geom_point(aes(x = gdp_per_capita, y = suicide_per_100k))+
  geom_line(aes(x = gdp_per_capita, y = pred), color = "orange")+
  theme_minimal()+
  labs(y = "Suicides per 100k Population", x = "GDP per Capita")+
  ggtitle("GDP per Capita Quadratic Model")
#seeing if adding a quadratic factor improves the validity of the model. It is better but there are some issues
#We need some way to weight the points based on population. the countries with the highest GDP per capita are the largest
#and all the countries with low GDP and low suicides are relatively small

#adding in data from the world bank development indicators 
read_csv("Education Spending Data.csv") -> Education_Spending_Data 
read_csv("Unemployment Data.csv") -> Unemployment_Data

Education_Spending_Data %>% 
  mutate(across('1987':'2015', as.numeric))->Education_Spending_Data

Education_Spending_Data %>%    
  pivot_longer(!'Country Name', names_to = "year", values_to = "Education Spending") ->education

education %>% 
  mutate(across(year, as.integer)) %>% 
  rename(country = `Country Name`) ->education

left_join(yearly_data, education) ->yearly_data

Unemployment_Data %>% 
  mutate(across('1987':'2015', as.numeric))->Unemployment_Data

Unemployment_Data %>%     
  pivot_longer(!'Country Name', names_to = "year", values_to = "Unemployment Rate") ->unemployment

unemployment %>% 
  mutate(across(year, as.integer)) %>% 
  rename(country = `Country Name`) ->unemployment

left_join(yearly_data, unemployment) ->yearly_data

yearly_data %>% 
  ggplot(aes(x = `Unemployment Rate`, y = suicide_per_100k)) +
  stat_bin_hex()+
  scale_fill_gradient(low= "lightblue", high = "darkblue")+
  ylab("Suicides per 100k Population")+
  ggtitle("Unemployment and Suicide Rates")
#plotting the world's suicide rates vs gdp per capita

#model using world bank data
yearly_data %>% 
  lm(suicide_per_100k ~ gdp_per_capita + `Unemployment Rate` + `Education Spending`, data = .)->yearly_model3

yearly_data %>% 
  add_predictions(yearly_model3) %>% add_residuals(yearly_model3)->yearly_data

summary(yearly_model3)
