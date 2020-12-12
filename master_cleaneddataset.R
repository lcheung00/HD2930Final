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


# --------------------------Main Findings of Initial Graphs that Seem to Implicate a Relationship Between Sex and Suicide Rate, as Well as Sex Differences-------------------------
suicidejoin2 %>% filter(!is.na(sex)) %>% ggplot(aes(x=continent, y=`suicides/100k pop`)) +
  geom_jitter(aes(color=sex), alpha=0.5) +
  xlab("Continent") +
  ylab("Suicide Rate") +
  ggtitle("Model of Continent Versus Suicide Rate; Europe is an Outlier") +
  theme_light() +
  theme(plot.title = element_text(face="bold", hjust=0.5)) + 
  theme(axis.text.x=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(axis.text.y=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(legend.title=element_text(face = "bold", 
                                  family="Times",colour="brown",size =14))
# Modeling suicides by continent, it seems that Europe has by far the highest suicide rates 

suicidejoin2 %>% filter(continent=="Europe") -> europesuicide
europesuicide %>% ggplot(aes(x=gdp_per_capita, y=`suicides/100k pop`)) +
  geom_jitter(aes(color=sex), alpha=0.5) +
  xlab("GDP per Capita") +
  ylab("Suicide Rate") +
  ggtitle("Model of GDP Per Capita Versus Suicide Rate for European Countries") +
  theme_light() +
  theme(plot.title = element_text(face="bold", hjust=0.5)) + 
  theme(axis.text.x=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(axis.text.y=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(legend.title=element_text(face = "bold", 
                                  family="Times",colour="brown",size =14)) +
  labs(caption="-there is an observable negative correlation between GDP and suicide-")
# Zooming in on Europe, we see a general negative correlation between gdp per 
# capita and suicide rate, with dramatic sex differences in suicide
# rates but the same general trend for both sexes; however, this might be due more
# to outliers than an actual effect (this will be explored more later)

europesuicide %>% ggplot(aes(x=sex, y=`suicides/100k pop`)) +
  geom_violin(aes(fill=sex)) +
  xlab("Sex") +
  ylab("Suicide Rate") +
  ggtitle("Violin Plot Shows Large Sex Differences in Suicide Rate Distribution") +
  theme(plot.title = element_text(face="bold", hjust=0.5))
# A violin plot of this sex difference shows that female suicide rates are much lower and also have a much shorter tail of outliers

europesuicide %>% ggplot(aes(x=year, y=`suicides/100k pop`)) +
  geom_jitter(aes(color=sex), alpha=0.5) +
  xlab("Year") +
  ylab("Suicide Rate") +
  ggtitle("No Year-Related Reason for the Huge European Suicide Rate") +
  theme(plot.title = element_text(face="bold", hjust=0.5))
# To check one more thing, this graph depicts year against suicide rate in Europe; as shown earlier, there is not correlation, although there are a few
# extra male outliers around the 1990s. A few tragedies such as the death of Princess Diana occurred in 1990s Europe, but there is no actual spike

suicidejoin2 %>% ggplot(aes(x=gdp_per_capita, y=`suicides/100k pop`)) +
  geom_jitter(aes(color=sex), alpha=0.5) +
  xlab("GDP per Capita") +
  ylab("Suicide Rate") +
  ggtitle("Model of GDP Per Capita Versus Suicide Rate for All Countries") +
  theme_light() +
  theme(plot.title = element_text(face="bold", hjust=0.5)) + 
  theme(axis.text.x=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(axis.text.y=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(legend.title=element_text(face = "bold", 
                                  family="Times",colour="brown",size =14)) +
  labs(caption="-the overall observable association between suicide and GDP is seen here-")
# Here is a broader look at the same trend found earlier in Europe; a negative
# correlation between gdp per capita and suicide rate, which is especially 
# prominent for males

sex_data %>% ggplot(aes(x=gdp_per_capita, y=suicide_per_100k)) +
  geom_jitter(aes(color=sex), alpha=0.5) +
  theme(plot.title = element_text(face="bold", hjust=0.5)) +
  xlab("GDP per Captia") +
  ylab("Suicide Rate") +
  ggtitle("The Same Sex Difference But a Clearer View of the Real Relationship Between GDP and Suicide")
# Here is the sex difference again shown from an aggregated dataset, which gives an
# interesting view of the GDP/suicide rate relationship; that is, the negative
# association seen in the larger dataset might be due to overplotting or something 
# else rather than a correlation

region_data %>% ggplot(aes(x=gdp_per_capita, y=suicide_rate)) +
  geom_jitter(aes(color=continent), alpha=0.5) +
  xlab("GDP per Capita") +
  ylab("Suicide Rate") +
  ggtitle("The Americas are an Outlier in the Relationship Between GDP and Suicide") +
  theme(plot.title = element_text(face="bold", hjust=0.5))
# Just for a little extra kick, here is a graph of gdp vs. suicide rate
# aggregated by continent; it seems like the suicide rate in the Americas
# is unique in that it doesn't correlate with gdp, so let's remove it

region_data %>% filter(continent!="Americas") %>% 
  ggplot(aes(x=gdp_per_capita, y=suicide_rate)) +
  geom_jitter(aes(color=continent), alpha=0.5) +
  geom_smooth() +
  xlab("GDP per Capita") +
  ylab("Suicide Rate") +
  ggtitle("Model of GDP Per Capita Versus Suicide Rate for Continents Excluding the Americas") +
  scale_color_brewer(palette="Set2", name="Continent",
                     labels=c("Europe", "Other")) +
  theme_light() +
  theme(plot.title = element_text(face="bold", hjust=0.5)) + 
  theme(axis.text.x=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(axis.text.y=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(legend.title=element_text(face = "bold", 
                                  family="Times",colour="brown",size =14))
# After removing the Americas, Europe and other continents seem to have suicide
# rates that follow an overall visual negative correlation with gdp

suicidejoin2 %>% rename(suicide_rate = "suicides/100k pop") -> suicidejoin2
# variable renamed for efficiency
suicidejoin2 %>% filter(!is.na(sex)) %>% ggplot(aes(x=gdp_per_capita, y=suicide_rate)) +
  geom_point(aes(color=sex)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  facet_grid(~sex) +
  xlab("GDP per Capita") +
  ylab("Suicide Rate") +
  ggtitle("Faceted Model of GDP Versus Suicide Rate With a Quadratic Function") +
  theme_light() +
  labs(caption="-quadratic model is showing the relationship between GDP and suicide-") +
  theme(plot.title = element_text(face="bold", hjust=0.5)) + 
  theme(axis.text.x=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(axis.text.y=element_text(family="Times",face="bold",
                                 colour="brown",size=rel(0.9))) + 
  theme(legend.title=element_text(face = "bold", 
                                  family="Times",colour="brown",size =14))
# Let's debunk the trends previously seen in predicting suicide rate from gdp;
# this graph, faceted by sex and with a quadratic equation fitting the prediction
# of suicide rate from gdp, shows that the correlation might not be what it 
# visually appears to be; this will be explored with simple regression models, 
# but it could also be affected by the deviating correlation between American 
# suicides and gdp or the astoundingly high rates of suicide in Europe


# From these plots, it seems as though Europe has a much higher suicide rate that other
# continents over the years, and that GDP per capita might negatively predict suicide
# rate in both sexes; however, the last couple of graphs seem to give evidence 
# against this assertion. Another large trend in this data is that male suicide
# rate is remarkably and reliably higher across all plots.


# --------------------------Making Models of the Main Findings and Non-Correlated Findings-----------------------------
# The models will be presented through summary statistics because of the difficulty
# of plotting continuous variables and their interactions; the models will be
# made to corroborate or debunk previous graphical findings

suicidejoin2 %>% rename(suicide_rate = "suicides/100k pop") -> suicide_exp 
# The suicide rate variable has been renamed and a new dataset loaded to differentiate model code

# Model 1: predicting suicide rate from sex and gdp per capita
lm(suicide_rate ~ sex +
     gdp_per_capita +
     sex*gdp_per_capita, data=suicide_exp) -> model1
summary(model1)
# A summary of this model finds that male sex, gdp per capita, and the interaction
# between these two variables are all significant predictors of suicide rate,
# which corroborates the previous graphs of a correlation

suicide_exp %>% add_predictions(model1, var="pred1") %>% add_residuals(model1, var="resid1") -> suicide_exp
suicide_exp %>% ggplot(aes(resid1)) +
  geom_freqpoly(binwidth=0.5)
# A plot of the residuals, however, shows that this model is a bit all over the place,
# which is likely due to the large amount of outliers this data contains


# Model 2: predicting suicide rate from sex and gdp per capita in aggregated data
lm(suicide_per_100k ~ sex +
     gdp_per_capita +
     sex*gdp_per_capita, data=sex_data) -> model2
summary(model2)
# When comparing the aggregated and non-aggregated data, male sex and gdp per 
# capita predict suicide rate, but the interaction between them does not

# Model 3: predicting American suicides by sex and gdp and then sex and year
aggregate(suicide_exp$suicide_rate, by=list(year=suicide_exp$year, country=suicide_exp$country, sex=suicide_exp$sex, gdp=suicide_exp$gdp_per_capita),FUN=sum) %>%
  rename(c( "suicide_rate" = "x")) -> sex_country_data
sex_country_data %>% filter(country=="United States") -> sex_USA_data
# new aggregated dataset of American suicides created

lm(suicide_rate ~ sex +
     gdp +
     sex*gdp, data=sex_USA_data) -> model3
summary(model3)
# Male sex and the interaction of gdp and male sex are significant predictors


# Model 4: Exploring sex differences in Europe
aggregate(suicide_exp$suicide_rate, by=list(year=suicide_exp$year, continent=suicide_exp$continent, sex=suicide_exp$sex, gdp=suicide_exp$gdp_per_capita),FUN=sum) %>%
  rename(c( "suicide_rate" = "x")) -> sex_continent_data
sex_continent_data %>% filter(continent=="Europe") -> sex_Euro_data
# new aggregated dataset

lm(suicide_rate ~ sex +
     gdp +
     sex*gdp, data=sex_Euro_data) -> model4
summary(model4)
# Like the United States model, male sex and the interaction between male sex and
# gdp are significant predictors of suicide in Europe


# Model 5: predicting suicide rate from sex and male infant mortality rate
suicide_exp$infant_deaths_per_1000_male <- as.numeric(suicide_exp$infant_deaths_per_1000_male)
# Male infant mortality was imported as a factor variable, so in order to be modeled
# it had to be converted into a numeric
lm(suicide_rate ~ sex +
     infant_deaths_per_1000_male +
     sex*infant_deaths_per_1000_male, data=suicide_exp) -> model5
summary(model5)
# Contrary to the previous graph, this model strongly predicts suicide from male
# infant mortality rate, male sex, and the interaction


# Model 6: predicting suicide rate from sex and male unemployment
suicide_exp$male_unemployment <- as.numeric(suicide_exp$male_unemployment)
# Male unemployment was imported as a factor variable, so in order to be modeled
# it had to be converted into a numeric
lm(suicide_rate ~ sex +
     male_unemployment +
     sex*male_unemployment, data=suicide_exp) -> model6
summary(model6)
# Going back to the absence of a relationship between unemployment and suicide,
# this model found that male unemployment was not a significant predictor of
# suicide, but male sex and the interaction variable were significant

# As an aside, the aggregated data (Model_) shows the same thing
unem_male$unemployment <- as.numeric(unem_male$unemployment)
# Male unemployment was imported as a factor variable, so in order to be modeled
# it had to be converted into a numeric
lm(suicides_rate ~ sex +
     unemployment +
     sex*unemployment, data=unem_male) -> model_
summary(model_)


# -------------------------------In Conclusion...-------------------------------
# From the models of aggregated and original data, it seems that male sex is a 
# major predictor of suicide both individually and in interaction with multiple variables,
# such as male unemployment and gdp; in addition, gdp per capita also seems to be a predictor. 
# Some unexpected findings from these models are that male infant mortality rate 
# and HDI individually predicted suicide; expectedly, variables like year and male 
# unemployment were not significant individual predictors.
