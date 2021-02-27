rm(list=ls())
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
Countries <-read.csv("OneTab Worldometer Covid_ 19 25Jan2021.csv", stringsAsFactors = FALSE)

Countries %>% ggplot()+
  geom_point(aes(x = HDI, y = Cases.Per..1M, col=Continent, size=))+
  xlab("HDI")+
  ylab("Cases Per 1 Million")+
  ggtitle("HDI vs Cases per 1 million")


Selected_Countries<-filter(Countries, Democracy.Index>65, Population>5000000)

Selected_Countries %>% ggplot()+
  geom_point(aes(x=Population.Density, y=Deaths.M, col=Continent))+
  xlab("Population Density")+
  ylab("Deaths per Million")+
  ggtitle("Population Density vs Deaths per 1 Million")

Selected_Countries %>% ggplot()+
  geom_point(aes(x=Length.of.Lockdown, y=Deaths.M, col=Continent))+
  xlab("Length of Lockdown")+
  ylab("Deaths per Million")+
  ggtitle("Length of Lockdowns vs Deaths per 1 Million")


Selected_Countries %>% ggplot()+
  geom_point(aes(x=Tests.1.million, y=Deaths.M, col=Continent))+
  xlab("Tests by 1 Million")+
  ylab("Deaths per Million")+
  ggtitle("Tests by 1 million vs Deaths per 1 Million")

Selected_Countries %>% ggplot()+
  geom_point(aes(x=HDI, y=Deaths.M, col=Continent, size=10))+
  xlab("HDI")+
  ylab("Deaths per Million")+
  theme_classic()+
  ggtitle("HDI vs Deaths per 1 Million")

Selected_Countries %>% ggplot()+
  geom_point(aes(x=GDP.Per.Capita, y=Deaths.M, col=Continent))+
  xlab("GDP Per Capita")+
  ylab("Deaths per Million")+
  ggtitle("GDP per Capita vs Deaths per 1 Million")

Selected_Countries %>% ggplot()+
  geom_point(aes(x=Population.Density, y=Cases.Per..1M, col=Continent))+
  xlab("Population Density")+
  ylab("Cases per Million")+
  ggtitle("Population Density vs Cases per 1 Million")

Selected_Countries %>% ggplot()+
  geom_point(aes(x=Length.of.Lockdown, y=Cases.Per..1M, col=Continent, size=10))+
  xlab("Length of Lockdown")+
  ylab("Cases per Million")+
  theme_classic()+
  ggtitle("Length of Lockdowns vs Cases per 1 Million")


Selected_Countries %>% ggplot()+
  geom_point(aes(x=Tests.1.million, y=Cases.Per..1M, col=Continent))+
  xlab("Tests by 1 Million")+
  ylab("Cases per Million")+
  ggtitle("Tests by 1 million vs Cases per 1 Million")

Selected_Countries %>% ggplot()+
  geom_point(aes(x=HDI, y=Cases.Per..1M, col=Continent))+
  xlab("HDI")+
  ylab("Cases per Million")+
  ggtitle("HDI vs Cases per 1 Million")

Selected_Countries %>% ggplot()+
  geom_point(aes(x=GDP.Per.Capita, y=Cases.Per..1M, col=Continent))+
  xlab("GDP Per Capita")+
  ylab("Cases per Million")+
  ggtitle("GDP per Capita vs Cases per 1 Million")



Selected_Countries %>% ggplot()+
  geom_point(aes(x=Mask.Mandate.implementation, y=Cases.Per..1M, col=Continent))+
  xlab("mask mandate implementation")+
  ylab("Cases per Million")+
  ggtitle("mask mandates vs Cases per 1 Million")

Selected_Countries %>% ggplot()+
  geom_point(aes(x=Mask.Mandate.implementation, y=Deaths.M, col=Continent))+
  xlab("mask mandate implementation")+
  ylab("Deaths per Million")+
  ggtitle("mask mandates vs Deaths per 1 Million")


Model_1 <- lm(formula=Cases.Per..1M~Length.of.Lockdown+
                Population.Density+
                GDP.Per.Capita+
                HDI+
                Tests.1.million+
                Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_1) #All of the explanatory variables


Model_2 <- lm(formula=Cases.Per..1M~Length.of.Lockdown+Population.Density+HDI+Democracy.Index+Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_2) #No tests per million or Gdp per capita

Model_3<- lm(formula=Cases.Per..1M~Length.of.Lockdown+
               Population.Density+
               Tests.1.million+
               HDI+
               Democracy.Index+
               Mask.Mandate.implementation, data=Selected_Countries)#No Gdp per Capita
summary(Model_3)

Model_4<- lm(formula=Cases.Per..1M~Length.of.Lockdown, data=Selected_Countries)#only length of Lockdown
summary(Model_4)
Model_5<- lm(formula=Cases.Per..1M~Population.Density, data=Selected_Countries)#only Population Denity
summary(Model_5)
Model_6<- lm(formula=Cases.Per..1M~GDP.Per.Capita, data=Selected_Countries)#only GDP Per Capita
summary(Model_6)
Model_7 <- lm(formula=Cases.Per..1M~Tests.1.million, data=Selected_Countries)#only tests per million
summary(Model_7)

Model_8 <- lm(formula=Cases.Per..1M~Length.of.Lockdown+
                Population.Density+
                #GDP.Per.Capita+
                #HDI+
                Tests.1.million+
                #Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_8)

Model_9 <- lm(formula=Cases.Per..1M~Length.of.Lockdown+
                Population.Density+
                #GDP.Per.Capita+
                HDI+
               # Tests.1.million+
                #Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_9)

Model_10 <- lm(formula=Cases.Per..1M~Length.of.Lockdown+
                Population.Density+
                GDP.Per.Capita+
                #HDI+
                #Tests.1.million+
                #Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_10)


Model_11<- lm(formula=Cases.Per..1M~Length.of.Lockdown+
                             Population.Density+
                 #GDP.Per.Capita+
                 #HDI+
                 #Tests.1.million+
                 Democracy.Index+
                 Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_11)

Model_11_2v<- lm(formula=Cases.Per..1M~#Length.of.Lockdown+
                Population.Density+
                GDP.Per.Capita+
                HDI+
                Tests.1.million+
                Democracy.Index,data=Selected_Countries)
                #Mask.Mandate.implementation data=Selected_Countries)
summary(Model_11_2v)

Model_12 <- lm(formula=Deaths.M~Length.of.Lockdown+
                Population.Density+
                GDP.Per.Capita+
                HDI+
                Tests.1.million+
                Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_12) #All of the explanatory variables


Model_13 <- lm(formula=Deaths.M~Length.of.Lockdown+Population.Density+HDI+Democracy.Index+Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_13) #No tests per million of Gdp per capita

Model_14<- lm(formula=Deaths.M~Length.of.Lockdown+
               Population.Density+
               Tests.1.million+
               HDI+
               Democracy.Index+
               Mask.Mandate.implementation, data=Selected_Countries)#No Gdp per Capita
summary(Model_14)

Model_15<- lm(formula=Deaths.M~Length.of.Lockdown, data=Selected_Countries)#only length of Lockdown
summary(Model_15)
Model_16<- lm(formula=Deaths.M~Population.Density, data=Selected_Countries)#only Population Denity
summary(Model_16)
Model_17<- lm(formula=Deaths.M~GDP.Per.Capita, data=Selected_Countries)#only GDP Per Capita
summary(Model_17)
Model_18 <- lm(formula=Deaths.M~Tests.1.million, data=Selected_Countries)#only tests per million
summary(Model_18)

Model_19 <- lm(formula=Deaths.M~Length.of.Lockdown+
                Population.Density+
                #GDP.Per.Capita+
                #HDI+
                Tests.1.million+
                #Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_19)

Model_20 <- lm(formula=Deaths.M~Length.of.Lockdown+
                #               Population.Density+
                #GDP.Per.Capita+
                #HDI+
                Tests.1.million+
                #Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_20)

Model_21 <- lm(formula=Deaths.M~#Length.of.Lockdown+
                 #               Population.Density+
                 #GDP.Per.Capita+
                 #HDI+
                 Tests.1.million+
                 #Democracy.Index+
                 Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_21)


Model_22<- lm(formula=Deaths.M~#Length.of.Lockdown+
                #               Population.Density+
                #GDP.Per.Capita+
                HDI+
                Tests.1.million+
                #Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries)
summary(Model_22)

Model_23<- lm(formula=Deaths.M~Length.of.Lockdown+
                #               Population.Density+
                #GDP.Per.Capita+
                HDI+
                Tests.1.million+
                #Democracy.Index+
                Mask.Mandate.implementation, data=Selected_Countries) #add length of lockdown
summary(Model_23)

Model_24<- lm(formula=Deaths.M~Length.of.Lockdown+
               #               Population.Density+
               #GDP.Per.Capita+
               #HDI+
               Tests.1.million+
               #Democracy.Index+
               Mask.Mandate.implementation, data=Selected_Countries) #add length of lockdown
summary(Model_24)


library(corrplot)


str(Selected_Countries)

Selected_Countries<-filter(Selected_Countries, Democracy.Index>65, Population>5000000)
drop.cols <- c('ï..countries', 'Continent')
NDF <- Selected_Countries %>% select(-one_of(drop.cols))
cor.table = cor(NDF)
corrplot(cor.table)

Selected_Countries(Model_1)

write.csv(Selected_Countries, "Selected.csv")

Model_11_2v<- lm(formula=Cases.Per..1M~#Length.of.Lockdown+
                   Population.Density+
                   GDP.Per.Capita+
                   HDI+
                   Tests.1.million+
                   Democracy.Index,data=Selected_Countries)
#Mask.Mandate.implementation data=Selected_Countries)
summary(Model_11_2v)

mini_df <- data.frame(country = c('Argentina', 'USA', 'UK', 'India', 'Germany'),
                      model = c(34953.02097, 62577.85345, 42632.81655, 5968.910392, 30576.94529),
                      actual = c(40808, 76462, 52638, 7668, 25320))

ggplot(mini_df) + geom_point(aes(x=country, y=actual), color='blue', size=8) +
  geom_point(aes(x=country, y=model), color='red', shape=15, size=8) + ggtitle('Prediction and Actual Comparison')

names(Selected_Countries)

pred5 <- predict(Model_11_2v, Selected_Countries[c(1,16,20,40,41),])
plot(pred5)

ggplot(Selected_Countries[c(1,16,20,40,41),]) + geom_point(aes(x=ï..countries, y=Cases.Per..1M, color="Actual"), shape=15, size=4) +
  geom_point(aes(x=ï..countries, y=pred5, color="Predicted"),  shape=15, size=4) +
  xlab("Countries")+
  ylab("Cases per 1 Million")+
  ggtitle("Comparison of Model Predictions vs Actual for Selected countries")+
  scale_color_manual(name="Model fit",  breaks = c("Predicted", "Actual"),
                     values = c("Predicted" = "black",
                                "Actual" = "red") )
