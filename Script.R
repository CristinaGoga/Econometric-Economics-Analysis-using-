install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")



library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

summary(setdedate.csv)
View(setdedate.csv)

# Regresia simpla ----------------------------------------------------

# Exemplu pret
rm(list = ls())
directory <- "C:/Users/criss/Desktop/Proiect econometrie/"


PackageNames <- c("tidyverse", "stargazer", "magrittr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}
Set <- read.csv(paste0(directory, "setdedate.csv.csv"))


Set %<>% select(Company,Product, TypeName, Inches, Cpu , Ram.GB.,Price.Euro., Weight.kg.)
str(Set)
stargazer(Set, type = "text")
head(Set, 10)

#Normality--testarea normalitatii pentru price
hist(Set$Price.Euro.)

# Explorarea datelor
#!!!!!!!!!!cor(Set)
#!!!!!!!!!!Set %<>% mutate(avg_Price.Euro. = mean(Price.Euro.))


#Linearity-- relatia dintre vd si vi trb sa fie lineara.
#Testam cu ajutorul Scatter-Plot

plot(Price.Euro. ~ Weight.kg.,  Set)
plot(Price.Euro. ~ Ram.GB.,  Set)
#plot(Price_euros ~ Memory,  setdedate)

# Regresia simpla: salary = beta0 + beta1*roe + u
model_setdedate <- lm(formula = Price.Euro. ~ Ram.GB., data = Set)
summary(model_Set)
model_Set$coefficients['Ram.GB.']

model_setdedate <- lm(formula = Price.Euro. ~ Inches, data = setdedate.csv)
summary(model_setdedate)
model_setdedate$coefficients['Inches']

model_setdedate <- lm(formula = Price.Euro. ~ Memory.GB., data = setdedate.csv)
summary(model_setdedate)
model_setdedate$coefficients['Memory.GB.']

model_setdedate <- lm(formula = Price.Euro. ~ Weight.kg., data = setdedate.csv)
summary(model_setdedate)
model_setdedate$coefficients['Weight.kg']
   










#Graficul observatiilor cu dreapta estimata

plot(x = setdedate.csv$Ram.GB., y = setdedate.csv$Price.Euro.)
abline(a = model_setdedate.csv$coefficients['(Intercept)'], 
       b = model_setdedate.csv$coefficients['Ram.GB.'],
       col = 'red')

plot(x = setdedate.csv$Inches, y = setdedate.csv$Price.Euro.)
abline(a = model_setdedate.csv$coefficients['(Intercept)'], 
       b = model_setdedate.csv$coefficients['Inches'],
       col = 'red')


plot(x = setdedate.csv$Memory.GB., y = setdedate.csv$Price.Euro.)
abline(a = model_setdedate.csv$coefficients['(Intercept)'], 
       b = model_setdedate.csv$coefficients['Memory.GB.'],
       col = 'red')

plot(x = setdedate.csv$Weight.kg, y = setdedate.csv$Price.Euro.)
abline(a = model_setdedate.csv$coefficients['(Intercept)'], 
       b = model_setdedate.csv$coefficients['Weight.kg'],
       col = 'red')






# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = setdedate.csv, mapping = aes(x = Weight.kg., y = Price.Euro.)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(data = setdedate.csv, mapping = aes(x = Ram.GB., y = Price.Euro.)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


ggplot(data = setdedate.csv, mapping = aes(x = Inches, y = Price.Euro.)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


ggplot(data = setdedate.csv, mapping = aes(x = Memory.GB., y = Price.Euro.)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Model cu coeficienti semnificativi- alegem R-quared:0.5244-- Ram.GB

Ram.GB.Price.Euro.lm <- lm(Price.Euro. ~ Ram.GB., data = setdedate.csv)
summary(Ram.GB.Price.Euro.lm)




Memory.GB.Price.Euro.lm <- lm(Price.Euro. ~ Memory.GB., data = setdedate.csv)
summary(Memory.GB.Price.Euro.lm)


Inches.Price.Euro.lm <- lm(Price.Euro. ~ Inches, data = setdedate.csv)
summary(Inches.Price.Euro.lm)



Weight.kg.Price.Euro.lm <- lm(Price.Euro. ~ Weight.kg., data = setdedate.csv)
summary(Weight.kg.Price.Euro.lm)




setdedate.csv %<>% mutate(pricehat = fitted(model_setdedate.csv))
stargazer(setdedate.csv, type = "text")
ggplot(data = setdedate.csv, mapping = aes(x = Ram.GB.)) +
  geom_point(mapping = aes(y = Price.Euro., color = 'Price - actual value')) +
  geom_point(mapping = aes(y = pricehat, color = 'Pricehat - predicted value')) + 
  xlab('Return on equity')






