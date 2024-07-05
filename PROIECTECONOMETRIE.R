install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")


library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(lmtest)

summary(setdedatecsv.csv)


# Regresia simpla ----------------------------------------------------
# Exemplu pret

rm(list = ls())
PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest","tidyverse", "stargazer", "magrittr", "car", "strucchange",
                  "ggplot2","caret", "splines","mgcv","glmnet","psych")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

#citire set de date
directory<-("C:/Users/criss/Desktop/Gidoiu_Gogan_Goga/")

DATA <- read.csv(paste0(directory, "setdedatecsv.csv"))
DATA %<>% select(Company,Product,Inches,Ram.GB,Memory.GB,hasSSD,Weight.kg,Price.Euro)
str(DATA)
stargazer(DATA, type = "text")
head(DATA, 10)


#Calcularea coeficientului de corelatie(informativ)
#COEF PERSON=0.7414 -->Acest lucru indica o corelatie pozitiva si mai puternica

cor(DATA$Price.Euro, DATA$Ram.GB, method = c("pearson"))



# Normality--testarea normalitatii pentru var dependenta
hist(DATA$Price.Euro)
summary(DATA$Price.Euro)
sd(DATA$Price.Euro)

#Linearity-- relatia dintre vd si vi trb sa fie lineara.
#Testam cu ajutorul Scatter-Plot

plot(Ram.GB ~ Price.Euro,  DATA)



# Regresie liniara simpla - Price.Euro. (dependenta) si Inches, Memory.GB., Ram.GB, Weight.kg (independente)

model_lsreg <- lm(formula = Price.Euro ~ Memory.GB, data=DATA)
# intercept (constanta) semnificativa la 99%
# variabila independenta Memory.GB este semnificativa la 99%
# R^2 = 0.08332 => Memoria explica 8,3% din variatia pretului, restul alte variabile
# F-statistic: p-value: 3.447e-14 <0.01 => model valid statistic, la 99%
model_lsreg <- lm(formula = Price.Euro ~ Ram.GB, data=DATA)
# intercept (constanta) semnificativa la 99%
# variabila independenta Ram.GB este semnificativa la 99%
# R^2 = 0.5497 => Ram.GB explica 54,97% din variatia pretului, restul alte variabile
# F-statistic: p-value: < 2.2e-16 <0.01 => model valid statistic, la 99%
model_lsreg <- lm(formula = Price.Euro ~ Inches, data=DATA)
# intercept (constanta) semnificativa la 99%
# variabila independenta inches este semnificativa la 99%
# R^2 = 0.0266 => Inches explica 2,6% din variatia pretului, restul alte variabile
# F-statistic: p-value: 2.388e-05 <0.01 => model valid statistic, la 99%
model_lsreg <- lm(formula = Price.Euro ~ Weight.kg, data=DATA)
# intercept (constanta) semnificativa la 99%
# variabila independenta Weight.kg este semnificativa la 95%
# R^2 = 0.008 => Weight.kg explica 0.8% din variatia pretului, restul alte variabile
# F-statistic: p-value: 0.01855 < 0.05 => model valid statistic, la 95%

# Alegem modelul cu cel mai mare R^2 din modelele testate cu coeficienti semnificativi=> 
# => Alegem modelul cu variabila independenta Ram.Gb
model_lsreg <- lm(formula = Price.Euro ~ Ram.GB, data=DATA)
summary(model_lsreg)

# in word pun poza cu rezultatul rularii summary(model_lsreg) pe modelul ales 
# si explic rezultatele obtinute (ce am scris mai sus in comentarii) + interpretare si explicare t value pt var independenta si pt constanta


# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase


ggplot(data = DATA, mapping = aes(x = Ram.GB, y = Price.Euro)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


ggplot(data = DATA, mapping = aes(x = Memory.GB, y = Price.Euro)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Valoarea estimata pentru variabila dependenta (pricehat)
DATA %<>% mutate(pricehat = fitted(model_lsreg))
stargazer(DATA, type = "text")
ggplot(data = DATA, mapping = aes(x = Ram.GB )) +
  geom_point(mapping = aes(y = Price.Euro, color = 'Price - actual value')) +
  geom_point(mapping = aes(y = pricehat, color = 'Pricehat - predicted value')) + 
  xlab('Return on equity')


# Reziduuri
DATA %<>% mutate(uhat = residuals(model_lsreg))
stargazer(DATA, type = "text")
ggplot(DATA, aes(x = Ram.GB)) +
  geom_point(aes(y = Price.Euro, col = 'Price - actual value')) +
  geom_point(aes(y = uhat, col = 'Residual uhat')) +
  xlab('Return on equity')

head(DATA, 10)

# Graficul valorilor si reziduurilor reale si previzionate
ggplot(DATA, aes(x = Ram.GB)) +
  geom_point(aes(y = Price.Euro, color = 'Price - actual value')) +
  geom_point(aes(y = pricehat, color = 'Pricehat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = Price.Euro, color = 'Fitted line'), 
              method = "lm", se = FALSE) +
  xlab('Return on equity')
#DURBIN WATSON TEST--> EXISTA AUTOCORELARE
dwtest(model_lsreg)
summary(DATA$uhat)



# Bonitatea modelului (R-squared) ----------------------------------


# 'r.squared' din summary este R-squared
summary(model_lsreg)$r.squared

#CORECTAREA HETEROSCEDASTICITATII------------------------------------------------------

# Forma functionala log: log-log si log-lin -------------------------------

# Cream o variabila noua pe care o logaritmam cu ajutorul functiei log
 DATA <- DATA %>% mutate(lRAM = log(Ram.GB),
                               lprice = log(Price.Euro))

# Log-log 
model_loglog <- lm(lprice ~ lRAM, DATA)
summary(model_loglog)

model_loglin <- lm(lprice ~ Ram.GB, DATA)
summary(model_loglog)

model_linlog <- lm(Price.Euro ~ lRAM, DATA)
summary(model_loglog)

# Grafic
ggplot(DATA, aes(x = lRAM, y = lprice)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

ggplot(DATA, aes(x = lRAM, y = Price.Euro)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

ggplot(DATA, aes(x = Ram.GB, y = lprice)) +
  geom_point() +
  geom_smooth(method = lm, se = F)


# TESTE DE HETEROSCEDASTICITATE------------------------------------------------


# Generam patratul si interactiunea variabilelor independente
DATA %<>% mutate(Ram.GBsq=Ram.GB^2)

# Testam heteroscedasticitatea pentru variabila price ------------------------

# Obtinem residuals(uhat) si valorile estimate(yhat), si le ridicam la patrat
DATA %<>% mutate(uhatsq = uhat^2,
                 pricehatsq = pricehat^2)

# Testul Breusch-Pagan  ------------------------------------------------------

# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)

# Regresia pentru testul Breusch-Pagan 
summary(model_loglog)

# Metoda predefinita (folosind functii predefinite)
bptest(model_linlog) # pe baza LM 
bptest(model_loglog)
# deoarece p-value = 0.002 < 0.05 => reziduurile sunt hetersocedastice =>
# ipoteza este incalcata



# Testul White  --------------------------------------------------------------

# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)


# Metoda predefinita 
white_test(model_loglog)  

#IPOTEZE DE NORMALITATE IN REZIDUURI-----------------
#Modelul de regresie pentru pretul laptopurilor
model_0 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA)
summary(model_0)
DATA %<>% mutate(uhat = resid(model_0)) # extragem reziduurile din model 


#TEST SHAPIRO -WILK PENTRU DETECTAREA NORMALITATII
#p value<0.1 variabila nu este normal distribuita
shapiro.test(DATA$Price.Euro)
shapiro.test(DATA$lprice)
#TEST JARQUE-BERA pentru normalitate
if(!require(tseries)){install.packages('tseries')}
jarque.bera.test(DATA$Price.Euro)
jarque.bera.test(DATA$lprice)
#p value>0.1 variabila este normal distribuita
#TESTUL T PENTRU SEMNIFICATIA COEFICIENTILOR
    #Model de regresie
model <- lm(Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg,DATA)
summary(model)
(coefficient <- coef(DATA)["exper"])
# Afisarea erorii standard
(se <- vcov(model) %>% # matricea de varianta-covarianta
    diag %>% # extragerea diagonale
    sqrt %>% # radical
    .["exper"]) # obtinerea erirolor stadarde (se) ale regresorului "exper"

(tstat <- coefficient/se)
(df_r <- model$df.residual)

qt(p = 0.975, df = df_r, lower.tail = TRUE)

#p-value nu este mai mic de  0.1/0.05/0.01 deci ipoteza nula nu se respinge deci nu exista diferente semnificative fata de valorile testate


2 * pt(q = abs(tstat), df = df_r, lower.tail = FALSE)
pt(q = tstat, df = df_r, lower.tail = FALSE)
pt(q = tstat, df = df_r, lower.tail = TRUE)
qt(p = 0.975, df = df_r)
coefficient - 1.96 * se

# Valoarea critica pentru nivel de semnificatie de 10%
qt(p = 0.95, df = df_r)
# Valoarea minima a intervalului pentru 90% nivel de incredere
coefficient - 1.65 * se
# Valoarea maxima a intervalului pentru 90% nivel de incredere
coefficient + 1.65 * se
#TESTAREA NORMALITATII REZIDUURILOR


#GRAFICUL RESIDULS VS FITTED
plot(model_0rez)
#In cazul de fata, reziduurile nu par a fi normal distribuite
#de aceea este nevoie de mai multe investigatii ca sa oferim un diagnostic final.
#Ne putem da seama de distributie prin orietarea liniei care nu este aproximativ dreapta.

#GRAFICUL Q-Q PLOT(CUANTILE-CUANTILE)
# Distributia nu pare a fi normala din grafic deoarece exista cateva valori
# extreme cum ar fi punctele 605, 508, 116 Concluzionam ca distributia reziduurilor
# nu este normal distribuita si testam in continuare pentru a oferi un diagnostic final
ols_plot_resid_qq(model_0)
plot(model_0)

#HISTOGRAMA REZIDUURILOR 
ols_plot_resid_hist(model_0)

ggplot(data = DATA) +
  theme_bw() +
  geom_histogram(mapping = aes(x = Ram.GB), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = DATA) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))

# Hisograma arata asimetrie la dreapta deoarece observam valori extreme 
# in jurul valorii 1000. 


skewness(DATA$Ram.GB)
#Valoarea asimetriei skewness este egala cu 1.04221 - ne indica o valaore pozitiva deci vorbim despre
#o distributie inclinata la dreapta
kurtosis(DATA$uhat)
#Valoarea boltirii (adica kurtosis) idica o valoare mai mare decat 3, deci distributia este platicurtica
# In urma testarii coeficintului de asimetrie si a boltirii putem trage concluzia
# ca distributia reziduurilor este asimetrica la dreapta si turtita => 
# reziduurile nu sunt normal distribuite

#GRAFICE DE TIP BOXPLOT

boxplot(model_0$residuals, main="Box Plot reziduuri")


ols_plot_resid_box(model_0)


ggplot(DATA, aes(x=Ram.GB, y=Price.Euro)) + 
  geom_boxplot() +
  theme_bw()+
  xlab('Reziduuri') + 
  ylab('Pret') +
  ggtitle('Boxplot reziduuri') + 
  theme(plot.title = element_text(hjust = 0.5))

# Toate cele 3 modalitati de reprezentare a graficului ne indica faptul ca 
# avem valori extreme in reziduuri si astfel distributia noastra nu este normal 
# distribuita


#Testarea normalitatii cu ajutorul testelor specifice acestei ipoteze.
# Cele mai frecvent utilizate teste de detectare a normalitatii in 
# reziduuri sunt testele Shapiro Wilk si Jarque Bera.



# Testul Shapiro Wilk pentru normalitate
# H0: distributie normala, Ha: distributie nenormala
shapiro.test(DATA$Ram.GB) 
# deoarece p-value < 0.05 => reziduurile nu sunt normal distribuite



# Testul Jarque-Bera pentru normalitate
# H0: distributie normala, Ha: distributie nenormala
jarque.bera.test(DATA$Ram.GB)
# deoarece p-value < 0.05 => reziduurile nu sunt normal distribuite
if(!require(tseries)){install.packages('tseries')}

ols_test_normality(model_0) 
#Toate teste prezinta un p-value < 0.01 => la 99% distrbutia nu este normal distribuita
# Din cele 4 teste => distributia nu este normal distribuita
# Jarque-Bera susinte ca distrbutia nu este normal distribuita la 99% =>
# conform celor 5 test vom respinde ipoteza conform careia ipotezele sunt 
# normal distribuite
#Deoarece am concluzionat ca reziduurile nu sunt normal distribuite, vom incerca
# sa corectam modelul pentru a nu se incalca aceasta ipoteza.

# Distanta Cook este folosita pentru a identifica punctele de date influente. 
# Ulterior, vom elimina aceste puncte (cat timp modelul nu este unul macroeconomic)
# si vom rerula modelul si retesta ipoteza de normalitate 
ols_plot_cooksd_bar(model_0) 
if(!require(olsrr)){install.packages('olsrr')}
ols_plot_cooksd_chart(model_0)
# ambele modalitati de calculare si afisare
# a distantei Cook ne indica faptul ca a 116-a observatie din setul nostru de date
# este un punct influent care ne afecteaza modelul. Il vom elimina si retesta

# Eliminam al 116 lea rand 
DATA_cook <- DATA[-c(116), ]

#Reestimam modelul
model_1 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA_cook)
summary(model_1)
DATA_cook %<>% mutate(Ram_cook = resid(model_1))

# Retestam distanta Cook
ols_plot_cooksd_bar(model_1) 
# in continuare identificam valorile outlier care
# ne strica modelul si le vom elimina si pe acestea
# Cu toate acestea, testam rapid doar cu Jarque-Bera sa confirmam ca reziduurile 
# in continoare nu sunt normal distribuite


# Eliminam punctele indentificate anterior cu distanta Cook
DATA_cook <- DATA[-c(607,604,506,68,85,143,148,159,169,268,264,256,269,625,111,149,583,363,606,372,582,583,581,624,505,563,536 ), ]
model_2 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA_cook)
summary(model_2)
DATA_cook %<>% mutate(Ram_cook = resid(model_2))
ols_plot_cooksd_bar(model_2) 



# Testam cu Jarque-Bera
jarque.bera.test(DATA_cook$Ram.GB) 

DATA_cook <- DATA[-c(113,255,584,583,115,604,601,1,144,258,486,560,581,578,596,580,581,113,578,596,258,560,559,
                     68,114,147,262,266,503,598,601,618,114,110,146,157,167,261,264,288,342,353,359,368,427,483,500,577,600,578,576,599,616
                     ,07,604,506,68,85,143,148,159,169,268,264,256,269,625,111,149,583,363,606,372,582,583,581,624,505,563,536), ]
model_3 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA_cook)
summary(model_3)
DATA_cook %<>% mutate(Ram_cook = resid(model_3))
ols_plot_cooksd_bar(model_3)
jarque.bera.test(DATA_cook$Ram.GB)


DATA_cook <- DATA[-c(1,547,545), ]
model_4 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA_cook)
DATA_cook %<>% mutate(Ram_cook = resid(model_4))
summary(model_4)
ols_plot_cooksd_bar(model_4)
jarque.bera.test(DATA_cook$Ram.GB)

# Pas 1 - Graficul 'Residuals vs Fitted'
ols_plot_resid_fit(model_4)
# Pas 2 - Graficul Q-Q plot
ols_plot_resid_qq(model_4)
# Pas 3 - Histograma reziduurilor
ols_plot_resid_hist(model_4)
# Pas 4 - Boxplotul reziduurilor
ols_plot_resid_box(model_4)
# Pas 5 - Testarea cu ajutorul diferitelor test de normalitate
ols_test_normality(model_4) 



# Testarea ipotezei de homoscedasticitate -----------------



# Graficul reziduurilor fata de variabila independenta Ram.GB
ggplot(data = DATA, mapping = aes(x = Ram.GB, y = Price.Euro)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Kg, weight')


# Graficul reziduurilor fata de valorile estimate de model
DATA %<>% mutate(yhat = fitted(model_0))
ggplot(data = DATA, mapping = aes(x = Ram.GB, y = Price.Euro)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')


# Ambele grafice indica faotul ca avem heteroscedasticitate in date deoarece 
# putem identifica vizual 'efect de palnie' cu precadere in partea
# dreapta a graficului 


# O modalitate frecvent utilizata care ne ajuta sa scapam de heteroscedasticitate
# este de a schimba forma functionala a variabilelor (log ne ajuta in cele mai 
# multe cazuri)


# Regresie cu forma functionala log-log
model_5 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA)
summary(model_5)
DATA %<>% mutate(Price.Euro = resid(model_5))

# Graficul reziduurilor fata de variabila independenta lweight
ggplot(DATA) + 
  theme_bw() + 
  geom_point(aes(x = Ram.GB, y = Price.Euro)) +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Log greutate, lWeight.kg')


# Graph of residuals against fitted values
DATA %<>% mutate(Ram.GB = fitted(model_5))
ggplot(data = DATA, mapping = aes(x = Ram.GB, y = Price.Euro)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')





# De aceasta data, folosind forma functionala log-log pare ca reziduurile 
# sunt homoscedastice, dar nu este destul pentru a concluziona. In continuare
# trebuie sa testam ca sa ajungem la un diagnostic final

# Teste de heteroscedasticitate ------------------------------------------------

# Testele de heteroscedasticitate presupun estimarea modelului de regresie, 
# regresand reziduurile ridicate la patrat (uhatsq) impreuna cu interactiunea si 
# patratul variabilelor independente si efectuarea testului F si testului LM 
# pentru testarea simultana a coeficientilor

# Generam patratul si interactiunea variabilelor independente
DATA %<>% mutate(Weight.kgsq = Weight.kg^2,
                 Inchessq = Inches^2,
                 Memory.GBsq = Memory.GB^2,
                 Ram.GBsq=Ram.GB^2,
                 Weight.kgXInches = Weight.kg*Inches,
                 Weight.kgXMemory.GB = Weight.kg*Memory.GB,
                 Weight.kgXRam.GB = Weight.kg*Ram.GB,
                 InchesXRam.GB = Inches*Ram.GB,
                 InchesXMemory.GB = Inches*Memory.GB,
                 Ram.GBXMemory.GB = Ram.GB*Memory.GB)

# Testam heteroscedasticitatea pentru variabila price ------------------------

# Modelul este acelasi cu cel de mai devreme model_0
# model_0 <- lm(price ~ lotsize + sqrft + bdrms, hprice1)
summary(model_0)

# Obtinem residuals(uhat) si valorile estimate(yhat), si le ridicam la patrat
DATA %<>% mutate(uhatsq = uhat^2,
                 yhatsq = yhat^2)


# Testul Breusch-Pagan  ------------------------------------------------------

# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)

# Regresia pentru testul Breusch-Pagan 
model_BP <- lm(uhatsq ~ Inches + Weight.kg + Memory.GB + Ram.GB, DATA)
summary(model_BP)

# Metoda matematizata a testului
# Nr de variabile independente k1 
(k1 <- model_BP$rank - 1)

(r2 <- summary(model_BP)$r.squared) # R-squared
(n <- nobs(model_BP)) # nr obs


( F_stat <- (r2/k1) / ((1-r2)/(n-k1-1)) ) # F-statistic
( F_pval <- pf(F_stat, k1, n-k1-1, lower.tail = FALSE) ) # p-value < 0.05 =>
# rezidurile sunt heteroscedastice


( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k1, lower.tail = FALSE)) # p-value < 0.05 =>
#reziduurile sunt heteroscedastice


# Metoda predefinita (folosind functii predefinite)
bptest(model_0) # pe baza LM 
# deoarece p-value = 0.002 < 0.05 => reziduurile sunt hetersocedastice =>
# ipoteza este incalcata



# Testul White  --------------------------------------------------------------

# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)



# Regresia pentru testul White
model_White <- lm(uhatsq ~ Inches + Weight.kg + Memory.GB + Ram.GB+Inchessq + Weight.kgsq + Memory.GBsq + Ram.GBsq+Weight.kgXInches+
                    Weight.kgXMemory.GB+Weight.kgXRam.GB+InchesXRam.GB+InchesXMemory.GB+Ram.GBXMemory.GB, DATA)
summary(model_White)

# Nr de variabile independente k2
(k2 <- model_White$rank - 1)


# Testul F si Testul LM pentru detectarea heteroscedasticitatii
(r2 <- summary(model_White)$r.squared) # R-squared
(n <- nobs(model_White)) # nr observatii


( F_stat <- (r2/k2) / ((1-r2)/(n-k2-1)) ) # F-statistic
( F_pval <- pf(F_stat, k2, n-k2-1, lower.tail = FALSE) ) # p-value < 0.05 =>
# reziduuri heteroscedastice

( LM_stat <- n * r2 ) # LM-statistic
( LM_pval <- pchisq(q = LM_stat, df = k2, lower.tail = FALSE)) # p-value < 0.05 =>
# reziduuri heteroscedastice




# Metoda predefinita 
white_test(model_0)  
# deoarece p-value = 0.0002 < 0.05 => reziduurile sunt hetersocedastice =>
# ipoteza este incalcata



# Testam heteroscedasticitatea pentru variabila  log price ---------------------

# Model de regresie log price (log-lin)
model_6 <- lm(  lInches ~ lWeight.kg + lMemory.GB + lRam.GB, DATA)
summary(model_6)

# Reziduurile si valorile estimate, ridicarea lor la patrat
DATA %<>% mutate(uhat1 = resid(model_5),
                 yhat1 = fitted(model_5), 
                 uhat1sq = uhat1^2, 
                 yhat1sq = yhat1^2)

# Testul White ----p value =0.1810
white_test(model_5) 

# Ambele teste prezinta p-value > 0.1 => la 99% reziduurile sunt homoscedastice
# deci aceasta ipoteza nu este incalcata atunci cand schimbam forma functionala
# a variabilei dependente.
# Acesta este unul din procedeele de corectie a incalcarii ipotezei de hetero

summary(model_0)
# Model de regresie pentru pret cu erori standard robuste
coeftest(model_0, vcov. = vcovHC(model_0, type = "HC1"))
summary(model_5)
coeftest(model_5, vcov. = vcovHC(model_2, type = "HC1"))

# De aceasta data avem homoscedasticitate deoarece p-value > 0.1


# Weighted Least Squares (WLS) --------------------------------------------


model_WLS1 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA)

summary(model_WLS1)



# Inmultim toate variabilele si constanta cu 1/sqrt(sqrft)
DATA %<>% mutate(Inchesstar = Inches/sqrt(Inches),
                 Weightstar = Weight.kg/sqrt(Inches),
                 Ramstar = Ram.GB/sqrt(Inches),
                 Memorystar = Memory.GB/sqrt(Inches),
                 Pricestar=Price.Euro/sqrt(Inches),
                 constantstar = 1/sqrt(Inches))

#  WLS: etimam modelul cu variabilele transformate prin OLS
model_WLS2 <- lm(Pricestar ~ 0 + constantstar+ Inchesstar + Weightstar + Ramstar+Memorystar, 
                 DATA) 
summary(model_WLS2)

# Retestam sa vedem daca am scapat de heteroscedasticitate
bptest(model_WLS2) # p-value > 0.1 => la 99% reziduuri homoscedastice
white_test(model_WLS2) # p-value >0.05 => la 95% reziduuri homoscedastice
# Retestam sa vedem daca am scapat de heteroscedasticitate
bptest(model_WLS2) # p-value > 0.1 => la 99% reziduuri homoscedastice
white_test(model_WLS2) # p-value >0.05 => la 95% reziduuri homoscedasticeZ

# Ambele teste confirma la un nivel de semnificatie de cel putin 95% ca 
# ipoteza de homoscedasticitate a reziduurilor nu este incalcata dupa corectia
# prin MCMMP ponderat (WLS)


# Optimizarea modelului ----------------------------
# Vom testa pe acest model si ipoteza de normalitate a reziduurilor 
DATA %<>% mutate(residuals_WLS2 = residuals(model_WLS2))
ols_plot_resid_qq(model_WLS2)
jarque.bera.test(DATA$residuals_WLS2) # ipoteza de normalitate este incalcata =>
# trebuie sa aplicam si corectia normalitatii
ols_plot_cooksd_bar(model_WLS2) # eliminam punctul 1,608,2,373,506
# Eliminam punctele indentificate anterior cu distanta Cook
DATA_cook2 <- DATA[-c(1,608,2,373,506,116), ]
# Reestimam modelul
model_WLS_normalitate1 <- lm(Pricestar ~ 0 + constantstar+ Inchesstar + Weightstar + Ramstar+Memorystar, 
                             DATA_cook2) 
summary(model_WLS_normalitate1)
DATA_cook2 %<>% mutate(Ram_cook2 = resid(model_WLS_normalitate1)) # extragem reziduurile din model 
# Testam cu Jarque-Bera
jarque.bera.test(DATA_cook2$Ram_cook2) # nu sunt normal distribuite
ols_plot_cooksd_bar(model_WLS_normalitate1) # eliminam punctele 42, 63, 73,75,76,79 si 80
DATA_cook2 <- DATA_cook2[-c(42,63,73,75,76,79,80), ]
# reestimam
model_WLS_normalitate2 <- lm(Pricestar ~ 0 + constantstar+ Inchesstar + Weightstar + Ramstar + Memorystar, 
                             DATA_cook2) 
summary(model_WLS_normalitate2)
DATA_cook2 %<>% mutate(Ram_cook3 = resid(model_WLS_normalitate2)) # extragem reziduurile din model 
# Testam cu Jarque-Bera
jarque.bera.test(DATA_cook2$Ram_cook3) # reziduuri normal distribuite
ols_plot_resid_qq(model_WLS_normalitate2)






#AUTOCORELAREA-------------------------------------
model1 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA)
summary(model1)
acf(DATA$residuals)
#eoarece lagurile depasesc intervalul punctat, lent putem deduce ca reziduurile
# sunt autocorelate (lag0 = valoarea prezenta, lag1 = valoarea de ieri, lag2 = valoarea
# de acum 2 zile samd)
# Testul Durbin-Watson (ordinul 1)

# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

dwtest(model1) # p-value > 0.1

# Testul Breusch-Godfrey (order superior)

# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

bgtest(model1) # p-value < 0.1 
bgtest(model1, order = 1, order = 2) # =>
bgtest(model1, order = 3)
# reziduurile sunt autocorelate si la lag superior

# Corectarea autocorelarii - adaugam lag1 ca variabila independenta in modelul original

# Cream un nou set de date 
DATA_data <- data.frame(DATA, resid_mod1=model1$residuals)
# Cream variabila lag1 
DATA_data_1 <- slide(DATA_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
library(slider)
DATA_data_2 <- na.omit(DATA_data_1) # eliminam valorile NA
# Reimplementam modelul cu noua variabila lag1 adaugata in model
model2 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg, data=DATA_data_2)


# Retestam ipoteza pe noul model
# ACF 
acf(model2$residuals) # autocorelarea a disparut
# Durbin Watson 
dwtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
# Breusch-Godfrey 
bgtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
bgtest(model2, order = 2)

# Retestam ipoteza pe noul model
# ACF 
acf(model2$residuals) # autocorelarea a disparut
# Durbin Watson 
dwtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
# Breusch-Godfrey 
bgtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
bgtest(model2, order = 2)
nobs(model_0) > (model_0$rank - 1)

var(DATA$Weight.kg)
var(DATA$Inches)
var(DATA$Memory.GB) # toate valorile > 0 => ipoteza acceptata

mean(model_0$residuals) # medie aproape de 0 => ipoteza acceptata


# Ipoteza 6 - Testare multicoliniaritate
vif(model_0) # nu avem valori pt VIF > 10 => ipoteza acceptata


# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(DATA$Weight.kg, model_0$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(DATA$Inches, model_0$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(DATA$Memory.GB, model_0$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(DATA$Ram.GB, model_0$residuals) # p-value > 0.1 => nu sunt corelate

# => ipoteza acceptata


summary(model_0)
# MODELUL ESTE BUN PENTRU PROGNOZE
# Prognoze 
y_predicted <- predict(model_0, s = best_lambda, newx = x)

# Prognoza out-of-sample
new <- matrix(c(15, 5, 3, 1), nrow=1, ncol=4) 
predict(model_0, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq 


# testarea valorii lamda
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 

# Prognoze 
y_predicted <- predict(model_5, s = best_lambda, newx = x)

# Prognoza out-of-sample
new <- matrix(c(15, 5, 3, 1), nrow=1, ncol=4) 
predict(model_5, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq

##################################################################
#REGRESIE MULTIPLA--------------------------------------------------------------
#Regresia simpla de la care plecam
model_lsreg <- lm(formula = Price.Euro ~ Ram.GB+Memory.GB+Weight.kg+hasSSD, data=DATA)
summary(model_lsreg)

# Inspectarea autocorelarii cu ajutorul graficului ACF (autocorelare)
acf(model_lsreg$residuals)

#Daca Memoria creste cu 1 unitate => pretul creste cu 0.54

#Regresia multipla
model_mreg <- lm(formula = Price.Euro ~ Memory.GB + Ram.GB+hasSDD, data=DATA)
summary(model_mreg)
#Daca memoria creste cu 1 => pretul creste cu 0.17

model_mreg1 <- lm(formula = Price.Euro ~ Memory.GB + Ram.GB + Inches, data=DATA)
summary(model_mreg1)
#Daca memoria creste cu 1 => pretul creste cu 0.31
#AFISAREA COEFICIENTILOR:
coef(model_mreg1)
model_mreg1$coefficients
#valorile previzionate si reziduale
DATA %<>% mutate(pricehat = fitted(model_mreg1),
                 uhat = residuals(model_mreg1))
DATA %>%
  select(Price.Euro, pricehat, uhat) %>%
  stargazer(type = "text")
model_partial <- lm(Price.Euro ~ Memory.GB + Ram.GB, data=DATA)
summary(model_partial)

# predict residuals ehat
DATA %<>% mutate(ehat = resid(model_partial))
lm(Price.Euro ~ Memory.GB, DATA) %>% summary

#Bonitatea R:59-->59%variatie si R sq-->0.54
summary(model_lsreg)
summary(model_lsreg)$r.squared


#Bonitatea pentru regresia multipla cu 2 regresori

# Regresie multipla cu 2 regresori
summary(model_mreg)
summary(model_mreg)$r.squared
summary(model_mreg)$adj.r.squared


# Regresie multipla cu 3 regresori
summary(model_mreg1)
summary(model_mreg1)$r.squared
summary(model_mreg1)$adj.r.squared


#CALCULAREA LUI R SQ --- 0.323
y    <- DATA$Price.Euro
yhat <- fitted(model_lsreg)
ehat <- resid(model_lsreg)

SSE <- sum((yhat - mean(y))^2) # sum of squares estimates of errors 
SSR <- sum(ehat^2) # sum of squares residual
SST <- SSE + SSR # sum of squares total

k <- model_lsreg$rank - 1 #3
n <- nobs(model_lsreg) #663

# gradele de libertate pt reziduuri = n - k - 1
df_SSR <- n - k - 1 #659
# gradele de libertate total = n-1 = nr de observ - 1
df_SST <- n - 1 #662


# Formula pt R-squared
R_squared <- SSE/SST
# echivalent cu R-squared = 1 - SSR/SST
R_squared


# Formula pt R-squared ajustat
adj_R_squared <- 1 - (SSR/df_SSR)/(SST/df_SST)
adj_R_squared


# Multicoliniaritate folosind VIF ------------------------------------------

# VIF = variance inflation factor
# Multicoliniaritatea este intalnita atunci cand regresorii sunt
# puternici corelati intre ei 


DATA %>% 
  select(-Price.Euro) %>% 
  na.omit %>% # stergerea valorilor NA
  cor
(model_high_vif <- lm(Price.Euro ~ Memory.GB + Ram.GB+hasSSD, data=DATA))
vif(model_high_vif)

acf(DATA$residuals)

#CORECTAREA HETEROSCEDASTICITATII------------------------------------------------------

# Forma functionala log: log-log si log-lin -------------------------------

# Cream o variabila noua pe care o logaritmam cu ajutorul functiei log
DATA <- DATA %>% mutate(lRAM = log(Ram.GB),
                        lprice = log(Price.Euro))

# Log-log 
model_loglog <- lm(lprice ~ lRAM, DATA)
summary(model_loglog)

model_loglin <- lm(lprice ~ Ram.GB, DATA)
summary(model_loglog)

model_linlog <- lm(Price.Euro ~ lRAM, DATA)
summary(model_loglog)

# Grafic
ggplot(DATA, aes(x = lRAM, y = lprice)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

ggplot(DATA, aes(x = lRAM, y = Price.Euro)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

ggplot(DATA, aes(x = Ram.GB, y = lprice)) +
  geom_point() +
  geom_smooth(method = lm, se = F)


# TESTE DE HETEROSCEDASTICITATE------------------------------------------------


# Generam patratul si interactiunea variabilelor independente
DATA %<>% mutate(Ram.GBsq=Ram.GB^2)

# Testam heteroscedasticitatea pentru variabila price ------------------------

# Obtinem residuals(uhat) si valorile estimate(yhat), si le ridicam la patrat
DATA %<>% mutate(uhatsq = uhat^2,
                 pricehatsq = pricehat^2)

# Testul Breusch-Pagan  ------------------------------------------------------

# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)

# Regresia pentru testul Breusch-Pagan 
summary(model_lsreg)

# Metoda predefinita (folosind functii predefinite)
bptest(model_lsreg) # pe baza LM 
bptest(model_lsreg)
# deoarece p-value = 0.002 < 0.05 => reziduurile sunt hetersocedastice =>
# ipoteza este incalcata



# Testul White  --------------------------------------------------------------

# H0: erorile sunt homoscedastice (reziduurile sunt distribuite cu varianta egala)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)


# Metoda predefinita 
white_test(model_lsreg)  

#IPOTEZE DE NORMALITATE IN REZIDUURI-----------------
#Modelul de regresie pentru pretul laptopurilor
model_0 <- lm(formula = Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg+hasSSD, data=DATA)
summary(model_0)
DATA %<>% mutate(uhat = resid(model_0)) # extragem reziduurile din model 


#TEST SHAPIRO -WILK PENTRU DETECTAREA NORMALITATII
#p value<0.1 variabila nu este normal distribuita
shapiro.test(DATA$Price.Euro)
shapiro.test(DATA$lprice)
#TEST JARQUE-BERA pentru normalitate
if(!require(tseries)){install.packages('tseries')}
jarque.bera.test(DATA$Price.Euro)
jarque.bera.test(DATA$lprice)
#p value>0.1 variabila este normal distribuita
#TESTUL T PENTRU SEMNIFICATIA COEFICIENTILOR
#Model de regresie
model <- lm(Price.Euro ~ Ram.GB + Memory.GB + Inches+ Weight.kg,DATA)
summary(model)
(coefficient <- coef(DATA)["exper"])
# Afisarea erorii standard
(se <- vcov(model) %>% # matricea de varianta-covarianta
    diag %>% # extragerea diagonale
    sqrt %>% # radical
    .["exper"]) # obtinerea erirolor stadarde (se) ale regresorului "exper"

(tstat <- coefficient/se)
(df_r <- model$df.residual)

qt(p = 0.975, df = df_r, lower.tail = TRUE)

#p-value nu este mai mic de  0.1/0.05/0.01 deci ipoteza nula nu se respinge deci nu exista diferente semnificative fata de valorile testate


2 * pt(q = abs(tstat), df = df_r, lower.tail = FALSE)
pt(q = tstat, df = df_r, lower.tail = FALSE)
pt(q = tstat, df = df_r, lower.tail = TRUE)
qt(p = 0.975, df = df_r)
coefficient - 1.96 * se

# Valoarea critica pentru nivel de semnificatie de 10%
qt(p = 0.95, df = df_r)
# Valoarea minima a intervalului pentru 90% nivel de incredere
coefficient - 1.65 * se
# Valoarea maxima a intervalului pentru 90% nivel de incredere
coefficient + 1.65 * se
#TESTAREA NORMALITATII REZIDUURILOR

-----------#RESIDUAL VS FITTED VALUES-------
# Testarea normalitatii rezidurilor--------
# 1. Graficul 'Residuals vs Fitted'
plot(model_mreg1) #NU SUNT NORMAL DISTRIBUITE

# 2. Graficul 'q-q Plot'
plot(model_mreg1) #NU SUNT NORMAL DISTRIBUITE

# 3. Histograma rezidurilor
ggplot(data=DATA) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey') +
  xlab('Residuals') +
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element.text(hjust = 0.5))

# skewness
skewness(DATA$uhat)

#kurtosis
kurtosis(DATA$uhat)

# 4. Boxplot
ggplot(DATA, aes(x = uhat, y = Price.Euro)) + 
  geom_boxplot() + 
  theme_bw() + 
  xlab('Reziduuri') + 
  ylab('Pret') + 
  ggtitle('Box plot Reziduuri') + 
  theme(plot.title = element_text(hjust = 0.5))

# 5. Teste specifice
#Jargue-Bera
jarque.test(DATA$uhat)

#libraria olsrr
ols_test_normality(model_mreg1)

# In urma testelor rezulta ca rezidurile nu sunt normale distribuite
ols_plot_cooksd_bar(model_mreg1)
ols_plot_cooksd_chart(model_mreg1)

#Eliminam randul 116
DATA_cook <- DATA[-c(116),]

#Reestimam modelul
model_mreg2 <- lm(formula = Price.Euro ~ Memory.GB + Ram.GB + Inches, data=DATA_cook)
summary(model_mreg2)
DATA %<>% mutate(uhat_cook = residuals(model_mreg2)) #extragem residuurile din noul model
#Retestam modelul
ols_plot_cooksd_bar(model_mreg2)
#In continuare exista valori outlier care trebuie eliminate
DATA_cook <- DATA[-c(116,42,51,68,85,111,132,143,148,159,169,203,264,290,268,372,431,482,505,582,581,624,606,604,607,583,600,562,255,252,140,357,415,487,575,574),]
model_mreg2 <- lm(formula = Price.Euro ~ Memory.GB + Ram.GB + Inches, data=DATA_cook)
summary(model_mreg2)
DATA %<>% mutate(uhat_cook = residuals(model_mreg2))
ols_plot_cooksd_bar(model_mreg2)

# Testam cu Jarque-Bera
jarque.test(DATA_cook$uhat)

#Refacem toate testele
ols_plot_resid_fit(model_mreg2)
ols_plot_resid_qq(model_mreg2)
ols_plot_resid_hist(model_mreg2)
ols_plot_resid_box(model_mreg2)
ols_test_normality(model_mreg2)

#Testarea heteroschedasticitatii-------------------------------
#1. Teste de heteroscedasticitate----
DATA %<>% mutate(Weight.kgsq = Weight.kg^2,
                 Inchessq = Inches^2,
                 Memory.GBsq = Memory.GB^2,
                 Ram.GBsq=Ram.GB^2,
                 Weight.kgXInches = Weight.kg*Inches,
                 Weight.kgXMemory.GB = Weight.kg*Memory.GB,
                 Weight.kgXRam.GB = Weight.kg*Ram.GB,
                 InchesXRam.GB = Inches*Ram.GB,
                 InchesXMemory.GB = Inches*Memory.GB,
                 Ram.GBXMemory.GB = Ram.GB*Memory.GB)

#2. Testul Breusch-Pagan-----
model_BP <- lm(formula = Price.Euro ~ Memory.GB + Ram.GB + Inches, data=DATA)
summary(model_BP)

(k1 <- model_BP$rank - 1)

(r2 <- summary(model_BP)$r.squared)
(n <- nobs(model_BP))

(F_stat <- (r2/k1) / ((1-r2)/(n-k1-1)) )
(F_pval <- pf(F_stat, k1, n-k1-1, lower.tail = FALSE))

(LM_stat <- n * r2)
(LM_pval <- pchisq(q = LM_stat, df = k1, lower.tail = FALSE))

# Metoda predefinita
bptest(model_0)

#3. Testul White----
model_White <- lm(formula = Price.Euro ~ Inches + Weight.kg + Memory.GB + Ram.GB+Inchessq + Weight.kgsq + Memory.GBsq + Ram.GBsq+Weight.kgXInches+
                    Weight.kgXMemory.GB+Weight.kgXRam.GB+InchesXRam.GB+InchesXMemory.GB+Ram.GBXMemory.GB, data=DATA)
summary(model_White)

(k2 <- model_White$rank - 1)

(r2 <- summary(model_White)$r.squared)
(n <- nobs(model_White))

( F_stat <- (r2/k2) / ((1-r2)/(n-k2-1)) )
( F_pval <- pf(F_stat, k2, n-k2-1, lower.tail = FALSE) )

( LM_stat <- n * r2 )
( LM_pval <- pchisq(q = LM_stat, df = k2, lower.tail = FALSE))

# Metoda predefinita 
white_test(model_0)  

# Autocorelarea ------------------------------
dwtest(model_mreg2) # p-value > 0.1 => reziduuri nonautocorelate
acf(model_mreg2$residuals) # nu sunt autocorelate
bgtest(model_mreg2) # p-value > 0.1 => reziduuri nonautocorelate 
# ipoteza acceptata
bgtest(model_mreg2, order=2)

# MODELUL ESTE BUN PENTRU PROGNOZE
summary(model_mreg2)
########################################################################

#METODE DE REGULARIZARE SI ALGORITMI DE SELECTIE AI VAR
#REGRESIA RIDGE ->multicoliniaritate in date


# Regresie liniara multipla
model0 <- lm(formula = Price.Euro ~ Memory.GB + Ram.GB + Inches +Weight.kg+has.SSD, data=DATA_cook)
summary(model0)
prognoza <- data.frame(Memory.GB = c(15),
                       Ram.GB = c(5),
                       Inches = c(3),
                       Weight.kg= c(1),
                       has.SSD=C(0))
y_pred_scenariu <- predict(model0, newdata = prognoza)
y_pred_scenariu

# Definim variabila raspuns
y <- DATA$Price.Euro

# Definim predictorii
x <- data.matrix(DATA[, c('Memory.GB', 'Ram.GB', 'Inches', 'Weight.kg','has.SSD')])

# Estimam modelul ridge (alpha = 0)
model <- glmnet(x, y, alpha = 0)
install.packages('glmnet')
summary(model)
install.packages('cv.glmnet')
# In continuare vom identifica valoarea lui lambda pt care avem MSE minimizat
# utilizand validarea incrucisata (cross validation)
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda # 0.15
# testarea valorii lamda 
plot(cv_model) 
# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 
# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:4, legend = colnames(x), cex = .5)
# Prognoze 
y_predicted <- predict(model, s = best_lambda, newx = x)
# Progoza out-of-sample
new <- matrix(c(15, 5, 3, 1), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)
# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
# Regresia LASSO - functioneaza similar cu regresie Ridge doar ca incearca sa minimizeze
# SSR + lambda*sum(|beta|)
model <- glmnet(x, y, alpha = 1)
# Din punct de vedere tehnic, vom seta valoarea alpha = 1 pentru 
# regresia LASSO. 
cv_model <- cv.glmnet(x, y, alpha = 1)
# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.006
# testarea valorii lamda
plot(cv_model) 
# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor  # coeficientii variabilelor 
# daca unul din coeficienti este 0 inseamna ca acea variabila nu este importanta
# si de aceea nu o estimeaza modelul
# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda",label=T)
legend("bottomright", lwd = 1, col = 1:4, legend = colnames(x), cex = .5)

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)
# Prognoza out-of-sample
new <- matrix(c(15, 5, 3, 1), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)
# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq 
# Elastic net regression - functioneaza similar cu Ridge si LASSO doar ca 
# adauga ambele penalitati SSR + lambda*sum(beta^2) + lambda*sum(|beta|)
model <- cv.glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.011
# testarea valorii lamda
plot(cv_model) 
# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 
# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)
# Prognoza out-of-sample
new <- matrix(c(15, 5, 3, 1), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)
# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 37.27%
# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim ------cel mai mare r^2

##################################################################

# Algoritmul Boruta 

# Vom converti variabilele categoricale in variabile factor 
convert <- c(5:10)
wage1[,convert] <- data.frame(apply(wage1[convert], 2, as.factor))

library(Boruta)
set.seed(111)
boruta.bank_train <- Boruta(Price.Euro~., data = DATA, doTrace = 2)
print(boruta.bank_train)

# Vom selecta atributele importante 
getSelectedAttributes(boruta.bank_train, withTentative = T)

# Vom reimplementa un model de regresie cu aceste atribute
model_boruta <- lm(Price.Euro ~ Memory.GB + Ram.GB + Inches +Weight.kg+ has.SSD, DATA)
summary(model_boruta) 

#APLICATIA 2-----------------------------------------------------------------



# Fisiere: 
#   panel_data.csv

PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}
data <- read_excel("C:/Users/criss/Desktop/Teste/panel_data.xlsx")
# Statistici descriptive
summary(data)

# Declararea setului de date de tip panel
#parametrul country cel de spatiu si year cel de timp
pd.df <- pdata.frame(data, index = c("country","year"), drop.index = TRUE)
#Corelatia din 2010-2019 Grafic care arata corelatia dintre pib si tari
coplot(solar_consumption~ year|country, type="l", data=data)
#In consola: View(pd.df) pentru a vizualiza tabelul care a introdus o variabila ce a unuit V de spatiu cu cea de timp
#Exemplu Austria-2010 etc
# Explorarea heterogeneitatii in sectiunea transversala
# Graficul traseaza un interval de incredere de 95% in jurul mediilor
# Tari cu rata foarte mare si tari cu rata foarte mica => avem heterogeneitate transversala

#In prima faza am studiat heterogeneitatea in randul tarilor
#cu ajutorul variabilei noastre independente (pib) si country
plotmeans(gdp  ~ country, main = 'Heterogeneitate in randul tarilor', data = data)
#Dupa analiza graficului putem trage concluzia ca avem heterogeneitate deoarece mediile tarilor nu se aliniaza in grafic

# Explorarea heterogeneitatii in sectiunea temporala-- adica in sectiunea de ani
# Ani cu rata mare si ani cu rata mica => avem heterogeneitate temporala, 
# dar mai mica decat in cazul heterogeneitatii transversale
plotmeans(gdp ~ year, main = 'Heterogeneitate in timp', data = data)
#In urma analizei plotului de mai sus putem raspunde la intrebarea daca avem heterogeneitate in timp. 
#Da, doar ca nu la fel de puternica cum este in cadrul tarilor

# Model OLS - model clasic de regresie liniara
# Nu ia in calcul heterogeneitatea intre spatiu si timp
ols <- lm(gdp ~ solar_consumption + wind_consumption, data)
summary(ols) #output
#modelul este valid datorita valorii lui p-value <2.2e-6
#Multiple R-squared:  0.8564,	Adjusted R-squared:  0.8501 

#Grafic-valorile estimate in raport cu pib-ul


remove.packages("ggplot2")
install.packages("ggplot2")

yhat <- ols$fitted # valori estimate
ggplot(data, aes(x=solar_consumption  , y=gdp))+ 
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  theme_bw()

#rulare model cu efece fixe 
#interpretare: consumul de energie solare si cea eoliana au impact asupra pib-ului celelalte variabile fiind nesemnificative
fe <- plm(gdp ~ solar_consumption + wind_consumption, data, index = c('country','year'),
          model = 'within')
#R sq 0.89, iar modelul este unul valid 
summary(fe)




# Alegerea celei mai adecvate variante de model prin testarea intre regresie 
# OLS vs fixed effects panel model pentru ca p value<0.05. (ALEGEM MODEL CU EFECTE FIXE)
# H0: FE 
# H1: OLS
pFtest(fe, ols) # p-value < 0.05 => se recomanda model de panel data FE

# Model cu efecte aleatorii RE (random effects)
#De acum vom rula modelul cu variabila semnidicativa wind_consumption
re <- plm(gdp ~ wind_consumption, data, index = c('country','year'),
          model = 'between')
summary(re)
#pentru efectul Random p-value 0.002.. este mai kic decat 0.05, variabila wind_consumtion este semnificatica
#iar R-sq 0.086

# Testul Hausmann il utilizam pentru a decide intre FE si RE
# H0: model cu efecte random 
# H1: model cu efecte fixe
phtest(fe,re) # p-value <0.05 => model cu efecte fixe se recomanda
#->>Ne uitam la probabilitate care este mai mica decat 0.05;
#--> deci testul Hausmann imi recomanda sa merg mai departe cu modelul cu efecte fixe


# Testare ipoteze model
# In urma testului Hausmann am decis sa utilizam modelul FE

# Testarea efectelor fixe in timp

fixed.time <- plm(gdp ~ wind_consumption + factor(year), data=data, index=c("country",
                                                                            "year"), model="within")
summary(fixed.time) #-->functia facotry de year imi va pune cate o variabila pt fiecare an

# H0:  nu sunt necesare efectele fixe in timp
# H1:  sunt necesare efectele fixe in timp
pFtest(fixed.time, fe) # p-value > 0.05 => nu se recomanda folosirea efectelor fixe in timp


# Cele doua teste sunt inconcluzive => vom alege varianta in care nu avem efecte fixe in timp

# Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
# Testul ne ajuta sa decidem intre RE si OLS 
pool <- plm(gdp ~ wind_consumption, data=data, index=c("country", "year"), model="pooling")
summary(pool)

# H0: variatiile in timp sunt 0
# H1: variatiile in timp sunt diferite de 0
plmtest(pool, type=c("bp")) # p-value < 0.05 => respingem ipoteza nula
# variatiile in timp sunt diferite de 0 => efectele aleatorii sunt adecvate a.i.
# exista diferente semnificative intre tari


# Testarea dependentei transversale folosind testul Breusch-Pagan LM si 
# testul Parasan CD

# Potrivit lui Baltagi, dependenta transversala este o problema intalnita
# in randul seturilor de date panel cu serii de timp lungi. Aceasta problema
# este rar intalnita in cazul panel data cu serii de timp scurte

# Ipoteze teste
# H0: reziduurile intre entitati nu sunt corelate
# H1: reziduurile intre entitati sunt corelate

pcdtest(fe, test = 'lm') # p-value <0.05 => dependenta transversala
pcdtest(fe, test = 'cd') # pvalue>0.05 => dependenta transversala
# Nu corectam pt ca avem panel mic. daca aveam serie de timp 40 perioade +
# trebuia sa corectam


# Testarea autocorelarii - Breusch-Godfrey/Wooldridge test 
# Testul se aplica doar cand seriile de timp sunt lungi. In acest caz nu 
# pune probleme setul de date deoarece avem date pe 12 ani

# H0: Nu exista autocorelate
# H1: autocorelarea este prezenta
pbgtest(fe) # p-value <0.05 => avem autocorelarem dar fiind panelul mic
# o vom ignora


# Testarea heteroschedasticitatii cu testul Breusch-Pagan
# H0: homoschedasticitate
# H1: heteroschedasticitate
bptest(gdp ~ wind_consumption + factor(country), data = data, studentize=F)
# deoarece p-value <0.05 => avem heteroschedasticitate



# Testarea efectelor random 
pFtest(re, ols) # p-value > 0.05 (0.722) => nu se recomanda efecte random.
plmtest(re, c('time'), type = 'bp') # p-value > 0.05 (0.20) => nu se recomanda efecte random
# Desi nu se recomanda, mai jos aveti testele care trebuie aplicate la proiect
# in cazul in care testul Hausmann recomanda efectele random
pbgtest(re) # p-value > 0.05 => nu exista autocorelare
bptest(gdp ~ wind_consumption + factor(year), data = data, studentize=F) # p-value > 0.05 (0.6149) => nu exista hetero
#Concluzie: Practic noi am mers mai departe cu acest model cu efecte fixe, iar testele ne au indicat ca modelul potrivit in cazul de fata
# pe setul nostru de date este cel cu efecte fixe incepand cu testul Hausmann, care a fost primul care ne a confirmat ca este potrivit
#modelul cu efecte fixe. Mai sus am facut testarea cu efecte random  adica sa tinem cont de dependenta in timp,
#practic fuctiile ne recomanda sa nu folosim testul efectelor random, iar testul de autocorelare ne spune ca nu exista autocorelare,
#iar in zona de heteor, nu am fi avut heteroschedasticitate daca am fi mers pe testarea folosind efecte random
