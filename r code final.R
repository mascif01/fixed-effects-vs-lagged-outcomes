#l'oggetto studio di questa analisi è la stima dell'effetto delle carte Bancomat sulla spesa familiare, dunque l'individuazione di un "nesso causa-effetto"

#caricamento librerie
library(readxl)
library(AER)
library(sandwich)
library(stargazer)
library(heaven)


#1) Modello ad effetti fissi
debitcard <- read_excel("dataframe_debitcard.xlsx")
View(debitcard)
str(debitcard)

#rimozione righe vuote
debitcard <- debitcard[,-1]
debitcard <- debitcard[,-5]

str(debitcard)

#$ spending1998         : num [1:564] 1000 1100 2000 1500 1000 1100 4000 1200 1200 3500 ...
#$ debitcard1998        : chr [1:564] "0" "1 (Yes)" "1 (Yes)" "0" ...
#$ cash_inventory       : num [1:564] 1000 500 500 900 500 500 1800 800 500 2000 ...
#$ interest_rate        : num [1:564] 4.82 4.93 5.37 4.93 5.44 ...
#$ wealth               : chr [1:564] "1575000" "412746.9" "103000" "200100" ...
#$ income               : num [1:564] 64868 61375 40429 40283 33113 ...
#$ spending1995         : num [1:564] 3000 1200 2000 900 1000 600 3000 1600 1000 3000 ...
#$ debitcard1995        : num [1:564] 0 0 0 0 0 0 0 0 0 0 ...
#$ num_of_banks         : num [1:564] 9 2 11 17 4 16 64 9 10 30 ...
#$ num_of_inhabitants   : chr [1:564] ">20,000 & <=40,000" ">40,000 & <=500,000" ">20,000 & <=40,000" ">20,000 & <=40,000" ...
#$ family_size          : num [1:564] 7 2 4 1 2 2 4 5 2 5 ...
#$ num_of_earners       : num [1:564] 4 2 3 1 2 1 2 1 1 3 ...
#$ average_age          : chr [1:564] ">30 & <=40" ">40 & <=50" "<=30" ">65" ...
#$ geograph_area        : chr [1:564] "South and Islands" "North" "North" "North" ...
#$ householder_age      : chr [1:564] ">50 & <=65" ">50 & <=65" ">40 & <=50" ">65" ...
#$ householder_education: chr [1:564] "elementary school" "elementary school" "bachelors degree" "elementary school" ...

attach(debitcard)
wealth <- as.numeric(wealth)
wealth
#la varibile da chr viene trasformata in num

debitcard1998
#per la variabile debitcard1998, invece, bisogna riassegnare i valori binari, nel dettaglio: "0 (No)" con 0 e "1 (Yes)" con 1

debitcard1998 <- ifelse(debitcard1998 == "0 (No)", 0, 1)

#definizione della variabile di trattamento e dell'outcome in termini di differenza
diff_spending <- spending1998 - spending1995
diff_debitcard <- debitcard1998 - debitcard1995

#modello privo di covariate
spending_diff_model <- lm(diff_spending ~ diff_debitcard)
summary(spending_diff_model)

#conferma che gli errori standard sono robusti all'eteroschedasticità
coeftest(spending_diff_model, vcov = vcovHC, type = "HC1")

#costruzione dell'intervalo di confidenza
rob_se <- diag(vcovHC(spending_diff_model, type = "HC1"))^0.5
rbind("lower" = coef(spending_diff_model) - qnorm(0.95) * rob_se,
      "upper" = coef(spending_diff_model) + qnorm(0.95) * rob_se)

#individuazione degli errori standard clustered per verificare che siano robusti all'autocorrelazione
coeftest(spending_diff_model, vcov = vcovHAC, type = "HC1")

rob_se_a <- diag(vcovHAC(spending_diff_model, type = "HC1"))^0.5

rbind("lower" = coef(spending_diff_model) - qnorm(0.95) * rob_se_a,
      "upper" = coef(spending_diff_model) + qnorm(0.95) * rob_se_a)


#modello con le covariate
spending_diff_model2 <- lm(diff_spending ~ diff_debitcard + income + householder_age + householder_education)
summary(spending_diff_model2)

coeftest(spending_diff_model2, vcov = vcovHC, type = "HC1")
rob_se <- diag(vcovHC(spending_diff_model2, type = "HC1"))^0.5
rbind("lower" = coef(spending_diff_model2) - qnorm(0.95) * rob_se_a,
      "upper" = coef(spending_diff_model2) + qnorm(0.95) * rob_se_a)

coeftest(spending_diff_model2, vcov = vcovHAC, type = "HC1")

#rappresentazione grafica dei trend paralleli
media95_debit <- mean(spending1995[debitcard1998 == 1])
media95_debit #2026.816

media95_nodebit <- mean(spending1995[debitcard1998 == 0])
media95_nodebit #1758.727

media98_debit <- mean(spending1998[debitcard1998 == 1])
media98_debit #2027.654

media98_nodebit <- mean(spending1998[debitcard1998 == 0])
media98_nodebit #1641.974

plot(c(-2,-2,2,2), c(2026.816,1758.727,2027.654,1641.974),
     type = "p",
     xlim = c(-2,3.5),
     ylim = c(1500,2500),
     bty = "n",
     xlab = "tempo",
     ylab = "Livello medio di spesa mensile in  migliaia di lire",
     pch = 20,
     xaxt = "n")
axis(1, at = c(-2,2), labels = c("1995", "1998"))
lines(c(-2,2), c(1758.727, 1641.974), col = "deeppink3", lwd = 2)
lines(c(-2,2), c(2026.816, 2027.654), col = "seagreen", lwd = 2)
lines(c(-2,2), c(2026.816, 1910.063), col = "deeppink3", lty = 2, lwd = 2)
lines(c(2,2), c(1910.063, 2026.816), col = "navy", lty = 2, lwd = 2)

text(2, 1970, "effetto del trattamento", cex = 0.8, pos = 4)
text(-2, 1785, expression(bar(Y)[i0(0)]), cex = 0.8 , pos = 4)
text(-2, 2053, expression(bar(Y)[i0(1)]), cex = 0.8 , pos = 4)
text(2, 1641.974, expression(bar(Y)[i1(0)]), cex = 0.8 , pos = 4)
text(2, 2027.654, expression(bar(Y)[i1(1)]), cex = 0.8 , pos = 4)


#2) Modello con variabili dipendenti ritardate
library(haven)
f89 <- read_dta("f89tsqf2.dta")
View(f89)

f91 <- read_dta("f91tsqf.dta")
View(f91)

f93 <- read_dta("f93tsqf1.dta")
View(f93)


#visualizzati dunque i seguenti dataset ed operata l'azione di estrazione delle due colonne d'interesse da ciascuno di essi, si può procedere con il linkaggio dei record individuali (familiari). Cos? si ottiene un campione di unità per il quale si possiedono osservazioni per tutti e 5 gli anni, andando a risolvere la problematica legata alle numerosità campionarie dei vari anni che inizialmente non corrispondevano.

#f89 --> 8274 obs
#f91 --> 8188 obs
#f93 --> 8089 obs


#dataframe finale
datasetspese1989_1998 <- read_excel("datasetspese1989_1998.xlsx")
View(datasetspese1989_1998)
dim(datasetspese1989_1998)
str(datasetspese1989_1998)

#$ NQUEST               : num [1:129] 123 325 465 474 824 ...
#$ spending1989         : num [1:129] 5800 9800 8500 6000 4000 12000 16000 8000 10000 26000 ...
#$ spending1991         : num [1:129] 1200 1800 2500 700 1100 2000 3000 850 1000 2400 ...
#$ spending1993         : num [1:129] 900 800 1500 1100 800 2000 3500 1300 1000 2000 ...
#$ spending1995         : num [1:129] 1500 800 2000 1400 2500 600 4000 1800 1000 1500 ...
#$ spending1998         : num [1:129] 2000 1000 3000 1500 2500 1100 4500 1500 1500 1200 ...
#$ debitcard1998        : chr [1:129] "0 (No)" "1 (Yes)" "1 (Yes)" "0 (No)" ...
#$ cash_inventory       : num [1:129] 500 600 500 600 1000 500 500 1600 1000 2000 ...
#$ interest_rate        : num [1:129] 5.94 5.37 5.37 5.37 5.73 ...
#$ wrongrate            : num [1:129] 5944551 5374717 5374096 537406 5726669 ...
#$ wealth               : num [1:129] 830805 651800 103000 62600 663000 ...
#$ income               : num [1:129] 35074 35135 40429 29888 85020 ...
#$ debitcard1995        : num [1:129] 0 0 0 0 0 0 0 0 0 0 ...
#$ num_of_banks         : num [1:129] 1 22 11 20 42 16 23 27 35 4 ...
#$ num_of_inhabitants   : chr [1:129] ">40,000 & <=500,000" ">40,000 & <=500,000" ">20,000 & #<=40,000" ">40,000 & <=500,000" ...
#$ family_size          : num [1:129] 2 2 4 3 4 2 4 2 1 2 ...
#$ num_of_earners       : num [1:129] 2 1 3 1 3 1 2 2 1 1 ...
#$ average_age          : chr [1:129] ">65" ">50 & <=65" "<=30" ">40 & <=50" ...
#$ geograph_area        : chr [1:129] "Central Italy" "South and Islands" "North" "South and #Islands" ...
#$ householder_age      : chr [1:129] ">65" ">50 & <=65" ">40 & <=50" ">50 & <=65" ...
#$ householder_education: chr [1:129] "elementary school" "middle school" "bachelors degree" "middle school" ...


attach(datasetspese1989_1998)
debitcard1998 <- ifelse(debitcard1998 == "0 (No)", 0, 1)

#modello privo di covariate
modelmult <- lm(spending1998 ~ debitcard1998 + spending1989 + spending1991 + spending1993 + spending1995)
summary(modelmult)

coeftest(modelmult, vcov = vcovHC, type = "HC1")
rob_se <- diag(vcovHC(modelmult, type = "HC1"))^0.5
rbind("lower" = coef(modelmult) - qnorm(0.95) * rob_se,
      "upper" = coef(modelmult) + qnorm(0.95) * rob_se)

rob_se <- diag(vcovHC(modelmult, type = "HC1"))^0.5
rbind("lower" = coef(modelmult) - qnorm(0.875) * rob_se,
      "upper" = coef(modelmult) + qnorm(0.875) * rob_se)


dataset_91_93_95_98 <- read_excel("dataset 91-93-95-98.xlsx")
View(dataset_91_93_95_98)

#eliminazione colonne superflue
dataset_91_93_95_98 <- dataset_91_93_95_98[,-2]
dataset_91_93_95_98 <- dataset_91_93_95_98[,-3]

dim(dataset_91_93_95_98)
str(dataset_91_93_95_98)

#$ spending1991         : num [1:391] 1200 1800 2500 700 1100 3000 850 1000 2400 800 ...
#$ spending1993         : num [1:391] 900 800 1500 1100 800 3500 1300 1000 2500 1500 ...
#$ NQUEST98             : num [1:391] 123 325 465 474 824 ...
#$ spending1998         : num [1:391] 1000 1100 2000 1500 1000 4000 1200 1200 3500 1500 ...
#$ debitcard1998        : chr [1:391] "0 (No)" "1 (Yes)" "1 (Yes)" "0 (No)" ...
#$ cash_inventory       : num [1:391] 1000 500 500 900 500 1800 800 500 2000 2000 ...
#$ interest_rate        : num [1:391] 4823153 4925455 5374096 4925455 5438059 ...
#$ wealth               : chr [1:391] "1575000" "412746.9" "103000" "200100" ...
#$ income               : chr [1:391] "64867.56" "61375.43" "40428.54" "40283.11" ...
#$ debitcard1995        : num [1:391] 0 0 0 0 0 0 0 0 0 0 ...
#$ spending1995         : num [1:391] 3000 1200 2000 900 1000 3000 1600 1000 3000 2000 ...
#$ num_of_banks         : num [1:391] 9 2 11 17 4 64 9 10 30 10 ...
#$ num_of_inhabitans    : chr [1:391] ">20,000 & <=40,000" ">40,000 & <=500,000" ">20,000 & #<=40,000" ">20,000 & <=40,000" ...
#$ family_size          : num [1:391] 7 2 4 1 2 4 5 2 5 4 ...
#$ num_of_earners       : num [1:391] 4 2 3 1 2 2 1 1 3 1 ...
#$ average_age          : chr [1:391] ">30 & <=40" ">40 & <=50" "<=30" ">65" ...
#$ geograph_area        : chr [1:391] "South and Islands" "North" "North" "North" ...
#$ householder_age      : chr [1:391] ">50 & <=65" ">50 & <=65" ">40 & <=50" ">65" ...
#$ householder_education: chr [1:391] "elementary school" "elementary school" "bachelors degree" "elementary school" ...

attach(dataset_91_93_95_98)
debitcard1998 <- ifelse(debitcard1998 == "0 (No)", 0, 1)

#modello privo di covariate
modelmult2 <- lm(spending1998 ~ debitcard1998 + spending1991 + spending1993 + spending1995)
summary(modelmult2)

coeftest(modelmult2, vcov = vcovHC, type = "HC1")
rob_se <- diag(vcovHC(modelmult2, type = "HC1"))^0.5
rbind("lower" = coef(modelmult2) - qnorm(0.95) * rob_se,
      "upper" = coef(modelmult2) + qnorm(0.95) * rob_se)

dataset_93_95_98 <- read_excel("dataset 93-95-98.xlsx")
View(dataset_93_95_98)

#eliminazione colonne superflue
dataset_93_95_98 <- dataset_93_95_98[,-2]

dim(dataset_93_95_98)
str(dataset_93_95_98)

#$ spending1993         : num [1:503] 900 800 1500 1100 800 3500 1300 1000 2500 1500 ...
#$ NQUEST               : num [1:503] 123 325 465 474 824 ...
#$ spending1998         : num [1:503] 1000 1100 2000 1500 1000 4000 1200 1200 3500 1500 ...
#$ debitcard1998        : chr [1:503] "0 (No)" "1 (Yes)" "1 (Yes)" "0 (No)" ...
#$ cash_inventory       : num [1:503] 1000 500 500 900 500 1800 800 500 2000 2000 ...
#$ interest_rate        : num [1:503] 4823153 4925455 5374096 4925455 5438059 ...
#$ wealth               : chr [1:503] "1575000" "412746.9" "103000" "200100" ...
#$ income               : chr [1:503] "64867.56" "61375.43" "40428.54" "40283.11" ...
#$ debitcard1995        : num [1:503] 0 0 0 0 0 0 0 0 0 0 ...
#$ spending1995         : num [1:503] 3000 1200 2000 900 1000 3000 1600 1000 3000 2000 ...
#$ num_of_banks         : num [1:503] 9 2 11 17 4 64 9 10 30 10 ...
#$ num_of_inhabitants   : chr [1:503] ">20,000 & <=40,000" ">40,000 & <=500,000" ">20,000 & <=40,000" ">20,000 & <=40,000" ...
#$ family size          : num [1:503] 7 2 4 1 2 4 5 2 5 4 ...
#$ num_of_earners       : num [1:503] 4 2 3 1 2 2 1 1 3 1 ...
#$ average_age          : chr [1:503] ">30 & <=40" ">40 & <=50" "<=30" ">65" ...
#$ geograph_area        : chr [1:503] "South and Islands" "North" "North" "North" ...
#$ householder_age      : chr [1:503] ">50 & <=65" ">50 & <=65" ">40 & <=50" ">65" ...
#$ householder_education: chr [1:503] "elementary school" "elementary school" "bachelors degree" "elementary school" ...

#modello privo di covariate
modelmult3 <- lm(spending1998 ~ debitcard1998 + spending1993 + spending1995)
summary(modelmult3)

coeftest(modelmult3, vcov = vcovHC, type = "HC1")
rob_se <- diag(vcovHC(modelmult3, type = "HC1"))^0.5
rbind("lower" = coef(modelmult3) - qnorm(0.95) * rob_se,
      "upper" = coef(modelmult3) + qnorm(0.95) * rob_se)


income <- as.numeric(income)

#modello con le covariate
modelmult4 <- lm(spending1998 ~ debitcard1998 + spending1991 + spending1993 + spending1995 + income + householder_age + householder_education)
summary(modelmult4)

coeftest(modelmult4, vcov = vcovHC, type = "HC1")
rob_se <- diag(vcovHC(modelmult4, type = "HC1"))^0.5
rbind("lower" = coef(modelmult4) - qnorm(0.95) * rob_se,
      "upper" = coef(modelmult4) + qnorm(0.95) * rob_se)


#modello con le covariate
modelmult5 <- lm(spending1998 ~ debitcard1998 + spending1993 + spending1995 + income + householder_age + householder_education)
summary(modelmult5)

coeftest(modelmult5, vcov = vcovHC, type = "HC1")
rob_se <- diag(vcovHC(modelmult5, type = "HC1"))^0.5
rbind("lower" = coef(modelmult5) - qnorm(0.95) * rob_se,
      "upper" = coef(modelmult5) + qnorm(0.95) * rob_se)