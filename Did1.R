# Avete tutti scaricato R??

rm(list = ls()) # a cosa serve?

################# Codici utili ############
# pacchetti utili

library(foreign)
library(psych)
library(DataExplorer)
library(readxl)
library(ADati)
library(dplyr)

# Lo script, perché serve

# 1. Automazione
# 2. Riproducibilità
# 3. Organizzazione
# 4. Flessibilità
# 5. Comunità
# 6. Visualizzazione

# Diversi tipi di dati

numerico <- c(3,6.7,121)
Intero <- c(2L, 42L) # dove 'L' dichiara questo come un intero
logico <- c('TRUE','FALSE')
complesso <- c(7 + 5i) # dove 'i' è un numero immaginario
carattere <- c("a", "B", "c è terzo", "69")

class()
str()

# conversione

v1 <- c("1", "2", "3")
v2 <- c("4", "5", "6")
v3 <- v1 + v2

v1 <- as.numeric(v1)
v2 <- as.numeric(v2)
v3 <- v1 + v2

# Come settare la WD

setwd()

getwd()

# Esplorazione e manipolazione dei dati in R
# Ispezionare il dataset

head(dati)    # Mostra le prime righe
tail(dati)    # Mostra le ultime righe
dim(dati)     # Mostra il numero di righe e colonne
str(dati)     # Visualizza la struttura: tipi di colonne e anteprima dei dati
summary(dati) # Fornisce statistiche riassuntive

# Controllare i nomi delle colonne
names(dati)

# --------- Selezione dei dati --------- #
# Selezionare una colonna
dati$mathquiz   # Usando l'operatore $
dati[, "mathquiz"] # Usando il nome della colonna
dati[, 6]         # Selezionando per indice di colonna

# Selezione di righe e colonne
dati[1, ]   # Prima riga
dati[, 1:2] # Prime due colonne
dati[1:5, ]  # Prime cinque righe
dati[1:5, 1:2] # Prime cinque righe, prime due colonne

# Filtrare i dati (esempio: selezionare righe dove mathquiz > 40)
dati[dati$mathquiz > 40, ]

# --------- Visualizzazioni di base --------- #
# Istogramma della variabile mathquiz
hist(dati$mathquiz, main = "Istogramma Math Quiz", xlab = "Punteggio", col = "violet")

# Boxplot della variabile hr_base
boxplot(dati$hr_base, main = "Boxplot HR Base", ylab = "Valori", col = "purple")

# Grafico a dispersione tra mathquiz e statquiz
plot(dati$mathquiz, dati$statquiz, main = "Grafico a dispersione Math vs Stat", 
     xlab = "Math Quiz", ylab = "Stat Quiz", col = "blue", pch = 19)

# Grafico a barre per la colonna phobia
conteggio_tabella <- table(dati$phobia)
barplot(conteggio_tabella, main = "Grafico a Barre Phobia", col = "orange")

# --------- Manipolazione dei dati --------- #
# Aggiungere una nuova colonna: differenza tra hr_post e hr_pre
dati$hr_diff <- dati$hr_post - dati$hr_pre

# Rimuovere una colonna
dati <- dati[,-1] 

# Rinominare una colonna
names(dati)[names(dati) == "prevmath"] <- "previous_math"

# Ordinare i dati in base a mathquiz
dati_ordinati <- dati[order(dati$mathquiz), ]

# Salvare il dataset modificato
write.csv(dati_ordinati, file = "Sara_dataset")
write.table(dati_ordinati, file="Sara_dataset")



########### Es. 1.10 #############

# 1.10.1

# Come importare i dati, diversi tipi di file

# x file txt
dati_txt <- read.table("dati/Sara_dataset.txt", header = TRUE, sep = "")

# library(readxl) x file xls/xlsx
dati_xls <- read_xls("dati/Sara_dataset.xls")

# x file sav
dati_sav <- read.spss("dati/Sara_dataset.sav", to.data.frame = TRUE)

# 1.10.2

str(Sara_txt)
str(Sara_sav)
str(Sara_xls)

summary(Sara_txt)

table(Sara_txt$major)
table(Sara_txt$mathquiz)

table(Sara_xls[,4])
table(table(Sara_xls[,4]))

table(Sara_txt[,4])
table(table(Sara_txt[,4]))

# 1.10.4

dati_sav[57,]

dati_sav$hr_base

mean(dati_sav$hr_base)
median(dati_sav$hr_base)
sd(dati_sav$hr_base)
min(dati_sav$hr_base)
max(dati_sav$hr_base)

hist(Sara_xls$hr_base, breaks = 15, col = "purple")
hist(Sara_xls$hr_base, breaks = 15, col = "#b2bea6")
hist(dati_sav$hr_base, breaks = 15, col = "#c6a5c2")

boxplot(dati_sav$hr_base,col = "#ad7c85")

sum(is.na(Sara_xls$mathquiz))

######### Es. 1.11 ##########
# LEZIONE 20/03


# In un reparto psichiatrico di un ospedale del nord Italia 
# sono ricoverati 30 pazienti. Per ciascuno di essi sono state
# rilevate le seguenti informazioni: regione di residenza, 
# classe sociale (definita età), tipo di disturbo presentato,
# come bassa, media e alta), punteggio su una scala di ansia 
# (0 = poco ansioso, 7 = molto ansioso),
# I dati sono raccolti nelle pazienti.xls3.

# 1. Si importi il file in R.

rm(list=ls())

library(readxl)
pazienti <- read_excel("C:/Users/peruamb13370/Desktop/pazienti.xls")

# 2. Si specifichi il livello di misura di ciascuna variabile del data-set.

str(pazienti)
summary(pazienti)
head(pazienti)

# 3. Si produca la tabella di frequenze di ciascuna variabile del data-set
# nel modo più opportuno.

table(pazienti$cl.sociale)
pazienti$cl.sociale <- ordered(pazienti$cl.sociale, 
                               levels = c("Bassa", "Media", "Alta"))

str(pazienti$cl.sociale)

table(pazienti$regione)
cumsum(table(pazienti$regione)/length(pazienti$regione))
(table(pazienti$regione)/length(pazienti$regione))


table(pazienti$cl.sociale)

# Si producano i grafici a barre o istogrammi delle variabili del data-set.

head(pazienti)
par(mfrow = c(1,2))
barplot(table(pazienti$cl.sociale), col="green", main = "classe sociale")
hist(pazienti$ansia, breaks = 15, main = "ansia")
barplot(table(pazienti$regione))

# Si producano i grafici delle cumulate empiriche 
# per le variabili per cui abbia senso.
par(mfrow = c(1,1))
barplot(cumsum(table(pazienti$regione)/length(pazienti$regione)))

######## Es. 4.4 ###########

# Nel autor.dat sono riportati i punteggi di 12 soggetti su una 
# scala di autoritarismo ed una di pregiudizi sociali 
# (dati: Siegel & Castellan, 1992).
# Ci si chiede se vi sia una relazione significativa tra il livello
# di autoritarismo e il livello di pregiudizio dei soggetti interpellati.

# 1. Si importi il file autor.dat in R assegnandogli nome AP.

AP <- read.table(file.choose(), header = TRUE)

AP <- read.table("C:/Users/peruamb13370/Desktop/autor.dat", header = TRUE)

# 2. Si identifichino unità statistiche e variabili del data-frame. Per ciascuna variabile si
# definiscano le proprietà metriche.

str(AP)
summary(AP)

# 3. Si calcoli il campo di variazione dei punteggi di autoritarismo e pregiudizio.

range(AP$aut)

range(AP$preg)

# 4. Si determini l'80° percentile della variabile autoritarismo.

quantile(AP$aut, .80)

# Si rappresenti graficamente la distribuzione dei punteggi di autoritarismo (in ascissa) e
# pregiudizio (in ordinata); sulla base del grafico ottenuto si ipotizzi un valore plausibile di
# correlazione.

plot(AP$aut, AP$preg, pch = 19, col = "blue")

abline(lm(preg ~ aut, data = AP))


# 5. Si calcolino covarianza e correlazione tra autoritarismo e pregiudizio e si interpretino.

cov(AP$aut, AP$preg)

cor(AP$aut, AP$preg)

######## Es. 4.5 ##########

# Al campione di soggetti dell’esercizio 4.4 si aggiunge un nuovo soggetto
# che ottiene i seguenti punteggi: 55 nell’autoritarismo e 85 nel pregiudizio.

# 1. Si aggiunga il soggetto al data-frame AP (ottenuto al punto 1 dell’esercizio 4.4)

AP <- rbind(AP,c(13,55,85))

# 2. Si rappresenti nuovamente la distribuzione dei punteggi di autoritarismo e pregiudizio;
# sulla base del grafico ottenuto si ipotizzi un valore plausibile di correlazione.

plot(AP$aut, AP$preg)

# 3. Si calcolino covarianza e correlazione tra autoritarismo e pregiudizio e si interpretino.

cov(AP$aut,AP$preg) # covarianza

cor(AP$aut,AP$preg) # correlazione

# 4. Quali considerazioni si possono fare su questo ultimo soggetto, 
# confrontando le stime di correlazione ottenute con quelle dell’esercizio precedente?

############# Es. 4.6 ############

# Il file Hooker.dat contiene dati raccolti da J.Hooker sulle montagne 
# dell’Himalaya (cfr. Wei- sberg, 1985). Tali dati rappresentano le 
# temperature in gradi Fahrenheit (variabile temp) di ebollizione dell’acqua 
# a diversi valori di pressione atmosferica (mmhg; variabile press).

# 1. Si importi il file Hooker.dat in R.

X <- read.table( file.choose(), header = TRUE )

# 2.Si identifichino unità statistiche e variabili del data-frame. 
# Per ciascuna variabile si definiscano le proprietà metriche.

str(X)
summary(X)

# 3. Si calcolino moda, mediana e media della variabile temp. 
# Sulla base del risultato si valuti se la distribuzione di tale 
# variabile possa considerarsi simmetrica.

library( ADati )
moda(X$temp); median(X$temp); mean(X$temp)

# 4. Si valuti con un grafico opportuno la simmetria della distribuzione.

par( mfrow = c( 1, 2 ) )
hist( X$temp, col = 'violet', xlab = 'temperatura', main = '' )
boxplot( X$temp, col = 'red' )

# 5. Si produca un grafico in quattro parti (utilizzando il comando layout() 
# oppure par()) con i boxplot e qqplot per le variabili press e temp.

par( mfrow = c( 2, 2 ) )
boxplot( X$press, col = 'lightblue', main = 'pressione' )
boxplot( X$temp, col = 'lightblue', main = 'temperatura' )
qqnorm( X$press, col = 'red', pch = 19 )
qqline( X$press )
qqnorm( X$temp, col = 'red', pch = 19 )
qqline( X$temp )

# 6. Si produca il diagramma di dispersione relativo alle variabili press e 
# temp valutando se sia ipotizzabile una relazione lineare.

par( mfrow = c( 1, 1 ) )
plot( temp ~ press, data = X, pch = 19 )

# 7. Si calcolino covarianza e correlazione tra le variabili press e temp.

cov( X$press, X$temp ) # covarianza

cor( X$press, X$temp ) # correlazione

# 8. Si stimino i parametri della retta di regressione.

library(rstanarm)

fit <- stan_glm( temp ~ press, data = X, seed = 3 )

fit_lm <- lm( temp ~ press, data = X )


# 9. Si valutino graficamente gli assunti del modello utilizzando i 
# grafici dei residui ed individuando la presenza di eventuali casi anomali o influenti.

library( performance )
check_model( fit, 
             check = c("linearity","qq","homogeneity","outliers") )

pp_check( fit, nreps = 50 )

# 10. Si valuti se la stima del parametro β1 sia statisticamente significativa, 
# definendo le ipotesi H0 e H1.

coef(fit)

# 11. Si aggiunga al grafico ottenuto al punto 6 la retta di regressione teorica.

plot( temp ~ press, data = X, pch = 19 )
abline( fit, col = "red" )

# 12. Si stimi, sulla base dei parametri calcolati, il valore atteso di 
# temperatura con una pressione di 27.

# yˆ = 146.79 + 2.25X

coef( fit )[ 1 ] + coef( fit )[ 2 ] * 27

######## Esercizio 5.1 ############

# 1

rm(list=ls())

library(ADati)

data("monkeys")

aggregate(score ~ drug, data = monkeys, FUN = mean)
aggregate(score ~ drug, data = monkeys, sd)
aggregate(score ~ fdep, data = monkeys, mean)
aggregate(score ~ fdep, data = monkeys, sd)

# 3

my <- aggregate( score ~ drug*fdep, data = monkeys, mean )
sy <- aggregate( score ~ drug*fdep, data = monkeys, sd )
myData <- data.frame( my, sy = sy$score )
colnames( myData ) <- c( "drug", "fdep", "media", "ds" )

myData$fdep <- factor( myData$fdep )
myData$up <- myData$media + myData$ds
myData$low <- myData$media - myData$ds
myData$x <- 1:3 + rep(c(-.05,.05),each = 3)

# con errbar
library( Hmisc )
plot( media ~ x, data = myData, type = "n", 
      axes = FALSE, xlab = "tipo di farmaco", ylab = "risposte corrette", 
      ylim = c(0,18), xlim = c(.5,3.5))
with( myData, errbar( x, media, up, low, add = TRUE, errbar.col = fdep, 
                      col = fdep ) )
axis(1, 1:3, unique(myData$drug))
axis(2)
box()

# con ggplot2
library( ggplot2 )
ggplot( myData, aes( x, media, color = fdep )) +
  geom_pointrange( aes( ymin = low, ymax = up )) +
  scale_x_continuous( breaks = 1:3, labels = unique(myData$drug) ) + 
  xlab("tipo di farmaco") 

# 4

library(car)

leveneTest( score ~ drug * factor(fdep), data = monkeys )

library(rstanarm)

fit_1<- stan_glm(score ~ drug + fdep, data = monkeys, seed = 35)
summary(fit_1)

fit_2<- stan_glm(score ~ drug * fdep, data = monkeys, seed = 35)

summary(fit_2)

loo_fit1<- loo(fit_1)
loo_fit2<- loo(fit_2)

loo_compare(loo_fit1, loo_fit2)

LIST_models <- list(loo_fit1, loo_fit2)
WEIGHTS <- loo_model_weights(LIST_models)





