# Avete tutti scaricato R??

rm(list = ls()) # a cosa serve?

#########################
# pacchetti di oggi

library(foreign)
library(psych)
library(DataExplorer)
library(readxl)
library(ADati)
library(dplyr)

######################## 
# Lo script, perché serve

# 1. Automazione
# 2. Riproducibilità
# 3. Organizzazione
# 4. Flessibilità
# 5. Comunità
# 6. Visualizzazione

#######################

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

########################
# Come settare la WD

setwd()

getwd()

########################

# Es. 1.10 

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



############### codici utili
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









