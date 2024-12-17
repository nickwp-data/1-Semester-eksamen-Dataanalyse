#Ola 3 opgave 4 og kode fra opgave 1 med resultater

#Til hentning af forbrugerforventninger og find realvæksten

library(dkstat)
library(tidyr)
library(ggplot2)
library(pls)

FORV <- list(
  INDIKATOR="*",
  TID="*"
)

FORV <- dst_get_data(table = "FORV1",query = FORV,lang = "da")

FORV <- pivot_wider(FORV, 
                    names_from = INDIKATOR, 
                    values_from = value, 
                    id_cols = TID)

FORV <- FORV[244:nrow(FORV),] #Fra 1995 og frem
FORV <- as.data.frame(FORV)
row.names(FORV) <- FORV[,1]
FORV <- FORV[,-1]

December_månede <- FORV[seq(12, nrow(FORV), by = 12), ]

December_månede_10_år <- December_månede[20:nrow(December_månede),]

December_gennemsnit <- colMeans(December_månede_10_år[, 1:13], na.rm = TRUE)

December_data <- data.frame(
  row.names = "2024-12-01", 
  t(December_gennemsnit))

colnames(December_data) <- colnames(FORV)
FORV <- rbind(FORV,December_data)

Kvartalvis <- data.frame(matrix(ncol = 13, nrow = 0,NA))

for (i in seq(1, nrow(FORV), by = 3)) {
  kvartal <- (FORV[i,] + FORV[i + 1,] + FORV[i + 2,])/3
  Kvartalvis <- rbind(Kvartalvis, kvartal)
}

K42024 <- Kvartalvis[120, ]
FORV <- Kvartalvis[-120, ]

#Til hentning af forbruget og find realvæksten

Forbrug <- dst_meta(table = "NKHC021", lang = "da")
print(Forbrug$variables)

Forbrugsamlet <- list(
  FORMAAAL="*",
  Tid="*",
  PRISENHED="2020-priser, kædede værdier",
  SÆSON="Sæsonkorrigeret"
)

Forbrugdata <- dst_get_data(table = "NKHC021",query = Forbrugsamlet,lang = "da")

Forbrug_opdelt <- pivot_wider(Forbrugdata, 
                              names_from = TID, 
                              values_from = value, 
                              id_cols = FORMAAAL)

Forbrug_opdelt <- as.data.frame(Forbrug_opdelt)

rownames(Forbrug_opdelt) <- Forbrug_opdelt[, 1]
Forbrug_opdelt <- Forbrug_opdelt[, -1]
Forbrug_opdelt <- as.data.frame(t(Forbrug_opdelt))

Forbrug_opdelt_Ialt <- data.frame(Forbrug_opdelt[, 1])

ændring_på_et_år <- data.frame(matrix(ncol = 16, nrow = 0,NA))

for (i in seq(5, nrow(Forbrug_opdelt), by = 1)) {
  årlig_ændring <- (Forbrug_opdelt[i,]/Forbrug_opdelt[i-4,]-1)*100
  ændring_på_et_år <- rbind(ændring_på_et_år, årlig_ændring)
}

Forbrug_opdelt_Ialt <- data.frame(Forbrug_opdelt_Ialt[5:nrow(Forbrug_opdelt_Ialt),])

#Realvækst af forbrug i alt
Forbrug_opdelt_Ialt$Realvækst <- ændring_på_et_år$`I alt`

rownames(Forbrug_opdelt_Ialt) <- rownames(ændring_på_et_år)
colnames(Forbrug_opdelt_Ialt) <- c("I alt mio kr.","realvækst")

#Nettotal - hvor meget er forbruger steget/faldet i kr. fra forrige år
Forbrug_opdelt_Ialt$nettoændring <- NA

for (i in seq(5, nrow(Forbrug_opdelt_Ialt), by = 1)) {
  årlig_ændring_kr <- Forbrug_opdelt_Ialt[i,1]-Forbrug_opdelt_Ialt[i-4,1]
  Forbrug_opdelt_Ialt[i,3] <- årlig_ændring_kr
}

Realvækst <- Forbrug_opdelt_Ialt[17:nrow(Forbrug_opdelt_Ialt),]

Realvækst_Procent <- as.data.frame(Realvækst[,2])
colnames(Realvækst_Procent) <- "Realvækst"

FORV01 <- cbind(FORV, Realvækst_Procent)
FORV01 <- FORV01[21:nrow(FORV01),]
FORV01$Kvartal <- rownames(FORV01)

DST_FTI <- as.data.frame(FORV01[,1])
colnames(DST_FTI) <- "Forbrugertillidsindikatoren"
FORV01 <- FORV01[,-1]

colnames(FORV01)
FORV01$`Priser i dag, sammenlignet med for et år siden` <- FORV01$`Priser i dag, sammenlignet med for et år siden`*-1
FORV01$`Arbejdsløsheden om et år, sammenlignet med i dag` <- FORV01$`Arbejdsløsheden om et år, sammenlignet med i dag`*-1

#### Opg. 1.1 - Kombinationsalgoritme i R ####

kombinationer <- list()
for (i in 1:12) {
  combos <- combn(1:12, i, simplify = FALSE)
  kombinationer <- append(kombinationer, combos)
}

Komb_DF <- data.frame(
  Antal = 1:length(kombinationer),
  Kombinationer = sapply(kombinationer, function(x) paste(x, collapse = "-"))
)

Mean_Komb <- data.frame(matrix(NA, nrow = nrow(FORV01), ncol = nrow(Komb_DF)))
rownames(Mean_Komb) <- rownames(FORV01)
colnames(Mean_Komb) <- Komb_DF$Kombinationer

r2_values <- numeric(ncol(Mean_Komb))
for (j in 1:ncol(Mean_Komb)) {
  cols <- kombinationer[[j]]
  avg_komb <- rowMeans(FORV01[, cols, drop = FALSE])
  Mean_Komb[, j] <- avg_komb
  model <- lm(FORV01$Realvækst ~ avg_komb)
  r2_values[j] <- summary(model)$r.squared
}

#### Opg. 1.2 - r^2 og forbrugertillidsindikatorer ####

#Find den højeste R²-værdi og den tilhørende kombination
bedste_r2 <- which.max(r2_values)
højeste_r2 <- r2_values[bedste_r2]
bedste_kombination <- kombinationer[[bedste_r2]]

print(max(r2_values)) 

print(bedste_kombination)
# 3, 7, 9, 11, 12
print(højeste_r2)
#0.441334 r^2

#Find de 5 bedste kombinationer baseret på R^2
bedste_5_r2 <- order(r2_values, decreasing = TRUE)
højeste_5_r2 <- r2_values[bedste_5_r2[1:5]]
bedste_5_kombinationer <- kombinationer[bedste_5_r2[1:5]]

print(bedste_5_kombinationer)
print(højeste_5_r2)

#Opret et nyt dataframe med udvalgte kolonner fra DI_ny
begge_indikatorer <- subset(FORV01, select = c(Realvækst, Kvartal))
begge_indikatorer <- cbind(begge_indikatorer,DST_FTI)

#Gennemsnittet af de 4 underspørgsmål fra DI
begge_indikatorer$DI_Indikator <- NA
for(i in 1:nrow(FORV01)){
  sum <- sum(FORV01[i,c(1,3,5,9)]/4)
  begge_indikatorer$DI_Indikator[i] <- sum 
}

#Den nye
begge_indikatorer$Bedste_Indikator <- NA
for(i in 1:nrow(FORV01)){
  sum <- sum(FORV01[i,c(3,7,9,11,12)]/5)
  begge_indikatorer$Bedste_Indikator[i] <- sum 
}

#DI indikator
model_DI <- lm(Realvækst ~ DI_Indikator, data = begge_indikatorer)
coef(model_DI)
summary(model_DI)
#R2 = 0.3181
cor(begge_indikatorer$Realvækst, begge_indikatorer$DI_Indikator)
#R = 0.56

#Bedste indikator
model_BI <- lm(Realvækst ~ Bedste_Indikator, data = begge_indikatorer)
coef(model_BI)
summary(model_BI)
#R2 = 0.44
cor(begge_indikatorer$Realvækst, begge_indikatorer$Bedste_Indikator)
#R = 0.66

begge_indikatorer$Kvartal <- as.Date(begge_indikatorer$Kvartal)

#### Opg 1.4 - forudsigelser ####

#Vi laver indikatoren med det fjerde kvartal
Bedste_Indikator_K4 <- rowMeans(K42024[, c(4, 8, 10, 12, 13)], na.rm = TRUE)
DI_Indikator_K4 <- rowMeans(K42024[, c(2, 4, 6, 10)], na.rm = TRUE)

#Forudsigelse med DI-indikator
summary(model_DI)
DI_Q4_2024 <- 2.25569 + 0.18078 * DI_Indikator_K4
DI_Q4_2024
#2024Q4 = 0.60

#Forudsigelse med BI-indikator
summary(model_BI)
BI_Q4_2024 <- -1.5489 + 0.2749 * Bedste_Indikator_K4
BI_Q4_2024
#2024Q4 = 1.74

#### Opg 1.5 - Mikroøkonomisk indikator ####
print(colnames(FORV01[,c(1,2,5,9,10,11,12)]))

kombinationer_mikro <- list()
r2_values_mikro <- numeric()

for (i in 1:7) {
  combs_mikro <- combn(c(1,2,5,9,10,11,12), i, simplify = FALSE)
  for (cols in combs_mikro) {
    if (length(cols) == 1) {
      avg_combination_mikro <- FORV01[, cols]
    } else {
      avg_combination_mikro <- rowMeans(FORV01[, cols])
    }
    model_mikro <- lm(FORV01$Realvækst ~ avg_combination_mikro)
    
    r2_mikro <- summary(model_mikro)$r.squared
    r2_values_mikro <- c(r2_values_mikro, r2_mikro)
    
    kombinationer_mikro <- c(kombinationer_mikro, list(cols))
  }
}

best_combination_index_mikro <- which.max(r2_values_mikro)
best_r2_mikro <- r2_values_mikro[best_combination_index_mikro]
best_combination_mikro <- kombinationer_mikro[[best_combination_index_mikro]]

print(best_combination_mikro)
# 9 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.
print(best_r2_mikro)
#0.3457733 r^2

sorted_r2_indices_mikro <- order(r2_values_mikro, decreasing = TRUE)
best_5_r2_mikro <- r2_values_mikro[sorted_r2_indices_mikro[1:5]]
best_5_combinations_mikro <- kombinationer_mikro[sorted_r2_indices_mikro[1:5]]
print(best_5_combinations_mikro)
print(best_5_r2_mikro)

begge_indikatorer$Mikro_bedst <- NA
for(i in 1:nrow(FORV01)){
  sum <- sum(FORV01[i,c(9,11)]/2)
  begge_indikatorer$Mikro_bedst[i] <- sum 
}

#BI_mikro
model_BI_mikro <- lm(Realvækst ~ Mikro_bedst, data = begge_indikatorer)
summary(model_BI_mikro)
#0.33 r^2
cor(begge_indikatorer$Realvækst, begge_indikatorer$Mikro_bedst)
#0.58 r



#### Opg 4.1 - Test af model fra opgave 1 ####

#Fjerner de første 10 år
r2_stabilitet <- list()

# Loop for at fjerne rækker og beregne R^2 værdier
for (antal_fjernede in 1:40) {
  r2_values <- numeric(ncol(Mean_Komb))
  
  for (j in 1:ncol(Mean_Komb)) {
    cols <- kombinationer[[j]]
    
    avg_komb <- rowMeans(FORV01[-(1:antal_fjernede), cols, drop = FALSE], na.rm = TRUE)
    Mean_Komb[-(1:antal_fjernede), j] <- avg_komb
    
    model <- lm(FORV01$Realvækst[-(1:antal_fjernede)] ~ avg_komb)
    r2_values[j] <- summary(model)$r.squared
  }
  r2_stabilitet[[antal_fjernede]] <- r2_values
}

# Saml R^2 værdier i en dataframe
stabilitet_df <- do.call(rbind, r2_stabilitet)
rownames(stabilitet_df) <- paste0("Fjernet_", 1:40, "_rækker")
colnames(stabilitet_df) <- Komb_DF$Kombinationer

# Beregn statistikker for hver kombination
Kombination_10_ældste_år <- data.frame(
  Kombinationsnavn = colnames(stabilitet_df),
  Gennemsnit = colMeans(stabilitet_df),
  Min = apply(stabilitet_df, 2, min),
  Max = apply(stabilitet_df, 2, max)
)

bedste_kombinationer <- apply(stabilitet_df, 1, function(x) names(which.max(x)))
kombination_frekvens <- table(bedste_kombinationer)
Kombination_10_ældste_år$Forekomst <- kombination_frekvens[match(Kombination_10_ældste_år$Kombinationsnavn, names(kombination_frekvens))]
Kombination_10_ældste_år$Forekomst[is.na(Kombination_10_ældste_år$Forekomst)] <- 0

# Beregn forekomster (frekvens) for hver kombination
kombination_frekvens <- table(bedste_kombinationer)

# Tilføj forekomst-kolonnen til Kombination_10_ældste_år
Kombination_10_ældste_år$Forekomst <- kombination_frekvens[match(Kombination_10_ældste_år$Kombinationsnavn, names(kombination_frekvens))]

# Udskift NA'er med 0 for kombinationer, der aldrig har været den bedste
Kombination_10_ældste_år$Forekomst[is.na(Kombination_10_ældste_år$Forekomst)] <- 0

# Sortér efter gennemsnitlig R^2 (eller andre kriterier, hvis ønsket)
Kombination_10_ældste_år <- Kombination_10_ældste_år[order(-Kombination_10_ældste_år$Gennemsnit), ]

# Vis top 10 kombinationer med forekomster
top_10_kombinationer <- head(Kombination_10_ældste_år, 10)
print(top_10_kombinationer)

#Fjerner de sidste 10 år
r2_stabilitet1 <- list()

# Loop for at fjerne fra række 99 til 59
for (antal_fjernede1 in 1:40) {
  r2_values1 <- numeric(ncol(Mean_Komb))
  
  for (j in 1:ncol(Mean_Komb)) {
    cols <- kombinationer[[j]]
    
    # Fjern rækker fra 99 til (99 - antal_fjernede1 + 1)
    fjern_rækker <- 99:(99 - antal_fjernede1 + 1)
    avg_komb <- rowMeans(FORV01[-fjern_rækker, cols, drop = FALSE], na.rm = TRUE)
    Mean_Komb[-fjern_rækker, j] <- avg_komb
    
    model <- lm(FORV01$Realvækst[-fjern_rækker] ~ avg_komb)
    r2_values1[j] <- summary(model)$r.squared
  }
  r2_stabilitet1[[antal_fjernede1]] <- r2_values1
}

# Saml R^2 værdier i en dataframe
stabilitet_df1 <- do.call(rbind, r2_stabilitet1)
rownames(stabilitet_df1) <- paste0("Fjernet_rækker_99_til_", 99 - (1:40) + 1)
colnames(stabilitet_df1) <- Komb_DF$Kombinationer

# Beregn statistikker for hver kombination
Kombination_10_nyeste_år <- data.frame(
  Kombinationsnavn = colnames(stabilitet_df1),
  Gennemsnit = colMeans(stabilitet_df1),
  Min = apply(stabilitet_df1, 2, min),
  Max = apply(stabilitet_df1, 2, max)
)

# Find bedste kombinationer og deres R^2 værdier
bedste_kombinationer1 <- apply(stabilitet_df1, 1, function(x) names(which.max(x)))
bedste_r2_værdier1 <- apply(stabilitet_df1, 1, max)

# Beregn frekvensen af hver bedste kombination
kombination_frekvens1 <- table(bedste_kombinationer1)
kombination_frekvens_df1 <- as.data.frame(kombination_frekvens1)
colnames(kombination_frekvens_df1) <- c("Kombination", "Forekomst")
kombination_frekvens_df1 <- kombination_frekvens_df1[order(-kombination_frekvens_df1$Forekomst), ]

# Saml resultater i én dataframe
resultater1 <- data.frame(
  Antal_fjernede_rækker = paste0("99_til_", 99 - (1:40) + 1),
  Bedste_Kombination = bedste_kombinationer1,
  Højeste_R2 = bedste_r2_værdier1
)

# Beregn samlet statistik for de højeste R^2 værdier
højeste_r2_statistik1 <- data.frame(
  Min_Højeste_R2 = min(resultater1$Højeste_R2),
  Max_Højeste_R2 = max(resultater1$Højeste_R2),
  Gennemsnit_Højeste_R2 = mean(resultater1$Højeste_R2)
)

# Tilføj forekomst-kolonnen til Kombination_10_nyeste_år
Kombination_10_nyeste_år$Forekomst <- kombination_frekvens1[match(Kombination_10_nyeste_år$Kombinationsnavn, names(kombination_frekvens1))]
Kombination_10_nyeste_år$Forekomst[is.na(Kombination_10_nyeste_år$Forekomst)] <- 0

# Sortér efter gennemsnitlig R^2
Kombination_10_nyeste_år <- Kombination_10_nyeste_år[order(-Kombination_10_nyeste_år$Gennemsnit), ]

# Vis top 10 kombinationer med forekomster
top_10_kombinationer1 <- head(Kombination_10_nyeste_år, 10)

# Print resultater
print(resultater1)
print(kombination_frekvens_df1)
print(Kombination_10_nyeste_år)
print(højeste_r2_statistik1)
print(top_10_kombinationer1)



#### Grafisk udvikling ####

#find r2 for vores bedste i hvert kvartal
bedst_komb <- as.integer(c(3, 7, 9, 11, 12))

rækkenummer <- which(sapply(kombinationer, function(x) identical(x, bedst_komb)))

print(rækkenummer)

r2_stabilitet_bedste <- list()

for (antal_fjernede in 1:40) {
  r2_values <- numeric(ncol(Mean_Komb))
  
  cols <- kombinationer[[1453]]
    
    avg_komb <- rowMeans(FORV01[-(1:antal_fjernede), cols, drop = FALSE], na.rm = TRUE)
    Mean_Komb[-(1:antal_fjernede), 1453] <- avg_komb
    
    model <- lm(FORV01$Realvækst[-(1:antal_fjernede)] ~ avg_komb)
    r2_stabilitet_bedste[antal_fjernede] <- summary(model)$r.squared
}

#find r2 for højest gennemsnit kombination
højest_gennemsnit <- as.integer(c(3, 7, 8, 9, 11, 12))

rækkenummer_gennemsnit <- which(sapply(kombinationer, function(x) identical(x, højest_gennemsnit)))

print(rækkenummer_gennemsnit)

r2_stabilitet_højest_gennemsnit <- list()

for (antal_fjernede in 1:40) {
  r2_values <- numeric(ncol(Mean_Komb))
  
  cols <- kombinationer[[2422]]
  
  avg_komb <- rowMeans(FORV01[-(1:antal_fjernede), cols, drop = FALSE], na.rm = TRUE)
  Mean_Komb[-(1:antal_fjernede), 2422] <- avg_komb
  
  model <- lm(FORV01$Realvækst[-(1:antal_fjernede)] ~ avg_komb)
  r2_stabilitet_højest_gennemsnit[antal_fjernede] <- summary(model)$r.squared
}

DF_højest_gennemsnit <- as.data.frame(r2_stabilitet_højest_gennemsnit)
colnames(DF_højest_gennemsnit) <- rownames(begge_indikatorer[1:40,])

DF_bedste_indikator <- as.data.frame(r2_stabilitet_bedste)
colnames(DF_bedste_indikator) <- rownames(begge_indikatorer[1:40,])

DF_bedst_og_højest <- as.data.frame(NA)
DF_bedst_og_højest <- rbind(DF_bedste_indikator,DF_højest_gennemsnit)

rownames(DF_bedst_og_højest) <- c("Vores bedste","Højeste gennemsnit")


DF_long <- as.data.frame(DF_bedst_og_højest)

DF_long$Kategori <- rownames(DF_long)

DF_long <- pivot_longer(DF_long, 
                        cols = -Kategori, 
                        names_to = "Dato", 
                        values_to = "Værdi")
DF_long$Dato <- as.Date(DF_long$Dato, format = "%Y-%m-%d")

ggplot(DF_long, aes(x = Dato, y = Værdi, color = Kategori, group = Kategori)) +
  geom_line(size = 1) +
  scale_x_date(
    breaks = seq(min(DF_long$Dato), max(DF_long$Dato), by = "1 year"),
    date_labels = "%Y"
  ) +
  labs(
    title = "Med hele perioden er vores indikator bedre, med kortere dataperiode er den med det højeste gennemsnit bedre",
    x = "Startår for dataperiode",
    y = "R2",
    color = "Regression"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))