# Répertoire de travail et packages---------------
setwd("nomrepertoire")

packages <- c("forecast", "tidyverse", "naniar", "VIM", "mice", "missForest", "zoo", "imputeTS")

# Installation et chargement des packages
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) {
    install.packages(pack, dependencies = TRUE)  
    library(pack, character.only = TRUE)
  }
}



# Chargement des dataset------------------------

meteo2022 = read.csv(file = "meteo2022.csv",sep = ",",header = TRUE)
summary(meteo2022)

meteo2023 = read.csv(file = "meteo2023.csv",sep = ",",header = TRUE)
summary(meteo2023)

meteo2024 <- read.csv("meteo2024.csv", stringsAsFactors = FALSE)
summary(meteo2024)

df = rbind(meteo2022,meteo2023,meteo2024)
summary(df)

#  Mutate pour transformer les données manquantes en NA et pouvoir les visualiser
df <- df %>%mutate(Temp.max...C. = ifelse(Temp.max...C. == "", NA, Temp.max...C.))

df <- df %>%mutate(Temp.min...C. = ifelse(Temp.min...C. == "", NA, Temp.min...C.))

df <- df %>% mutate(Temp.moy...C. = ifelse(Temp.moy...C. == "", NA, Temp.moy...C.))

df <- df %>%mutate(DJC...C. = ifelse(DJC...C. == "", NA, DJC...C.))

df <- df %>% mutate(DJR...C. = ifelse(DJR...C. == "", NA, DJR...C.))

df <- df %>%mutate(Pluie.tot...mm. = ifelse(Pluie.tot...mm. == "", NA, Pluie.tot...mm.))

df <- df %>% mutate(Neige.tot...cm. = ifelse(Neige.tot...cm. == "", NA, Neige.tot...cm.))

df <- df %>%mutate(Précip..tot...mm. = ifelse(Précip..tot...mm. == "", NA, Précip..tot...mm.))

df <- df %>%mutate(Neige.au.sol..cm. = ifelse(Neige.au.sol..cm. == "", NA, Neige.au.sol..cm.))

df <- df %>%mutate(Dir..raf..max...10s.deg. = ifelse(Dir..raf..max...10s.deg. == "", NA, Dir..raf..max...10s.deg.))

df <- df %>%mutate(Vit..raf..max...km.h. = ifelse(Vit..raf..max...km.h. == "", NA, Vit..raf..max...km.h.))

df_clean <- df%>%select(-Longitude..x., -Latitude..y., - Nom.de.la.Station, -Année, -Mois, -Jour,
                  -ID.climatologique, -Qualité.des.Données,-Temp.max..Indicateur,
                  -Temp.min..Indicateur,-Temp.moy..Indicateur,-DJC.Indicateur,-DJR.Indicateur,
                  -Pluie.tot..Indicateur,-Neige.tot..Indicateur,-Neige.au.sol.Indicateur,-Précip..tot..Indicateur ,-Dir..raf..max..Indicateur,
                  -Vit..raf..max..Indicateur,-Dir..raf..max...10s.deg.,-Vit..raf..max...km.h., -DJC...C., - DJR...C. , -Précip..tot...mm., -Neige.au.sol..cm. )

df_clean$Date.Heure <- as.Date(df_clean$Date.Heure)
summary(df_clean)



for (i in 2:6) {
  df_clean[,i] <- gsub(",", ".", df_clean[,i])  # Remplace virgule par point 
  df_clean[,i] <- gsub("[^0-9.-]", "", df_clean[,i])  # on enlève les caractères non numériques sauf "." et "-"
  df_clean[,i] <- as.numeric(df_clean[,i]) 
}



summary(df_clean)
head(df_clean)


# visualisation de l'évolution des températures avec les données manquantes

par(mfrow=c(3,1)); 
na_index <- which(is.na(df_clean$Temp.max...C.))
plot.ts(df_clean$Temp.max...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures maximales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)

na_index <- which(is.na(df_clean$Temp.min...C.))
plot.ts(df_clean$Temp.min...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures minimales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)

na_index <- which(is.na(df_clean$Temp.moy...C.))
plot.ts(df_clean$Temp.moy...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Température moyenne avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.moy...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)

par(mfrow=c(2,1)); 

na_index <- which(is.na(df_clean$Pluie.tot...mm.))
plot.ts(df_clean$Pluie.tot...mm., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Quantité totale de pluie avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Pluie.tot...mm., na.rm = TRUE), length(na_index)), col = "red", pch = 4)

na_index <- which(is.na(df_clean$Neige.tot...cm.))
plot.ts(df_clean$Neige.tot...cm., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Quantité totale de neige avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Neige.tot...cm., na.rm = TRUE), length(na_index)), col = "red", pch = 4)



# Exploration des données
str(df_clean)
summary(df_clean)
n_miss <- sapply(df_clean, function(x) sum(is.na(x)))
print(n_miss)  # Nombre de valeurs manquantes pour chaque variable

par(mfrow=c(1,1)); 
vis_miss(df_clean)  # visualisation des valeurs manquantes



# Imputation des valeurs manquantes

par(mfrow=c(2,1)); 


#-----------------------------------------------------------

# Imputation par la moyenne 
df_imputed_mean <- df_clean %>%mutate(across(where(is.numeric), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)))

# Vérification graphique pour voir si c'est cohérent
na_index <- which(is.na(df_clean$Temp.max...C.))
plot.ts(df_imputed_mean$Temp.max...C., main = "Température maximales après imputation par la moyenne")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.max...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures maximales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)    


# comparaison des moyennes, médianes etc pour voir si c'est cohérent
summary(df_clean)
summary(df_imputed_mean)


# puis comparaison des densités pour voir si ça colle

ggplot() +
  geom_density(data = df, aes(x = df_clean$Temp.max...C.), color = "black", na.rm = TRUE) +
  geom_density(data = df_imputed_mean, aes(x = df_imputed_mean$Temp.max...C.), color = "red", alpha = 0.5) +
  ggtitle("Densité avant/après imputation par la moyenne") +
  theme_minimal()


# vérification de la cohérence temporelle

acf(df_clean$Temp.max...C., na.action = na.pass, main = "ACF avant imputation")
acf(df_imputed_mean$Temp.max...C., na.action = na.pass, main = "ACF après imputation")

ggplot() +
  geom_line(data = df_imputed_mean, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "red") +
  geom_line(data = df_clean, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "black") +
  ggtitle("Moyenne mobile (7 jours) avant/après imputation") +
  theme_minimal()

cor <- cor(df_clean[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.", "Pluie.tot...mm.",   "Neige.tot...cm.")], use = "complete.obs")
cor_mean  <- cor(df_imputed_mean[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.","Pluie.tot...mm.",   "Neige.tot...cm.")])
print(cor)
print(cor_mean)






#-----------------------------------------------------------


## Imputation multiple par MICE

imp <- mice(df_clean, method = "pmm", m = 5, maxit = 10)
df_imputed_mice <- complete(imp)

# Vérification graphique pour voir si c'est cohérent
na_index <- which(is.na(df_clean$Temp.max...C.))
plot.ts(df_imputed_mice$Temp.max...C., main = "Températures maximales avec imputation par MICE")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.max...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures maximales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)     


# comparaison des moyennes, médianes etc pour voir si c'est cohérent
summary(df_clean)
summary(df_imputed_mice)


# puis comparaison des densités pour voir si ça colle

ggplot() +
  geom_density(data = df_clean, aes(x = df_clean$Temp.max...C.), color = "black", na.rm = TRUE) +
  geom_density(data = df_imputed_mice, aes(x = df_imputed_mice$Temp.max...C.), color = "red", alpha = 0.5) +
  ggtitle("Densité avant/après imputation par MICE") +
  theme_minimal()


# vérification de la cohérence temporelle

acf(df_clean$Temp.max...C., na.action = na.pass, main = "ACF avant imputation")
acf(df_imputed_mice$Temp.max...C., na.action = na.pass, main = "ACF après imputation")

ggplot() +
  geom_line(data = df_imputed_mice, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "red") +
  geom_line(data = df_clean, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "black") +
  ggtitle("Moyenne mobile (7 jours) avant/après imputation") +
  theme_minimal()

cor <- cor(df_clean[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.", "Pluie.tot...mm.",   "Neige.tot...cm.")], use = "complete.obs")
cor_mice  <- cor(df_imputed_mice[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.","Pluie.tot...mm.",   "Neige.tot...cm.")])
print(cor)
print(cor_mice)







#---------------------------------------------------------------------



## Imputation avec `missForest`

df_clean <- df_clean %>% mutate(across(where(is.character), as.factor))
df_clean <- df_clean %>%mutate(across(where(is.factor), as.numeric))
# on transorme la colonne Date.Heure en numérique sinon ça ne fonctionne pas
df_clean$Date.Heure <- as.numeric(df_clean$Date.Heure)


df_imputed_mf <- missForest(df_clean)$ximp

# On remet Date.Heure en date pour la suite
df_clean$Date.Heure <- as.Date(df_clean$Date.Heure, origin = "1970-01-01")

# Vérification graphique pour voir si c'est cohérent
na_index <- which(is.na(df_clean$Temp.max...C.))
plot.ts(df_imputed_mf$Temp.max...C., main = "Températures maximales avec imputation par Miss Forest")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.max...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures maximales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)     


# comparaison des moyennes, médianes etc pour voir si c'est cohérent
summary(df_clean)
summary(df_imputed_mf)


# puis comparaison des densités pour voir si ça colle

ggplot() +
  geom_density(data = df, aes(x = df_clean$Temp.max...C.), color = "black", na.rm = TRUE) +
  geom_density(data = df_imputed_mf, aes(x = df_imputed_mf$Temp.max...C.), color = "red", alpha = 0.5) +
  ggtitle("Densité avant/après imputation pour miss forest") +
  theme_minimal()


# vérification de la cohérence temporelle

acf(df_clean$Temp.max...C., na.action = na.pass, main = "ACF avant imputation")
acf(df_imputed_mf$Temp.max...C., na.action = na.pass, main = "ACF après imputation")

ggplot() +
  geom_line(data = df_imputed_mf, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "red") +
  geom_line(data = df_clean, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "black") +
  ggtitle("Moyenne mobile (7 jours) avant/après imputation") +
  theme_minimal()

cor <- cor(df_clean[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.", "Pluie.tot...mm.",   "Neige.tot...cm.")], use = "complete.obs")
cor_mf <- cor(df_imputed_mf[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.","Pluie.tot...mm.",   "Neige.tot...cm.")])
print(cor)
print(cor_mf)


#-------------------------------------------------------------------------

# Imputation par interpolation


ts_tmax <- ts(df_clean$Temp.max...C., start = c(year(df$Date[1]), yday(df$Date[1])), frequency = 365)
ts_interpTmax <- na.approx(df_clean$Temp.max...C.)

ts_tmin <- ts(df_clean$Temp.min...C., start = c(year(df$Date[1]), yday(df$Date[1])), frequency = 365)
ts_interpTmin <- na.approx(df_clean$Temp.min...C.)

ts_tmoy <- ts(df_clean$Temp.moy...C., start = c(year(df$Date[1]), yday(df$Date[1])), frequency = 365)
ts_interpTmoy <- na.approx(df_clean$Temp.moy...C.)

ts_pluie <- ts(df_clean$Pluie.tot...mm., start = c(year(df$Date[1]), yday(df$Date[1])), frequency = 365)
ts_interpPluie <- na.approx(df_clean$Pluie.tot...mm.)

ts_neige <- ts(df_clean$Temp.max...C., start = c(year(df$Date[1]), yday(df$Date[1])), frequency = 365)
ts_interpNeige <- na.approx(df_clean$Neige.tot...cm.)



# Vérification graphique pour voir si c'est cohérent

na_index <- which(is.na(df_clean$Temp.max...C.))
plot.ts(ts_interpTmax, main = "Température max avec interpolation")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.max...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures maximales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)


# comparaison des moyennes, médianes etc pour voir si c'est cohérent
summary(df_clean)
summary(ts_interpTmax)




# vérification de la cohérence temporelle

acf(df_clean$Temp.max...C., na.action = na.pass, main = "ACF avant imputation")
acf(ts_interpTmax, na.action = na.pass, main = "ACF après imputation")

# L'interpolation fonctionne bien mais il faut faire les colonnes une par une et ça ne prend pas en compte les corrélations entre les variables


#-----------------------------------------------------------------

# Imputation par k-NN

df_clean$Jour_tot <- yday(df_clean$Date.Heure)

df_knn <- kNN(df_clean,k = 5, dist_var = "Jour_tot")
df_knn <- df_knn %>% select(-contains("_imp"))

# Vérification graphique pour voir si c'est cohérent
na_index <- which(is.na(df_clean$Temp.max...C.))
plot.ts(df_knn$Temp.max...C., main = "Températures maximales avec imputation par kNN")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.max...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures maximales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)   

# comparaison des moyennes, médianes etc pour voir si c'est cohérent
summary(df_clean)
summary(df_knn)


# puis comparaison des densités pour voir si ça colle

ggplot() +
  geom_density(data = df_clean, aes(x = df_clean$Temp.max...C.), color = "black", na.rm = TRUE) +
  geom_density(data = df_knn, aes(x = df_knn$Temp.max...C.), color = "red", alpha = 0.5) +
  ggtitle("Densité avant/après imputation pour knn") +
  theme_minimal()


#verification de la cohérence temporelle

acf(df_clean$Temp.max...C., na.action = na.pass, main = "ACF avant imputation")
acf(df_knn$Temp.max...C., na.action = na.pass, main = "ACF après imputation")

ggplot() +
  geom_line(data = df_knn, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "red") +
  geom_line(data = df_clean, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "black") +
  ggtitle("Moyenne mobile (7 jours) avant/après imputation") +
  theme_minimal()

cor <- cor(df_clean[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.", "Pluie.tot...mm.",   "Neige.tot...cm.")], use = "complete.obs")
cor_knn  <- cor(df_knn[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.","Pluie.tot...mm.",   "Neige.tot...cm.")])
print(cor)
print(cor_knn)

df_clean <- df_clean %>% select(-Jour_tot)


#----------------------------------------------------------------

# Imputation par moyenne mobile sur 7 jours

df_ma <- na_ma(df_clean, k = 7, weighting = "linear")  

# Vérification graphique pour voir si c'est cohérent
na_index <- which(is.na(df_clean$Temp.max...C.))
plot.ts(df_ma$Temp.max...C., main = "Températures maximales avec imputation par moyenne mobile sur 7 jours")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.max...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures maximales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)     


# comparaison des moyennes, médianes etc pour voir si c'est cohérent
summary(df_clean)
summary(df_ma)


# puis comparaison des densités pour voir si ça colle

ggplot() +
  geom_density(data = df_clean, aes(x = df_clean$Temp.max...C.), color = "black", na.rm = TRUE) +
  geom_density(data = df_ma, aes(x = df_ma$Temp.max...C.), color = "red", alpha = 0.5) +
  ggtitle("Densité avant/après imputation pour MA") +
  theme_minimal()


# vérification de la cohérence temporelle

acf(df_clean$Temp.max...C., na.action = na.pass, main = "ACF avant imputation")
acf(df_ma$Temp.max...C., na.action = na.pass, main = "ACF après imputation")

ggplot() +
  geom_line(data = df_ma, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "red") +
  geom_line(data = df_clean, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "black") +
  ggtitle("Moyenne mobile (7 jours) avant/après imputation") +
  theme_minimal()

cor<- cor(df_clean[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.", "Pluie.tot...mm.",   "Neige.tot...cm.")], use = "complete.obs")
cor_ma  <- cor(df_ma[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.","Pluie.tot...mm.",   "Neige.tot...cm.")])
print(cor)
print(cor_ma)

#-------------------------------------------------------------------------

# Imputation avec arima

df_arima <- na_kalman(df_clean, model = "auto.arima")

# Vérification graphique pour voir si c'est cohérent
na_index <- which(is.na(df_clean$Temp.max...C.))
plot.ts(df_arima$Temp.max...C., main = "Températures maximales avec imputation par moyenne mobile sur 7 jours")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.max...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures maximales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.max...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)    

# comparaison des moyennes, médianes etc pour voir si c'est cohérent
summary(df_clean)
summary(df_arima)


# puis comparaison des densités pour voir si ça colle

ggplot() +
  geom_density(data = df_clean, aes(x = df_clean$Temp.max...C.), color = "black", na.rm = TRUE) +
  geom_density(data = df_arima, aes(x = df_arima$Temp.max...C.), color = "red", alpha = 0.5) +
  ggtitle("Densité avant/après imputation pour arima") +
  theme_minimal()


# vérification de la cohérence temporelle

acf(df_clean$Temp.max...C., na.action = na.pass, main = "ACF avant imputation")
acf(df_arima$Temp.max...C., na.action = na.pass, main = "ACF après imputation")

ggplot() +
  geom_line(data = df_arima, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "red") +
  geom_line(data = df_clean, aes(x = Date.Heure, y = rollmean(Temp.max...C., 7, na.pad = TRUE)), color = "black") +
  ggtitle("Moyenne mobile (7 jours) avant/après imputation") +
  theme_minimal()

cor<- cor(df_clean[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.", "Pluie.tot...mm.",   "Neige.tot...cm.")], use = "complete.obs")
cor_arima  <- cor(df_arima[, c("Temp.max...C.", "Temp.min...C.",  "Temp.moy...C.","Pluie.tot...mm.",   "Neige.tot...cm.")])
print(cor)
print(cor_arima)



#-------------------------------------------------------------------------

# Les graphiques températures min


par(mfrow=c(2,1)); 

na_index <- which(is.na(df_clean$Temp.min...C.))
plot.ts(df_imputed_mice$Temp.min...C., main = "Températures minimales pour MICE")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4) 
plot.ts(df_clean$Temp.min...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures minimales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)     # MICE ça fonctionne mal


na_index <- which(is.na(df_clean$Temp.min...C.))
plot.ts(df_imputed_mf$Temp.min...C., main = "Températures minimales pour Miss Forest")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4) 
plot.ts(df_clean$Temp.min...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures minimales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)     # MF ça fonctionne mal vers 550


na_index <- which(is.na(df_clean$Temp.min...C.))
plot.ts(df_imputed_mean$Temp.min...C., main = "Températures minimales mean")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4) 
plot.ts(df_clean$Temp.min...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures minimales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)     # Mean ça ne fonctionne pas vers 400 et 550


na_index <- which(is.na(df_clean$Temp.min...C.))
plot.ts(df_ma$Temp.min...C., main = "Températures minimales poour la moyenne mobile")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4) 
plot.ts(df_clean$Temp.min...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures minimales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)     # MA ça fonctionne bien



na_index <- which(is.na(df_clean$Temp.min...C.))
plot.ts(df_arima$Temp.min...C., main = "Températures minimales pour arima")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.min...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures minimales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)      # ARIMA ça fonctionne bien

na_index <- which(is.na(df_clean$Temp.min...C.))
plot.ts(ts_interpTmin, main = "Températures minimales pour l'interpolation")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)
plot.ts(df_clean$Temp.min...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures minimales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)


na_index <- which(is.na(df_clean$Temp.min...C.))
plot.ts(df_knn$Temp.min...C., main = "Températures minimales pour kNN")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)    
plot.ts(df_clean$Temp.min...C., type = "o", col = "blue", cex = 0.3, pch = 16, main = "Températures minimales avec données manquantes apparentes")
points(na_index, rep(min(df_clean$Temp.min...C., na.rm = TRUE), length(na_index)), col = "red", pch = 4)     # kNN ça fonctionne bien

















