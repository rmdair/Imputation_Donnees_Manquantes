# Répertoire de travail et packages---------------
setwd("nomrepertoire")

packages <- c("dplyr", "VIM", "mice", "laeken", "missForest")

# Installation et chargement des packages
for (pack in packages) {
  if (!require(pack, character.only = TRUE)) {
    install.packages(pack, dependencies = TRUE)  
    library(pack, character.only = TRUE)
  }
}



# Chargement du dataset--------------------
data <- read.csv("olympics-economics.csv", header = TRUE, sep = ",")

# constantes de stabilité
n.gold = sum(data$gold)
n.silver = sum(data$silver)
n.bronze = sum(data$bronze)
n.total = sum(data$total)

# Création de jeux de données avec valeurs manquantes-----------------
data.mar <- data %>% mutate(gold = ifelse(runif(nrow(data)) < 0.1, NA, gold),
                        silver = ifelse(runif(nrow(data)) < 0.1, NA, silver),
                        bronze = ifelse(runif(nrow(data)) < 0.1, NA, bronze))
# 10% de valeurs manquantes, générées aléatoirement

# Le total est NA si une des médailles est NA
data.mar$total <- ifelse(is.na(data.mar$gold) | is.na(data.mar$silver) | is.na(data.mar$bronze), NA, data.mar$total)

# Fonction pour calculer la moyenne pondérée en utilisant l'inverse du cube des distances
weightedMeanDistance <- function(x, distances) {
  # Calcul des poids en fonction des distances
  weights <- 1 / (distances^3)
  
  # Calcul de la somme pondérée des valeurs
  weighted_sum <- sum(x * weights)
  
  # Calcul de la somme des poids
  sum_weights <- sum(weights)
  
  # Calcul de la moyenne pondérée
  weighted_mean <- weighted_sum / sum_weights
  
  return(weighted_mean)
}

# Fonction pour arrondir la moyenne pondérée à un entier
weightedMeanInt <- function(x, distances, ...) {
  return(as.integer(round(weightedMeanDistance(x, distances, ...))))
}

# Imputation des valeurs manquantes---------------------
# Méthode k-nn avec la nouvelle fonction de moyenne pondérée
knnImpute <- kNN(data = data.mar, 
                        variable = c("gold", "silver", "bronze"), 
                        k = 2, 
                        metric = "euclidean", 
                        dist_var = c("gold", "silver", "bronze", "total"), 
                        weightDist = TRUE, 
                        numFun = weightedMeanInt, 
                        useImputedDist = FALSE)
knnImpute$total <- rowSums(knnImpute[,c("gold","silver","bronze")])

knnImpute.pop <- kNN(data = data.mar, 
                       variable = c("gold","silver","bronze"), 
                       k=2, 
                       metric = "euclidean", 
                       dist_var = c("gold","silver","bronze","total","population"),
                       weightDist = TRUE,
                       numFun = weightedMeanInt,
                       useImputedDist = FALSE)
knnImpute.pop$total <- rowSums(knnImpute.pop[,c("gold","silver","bronze")])

# Imputation par la méthode MICE
impMICE <- mice(data = data.mar, m = 5, maxit = 50, method = "pmm", printFlag = FALSE)
miceImpute <- complete(impMICE)
miceImpute$total <- rowSums(miceImpute[,c("gold","silver","bronze")])

# méthode missForest
# copie de data.mar
data.mar.missForest <- data.mar

# suppression des colonnes inutiles
data.mar.missForest <- data.mar.missForest[,-c(1,2)]

data.mar.missForest <- data.mar.missForest %>% mutate(across(where(is.character),as.numeric))

# Imputation par MissForest
missForestImpute <- missForest(data.mar.missForest, maxiter = 10, ntree = 100)
missForestImpute <- missForestImpute$ximp

# ajout des colonnes manquantes
missForestImpute <- cbind(data.mar[1:2], missForestImpute)

# Arrondissement des valeurs aux entiers
missForestImpute <- missForestImpute %>% mutate(across(where(is.numeric), round))
missForestImpute$total <- rowSums(missForestImpute[,c("gold","silver","bronze")])

# Etude de la qualité de l'imputation-----------------------------------------
# nombre de valeurs à prédire dans le cas MAR
n <- sum(is.na(data.mar$gold))
n <- n + sum(is.na(data.mar$silver))
n <- n + sum(is.na(data.mar$bronze))
n <- n + sum(is.na(data.mar$total))

# Nombre de valeurs mal prédites
n.diff.knn <- sum(data$gold != knnImpute$gold)
n.diff.knn <- n.diff.knn + sum(data$silver != knnImpute$silver)
n.diff.knn <- n.diff.knn + sum(data$bronze != knnImpute$bronze)
n.diff.knn <- n.diff.knn + sum(data$total != knnImpute$total)

n.diff.knn.pop <- sum(data$gold != knnImpute.pop$gold)
n.diff.knn.pop <- n.diff.knn.pop + sum(data$silver != knnImpute.pop$silver)
n.diff.knn.pop <- n.diff.knn.pop + sum(data$bronze != knnImpute.pop$bronze)
n.diff.knn.pop <- n.diff.knn.pop + sum(data$total != knnImpute.pop$total)

n.diff.mice <- sum(data$gold != miceImpute$gold)
n.diff.mice <- n.diff.mice + sum(data$silver != miceImpute$silver)
n.diff.mice <- n.diff.mice + sum(data$bronze != miceImpute$bronze)
n.diff.mice <- n.diff.mice + sum(data$total != miceImpute$total)

n.diff.mf <- sum(data$gold != missForestImpute$gold)
n.diff.mf <- n.diff.mf + sum(data$silver != missForestImpute$silver)
n.diff.mf <- n.diff.mf + sum(data$bronze != missForestImpute$bronze)
n.diff.mf <- n.diff.mf + sum(data$total != missForestImpute$total)

# précision de l'imputation
precision.knn <- 1 - n.diff.knn/n # l'imputation est peu précise, les similitudes au niveau des covariables ne semblent pas influencer grandement le nombre de médailles obtenues
precision.knn.pop <- 1 - n.diff.knn.pop/n
precision.mice <- 1 - n.diff.mice/n
precision.mf <- 1 - n.diff.mf/n

# copie du jeu de données complet
original_data <- data

# Calcul de la précision pour chaque valeur NA--------------------------------
# cas de la méthode k-nn avec la distance euclidienne
accuracy <- data.mar %>%
  mutate(gold_accuracy = ifelse(is.na(gold), abs(original_data$gold - knnImpute$gold) / pmax(original_data$gold, 1), NA),
         silver_accuracy = ifelse(is.na(silver), abs(original_data$silver - knnImpute$silver) / pmax(original_data$silver, 1), NA),
         bronze_accuracy = ifelse(is.na(bronze), abs(original_data$bronze - knnImpute$bronze) / pmax(original_data$bronze, 1), NA),
         total_accuracy = ifelse(is.na(total), abs(original_data$total - knnImpute$total) / pmax(original_data$total,1), NA))

# Ajustement des valeurs de précision
accuracy <- accuracy %>%
  mutate(gold_accuracy = ifelse(gold_accuracy >= 1, 0, 1 - gold_accuracy),
         silver_accuracy = ifelse(silver_accuracy >= 1, 0, 1 - silver_accuracy),
         bronze_accuracy = ifelse(bronze_accuracy >= 1, 0, 1 - bronze_accuracy),
         total_accuracy = ifelse(total_accuracy >= 1, 0, 1 - total_accuracy))

# Calcul du nombre de valeurs imputées
n_imputed_values <- sum(is.na(data.mar$gold)) + sum(is.na(data.mar$silver)) + sum(is.na(data.mar$bronze)) + sum(is.na(data.mar$total))

# Calcul de la précision moyenne
mean_accuracy.2nn <- (sum(accuracy$gold_accuracy, na.rm = TRUE) +
                    sum(accuracy$silver_accuracy, na.rm = TRUE) +
                    sum(accuracy$bronze_accuracy, na.rm = TRUE) +
                    sum(accuracy$total_accuracy, na.rm = TRUE)) / n_imputed_values

# cas de la méthode k-nn avec prise en compte de la population
accuracy <- data.mar %>%
  mutate(gold_accuracy = ifelse(is.na(gold), abs(original_data$gold - knnImpute.pop$gold) / pmax(original_data$gold, 1), NA),
         silver_accuracy = ifelse(is.na(silver), abs(original_data$silver - knnImpute.pop$silver) / pmax(original_data$silver, 1), NA),
         bronze_accuracy = ifelse(is.na(bronze), abs(original_data$bronze - knnImpute.pop$bronze) / pmax(original_data$bronze, 1), NA),
         total_accuracy = ifelse(is.na(total), abs(original_data$total - knnImpute.pop$total) / pmax(original_data$total,1), NA))

# Ajustement des valeurs de précision
accuracy <- accuracy %>%
  mutate(gold_accuracy = ifelse(gold_accuracy >= 1, 0, 1 - gold_accuracy),
         silver_accuracy = ifelse(silver_accuracy >= 1, 0, 1 - silver_accuracy),
         bronze_accuracy = ifelse(bronze_accuracy >= 1, 0, 1 - bronze_accuracy),
         total_accuracy = ifelse(total_accuracy >= 1, 0, 1 - total_accuracy))

# Calcul de la précision moyenne
mean_accuracy.2nn.pop <- (sum(accuracy$gold_accuracy, na.rm = TRUE) +
                    sum(accuracy$silver_accuracy, na.rm = TRUE) +
                    sum(accuracy$bronze_accuracy, na.rm = TRUE) +
                    sum(accuracy$total_accuracy, na.rm = TRUE)) / n_imputed_values

# cas de la méthode MICE
accuracy <- data.mar %>%
  mutate(gold_accuracy = ifelse(is.na(gold), abs(original_data$gold - miceImpute$gold) / pmax(original_data$gold, 1), NA),
         silver_accuracy = ifelse(is.na(silver), abs(original_data$silver - miceImpute$silver) / pmax(original_data$silver, 1), NA),
         bronze_accuracy = ifelse(is.na(bronze), abs(original_data$bronze - miceImpute$bronze) / pmax(original_data$bronze, 1), NA),
         total_accuracy = ifelse(is.na(total), abs(original_data$total - miceImpute$total) / pmax(original_data$total,1), NA))

# Ajustement des valeurs de précision
accuracy <- accuracy %>%
  mutate(gold_accuracy = ifelse(gold_accuracy >= 1, 0, 1 - gold_accuracy),
         silver_accuracy = ifelse(silver_accuracy >= 1, 0, 1 - silver_accuracy),
         bronze_accuracy = ifelse(bronze_accuracy >= 1, 0, 1 - bronze_accuracy),
         total_accuracy = ifelse(total_accuracy >= 1, 0, 1 - total_accuracy))

# Calcul de la précision moyenne
mean_accuracy.mice <- (sum(accuracy$gold_accuracy, na.rm = TRUE) +
                    sum(accuracy$silver_accuracy, na.rm = TRUE) +
                    sum(accuracy$bronze_accuracy, na.rm = TRUE) +
                    sum(accuracy$total_accuracy, na.rm = TRUE)) / n_imputed_values

# cas de la méthode MissForest
accuracy <- data.mar %>%
  mutate(gold_accuracy = ifelse(is.na(gold), abs(original_data$gold - missForestImpute$gold) / pmax(original_data$gold, 1), NA),
         silver_accuracy = ifelse(is.na(silver), abs(original_data$silver - missForestImpute$silver) / pmax(original_data$silver, 1), NA),
         bronze_accuracy = ifelse(is.na(bronze), abs(original_data$bronze - missForestImpute$bronze) / pmax(original_data$bronze, 1), NA),
         total_accuracy = ifelse(is.na(total), abs(original_data$total - missForestImpute$total) / pmax(original_data$total,1), NA))

# Ajustement des valeurs de précision
accuracy <- accuracy %>%
  mutate(gold_accuracy = ifelse(gold_accuracy >= 1, 0, 1 - gold_accuracy),
         silver_accuracy = ifelse(silver_accuracy >= 1, 0, 1 - silver_accuracy),
         bronze_accuracy = ifelse(bronze_accuracy >= 1, 0, 1 - bronze_accuracy),
         total_accuracy = ifelse(total_accuracy >= 1, 0, 1 - total_accuracy))

# Calcul de la précision moyenne
mean_accuracy.mf <- (sum(accuracy$gold_accuracy, na.rm = TRUE) +
                    sum(accuracy$silver_accuracy, na.rm = TRUE) +
                    sum(accuracy$bronze_accuracy, na.rm = TRUE) +
                    sum(accuracy$total_accuracy, na.rm = TRUE)) / n_imputed_values

# Comparaison des méthodes d'imputation---------------------------------------

n.miss <- 0
n.miss.knn.rep <- 0
n.miss.knn.pop.rep <- 0
n.miss.mice.rep <- 0
n.miss.mf.rep <- 0
# liste des précisions
prop.knn.list <- c()
prop.knn.pop.list <- c()
prop.mice.list <- c()
prop.mf.list <- c()

for (i in 1:25) {
  # Création des jeux de données avec des valeurs manquantes
  data.mar.rep <- data %>% mutate(gold = ifelse(runif(nrow(data)) < 0.1, NA, gold),
                              silver = ifelse(runif(nrow(data)) < 0.1, NA, silver),
                              bronze = ifelse(runif(nrow(data)) < 0.1, NA, bronze))
  data.mar.rep$total <- ifelse(is.na(data.mar.rep$gold) | is.na(data.mar.rep$silver) | is.na(data.mar.rep$bronze), NA, data.mar.rep$total)

  # nombre de valeurs à imputer
  n_imputed_values.rep <- sum(is.na(data.mar.rep$gold)) + sum(is.na(data.mar.rep$silver)) + sum(is.na(data.mar.rep$bronze)) + sum(is.na(data.mar.rep$total))

  # Imputation par k-nn avec distance euclidienne
  knnImpute.rep <- kNN(data = data.mar.rep, 
                          variable = c("gold", "silver", "bronze"), 
                          k = 2, 
                          metric = "euclidean", 
                          dist_var = c("gold", "silver", "bronze", "total"), 
                          weightDist = TRUE, 
                          numFun = weightedMeanInt, 
                          useImputedDist = FALSE)
  knnImpute.rep$total <- rowSums(knnImpute.rep[,c("gold","silver","bronze")])

  # Imputation par k-nn avec prise en compte de la population
  knnImpute.pop.rep <- kNN(data = data.mar.rep, 
                             variable = c("gold","silver","bronze"), 
                             k=2, 
                             metric = "euclidean", 
                             dist_var = c("gold","silver","bronze","total","population"),
                             weightDist = TRUE,
                             numFun = weightedMeanInt,
                             useImputedDist = FALSE)
  knnImpute.pop.rep$total <- rowSums(knnImpute.pop.rep[,c("gold","silver","bronze")])

  # Imputation par MICE
  impMICE.rep <- mice(data = data.mar.rep, m = 5, maxit = 50, method = "pmm", printFlag = FALSE)
  miceImpute.rep <- complete(impMICE.rep)
  miceImpute.rep$total <- rowSums(miceImpute.rep[,c("gold","silver","bronze")])

  # Imputation par MissForest
  
  # copie de data.mar.rep
  data.mar.missForest.rep <- data.mar.rep
  
  # suppression des colonnes inutiles
  data.mar.missForest.rep <- data.mar.missForest.rep[,-c(1,2)]
  
  data.mar.missForest.rep <- data.mar.missForest.rep %>% mutate(across(where(is.character),as.numeric))
  
  # imputation par missForest
  missForestImpute.rep <- missForest(data.mar.missForest.rep)
  missForestImpute.rep <- missForestImpute.rep$ximp
  
  # ajout des colonnes manquantes
  missForestImpute.rep <- cbind(data.mar[1:2], missForestImpute.rep)
  
  # Arrondissement des valeurs aux entiers
  missForestImpute.rep <- missForestImpute.rep %>% mutate(across(where(is.numeric), round))
  missForestImpute.rep$total <- rowSums(missForestImpute.rep[,c("gold","silver","bronze")])
  
  # Calcul de la précision pour chaque valeur NA
  original_data <- data
  accuracy.knn <- data.mar.rep %>%
    mutate(gold_accuracy = ifelse(is.na(gold), abs(original_data$gold - knnImpute.rep$gold) / pmax(original_data$gold, 1), NA),
           silver_accuracy = ifelse(is.na(silver), abs(original_data$silver - knnImpute.rep$silver) / pmax(original_data$silver, 1), NA),
           bronze_accuracy = ifelse(is.na(bronze), abs(original_data$bronze - knnImpute.rep$bronze) / pmax(original_data$bronze, 1), NA),
           total_accuracy = ifelse(is.na(total), abs(original_data$total - knnImpute.rep$total) / pmax(original_data$total,1), NA))

  accuracy.knn <- accuracy.knn %>%
    mutate(gold_accuracy = ifelse(gold_accuracy >= 1, 0, 1 - gold_accuracy),
           silver_accuracy = ifelse(silver_accuracy >= 1, 0, 1 - silver_accuracy),
           bronze_accuracy = ifelse(bronze_accuracy >= 1, 0, 1 - bronze_accuracy),
           total_accuracy = ifelse(total_accuracy >= 1, 0, 1 - total_accuracy))
  
  original_data <- data
  accuracy.knn.pop <- data.mar.rep %>%
    mutate(gold_accuracy = ifelse(is.na(gold), abs(original_data$gold - knnImpute.pop.rep$gold) / pmax(original_data$gold, 1), NA),
           silver_accuracy = ifelse(is.na(silver), abs(original_data$silver - knnImpute.pop.rep$silver) / pmax(original_data$silver, 1), NA),
           bronze_accuracy = ifelse(is.na(bronze), abs(original_data$bronze - knnImpute.pop.rep$bronze) / pmax(original_data$bronze, 1), NA),
           total_accuracy = ifelse(is.na(total), abs(original_data$total - knnImpute.pop.rep$total) / pmax(original_data$total,1), NA))

  accuracy.knn.pop <- accuracy.knn.pop %>%
    mutate(gold_accuracy = ifelse(gold_accuracy >= 1, 0, 1 - gold_accuracy),
           silver_accuracy = ifelse(silver_accuracy >= 1, 0, 1 - silver_accuracy),
           bronze_accuracy = ifelse(bronze_accuracy >= 1, 0, 1 - bronze_accuracy),
           total_accuracy = ifelse(total_accuracy >= 1, 0, 1 - total_accuracy))
  
  original_data <- data
  accuracy.mice <- data.mar.rep %>%
    mutate(gold_accuracy = ifelse(is.na(gold), abs(original_data$gold - miceImpute.rep$gold) / pmax(original_data$gold, 1), NA),
           silver_accuracy = ifelse(is.na(silver), abs(original_data$silver - miceImpute.rep$silver) / pmax(original_data$silver, 1), NA),
           bronze_accuracy = ifelse(is.na(bronze), abs(original_data$bronze - miceImpute.rep$bronze) / pmax(original_data$bronze, 1), NA),
           total_accuracy = ifelse(is.na(total), abs(original_data$total - miceImpute.rep$total) / pmax(original_data$total,1), NA))

  accuracy.mice <- accuracy.mice %>%
    mutate(gold_accuracy = ifelse(gold_accuracy >= 1, 0, 1 - gold_accuracy),
           silver_accuracy = ifelse(silver_accuracy >= 1, 0, 1 - silver_accuracy),
           bronze_accuracy = ifelse(bronze_accuracy >= 1, 0, 1 - bronze_accuracy),
           total_accuracy = ifelse(total_accuracy >= 1, 0, 1 - total_accuracy))
  
  original_data <- data
  accuracy.mf <- data.mar.rep %>%
    mutate(gold_accuracy = ifelse(is.na(gold), abs(original_data$gold - missForestImpute.rep$gold) / pmax(original_data$gold, 1), NA),
           silver_accuracy = ifelse(is.na(silver), abs(original_data$silver - missForestImpute.rep$silver) / pmax(original_data$silver, 1), NA),
           bronze_accuracy = ifelse(is.na(bronze), abs(original_data$bronze - missForestImpute.rep$bronze) / pmax(original_data$bronze, 1), NA),
           total_accuracy = ifelse(is.na(total), abs(original_data$total - missForestImpute.rep$total) / pmax(original_data$total,1), NA))

  accuracy.mf <- accuracy.mf %>%
    mutate(gold_accuracy = ifelse(gold_accuracy >= 1, 0, 1 - gold_accuracy),
           silver_accuracy = ifelse(silver_accuracy >= 1, 0, 1 - silver_accuracy),
           bronze_accuracy = ifelse(bronze_accuracy >= 1, 0, 1 - bronze_accuracy),
           total_accuracy = ifelse(total_accuracy >= 1, 0, 1 - total_accuracy))

  # Ajustement des valeurs de précision aux listes correspondantes

  prop.knn.list <- c(prop.knn.list, (sum(accuracy.knn$gold_accuracy, na.rm = TRUE) +
                    sum(accuracy.knn$silver_accuracy, na.rm = TRUE) +
                    sum(accuracy.knn$bronze_accuracy, na.rm = TRUE) +
                    sum(accuracy.knn$total_accuracy, na.rm = TRUE)) / n_imputed_values.rep)
  prop.knn.pop.list <- c(prop.knn.pop.list, (sum(accuracy.knn.pop$gold_accuracy, na.rm = TRUE) +
                    sum(accuracy.knn.pop$silver_accuracy, na.rm = TRUE) +
                    sum(accuracy.knn.pop$bronze_accuracy, na.rm = TRUE) +
                    sum(accuracy.knn.pop$total_accuracy, na.rm = TRUE)) / n_imputed_values.rep)
  prop.mice.list <- c(prop.mice.list, (sum(accuracy.mice$gold_accuracy, na.rm = TRUE) +
                    sum(accuracy.mice$silver_accuracy, na.rm = TRUE) +
                    sum(accuracy.mice$bronze_accuracy, na.rm = TRUE) +
                    sum(accuracy.mice$total_accuracy, na.rm = TRUE)) / n_imputed_values.rep)
  prop.mf.list <- c(prop.mf.list, (sum(accuracy.mf$gold_accuracy, na.rm = TRUE) +
                    sum(accuracy.mf$silver_accuracy, na.rm = TRUE) +
                    sum(accuracy.mf$bronze_accuracy, na.rm = TRUE) +
                    sum(accuracy.mf$total_accuracy, na.rm = TRUE)) / n_imputed_values.rep)
                                
  # Incrémentation du nombre de valeurs mal prédites pour chaque méthode 
  n.miss <- n.miss + n_imputed_values
  n.miss.knn.rep <- n.miss.knn.rep + sum(data$total != knnImpute.rep$total) + sum(data$gold != knnImpute.rep$gold) + sum(data$silver != knnImpute.rep$silver) + sum(data$bronze != knnImpute.rep$bronze)
  n.miss.knn.pop.rep <- n.miss.knn.pop.rep + sum(data$total != knnImpute.pop.rep$total) + sum(data$gold != knnImpute.pop.rep$gold) + sum(data$silver != knnImpute.pop.rep$silver) + sum(data$bronze != knnImpute.pop.rep$bronze)
  n.miss.mice.rep <- n.miss.mice.rep + sum(data$total != miceImpute.rep$total) + sum(data$gold != miceImpute.rep$gold) + sum(data$silver != miceImpute.rep$silver) + sum(data$bronze != miceImpute.rep$bronze)
  n.miss.mf.rep <- n.miss.mf.rep + sum(data$total != missForestImpute.rep$total) + sum(data$gold != missForestImpute.rep$gold) + sum(data$silver != missForestImpute.rep$silver) + sum(data$bronze != missForestImpute.rep$bronze)
}

# Calcul de la précision moyenne pour chaque méthode
prop.knn.rep <- 1 - n.miss.knn.rep / n.miss
prop.knn.pop.rep <- 1 - n.miss.knn.pop.rep / n.miss
prop.mice.rep <- 1 - n.miss.mice.rep / n.miss
prop.mf.rep <- 1 - n.miss.mf.rep / n.miss

# Moyenne des précisions moyennes
mean.prop.knn <- mean(prop.knn.list)
mean.prop.knn.pop <- mean(prop.knn.pop.list)
mean.prop.mice <- mean(prop.mice.list)
mean.prop.mf <- mean(prop.mf.list)

# courbe d'évolution de la précision moyenne en fonction du nombre de répétitions
plot(prop.knn.list, type = "s", col = "blue", xlab = "Répétitions", ylab = "Précision moyenne", main = "Evolution de la précision moyenne de l'imputation au cours de 25 tests", ylim = c(0.2,0.65))
lines(prop.knn.pop.list, type = "s", col = "red")
lines(prop.mice.list, type = "s", col = "darkgreen")
lines(prop.mf.list, type = "s", col = "purple")
legend("bottomright", legend = c("knn", "knn avec population", "MICE", "MissForest"), col = c("blue", "red", "darkgreen", "purple"), lty = 1, cex = 0.5)

par(mfrow = c(2,2))
plot(prop.knn.list, type = "s", col = "blue", xlab = "Répétitions", ylab = "Précision moyenne", main = "Précision moyenne de l'imputation par knn", ylim = c(0.25,0.7))
abline(h = mean.prop.knn, col = "black")
plot(prop.knn.pop.list, type = "s", col = "red", xlab = "Répétitions", ylab = "Précision moyenne", main = "Précision moyenne de l'imputation par knn avec population", ylim = c(0.25,0.7))
abline(h = mean.prop.knn.pop, col = "black")
plot(prop.mice.list, type = "s", col = "darkgreen", xlab = "Répétitions", ylab = "Précision moyenne", main = "Précision moyenne de l'imputation par MICE", ylim = c(0.25,0.7))
abline(h = mean.prop.mice, col = "black")
plot(prop.mf.list, type = "s", col = "purple", xlab = "Répétitions", ylab = "Précision moyenne", main = "Précision moyenne de l'imputation par MissForest", ylim = c(0.25,0.7))
abline(h = mean.prop.mf, col = "black")

