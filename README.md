<div align="center">
  <h1>Imputation de Données Manquantes</h1>
</div>

L’analyse de données est souvent confrontée à des **valeurs manquantes**, qui peuvent biaiser les résultats statistiques et nuire à la fiabilité des modèles.  
Ce projet explore la nature des données manquantes ainsi que différentes méthodes (classiques et avancées) pour traiter ces données incomplètes.

## Méthodes étudiées 
1. **Approches classiques :**  
   - Suppression des observations incomplètes.
   - Imputation par la moyenne/médiane/mode.

2. **Méthodes avancées :**  
   - **k-NN** : Estimation par les k-plus proches voisins.  
   - **MICE** : Imputation multiple par équations chaînées.
   - **MissForest** : Imputation via forêts aléatoires.

Le projet inclut une **analyse approfondie** et des **applications concrètes** sur des données sportives et météorologiques.  

---

## Contenu du Repository  
**Documents :**  
- [📄 Rapport - Imputation de Données Manquantes](./Rapport_Imputation_Donnees_Manquantes.pdf)  
- [📽️ Slides - Présentation avec des graphiques](./Slides_Imputation_Donnees_Manquantes.pdf)  

**Code source :**  
- Répertoire [`/src`](/src)  : Implémentation en **R** de différentes méthodes d’imputation sur les applications.  

**Données :**  
- Répertoire [`/data`](/data) : Contient les fichiers **CSV** utilisés pour les applications.  

---

## Quelques références
- Little, R. & Rubin, D. *Statistical Analysis with Missing Data*. Wiley.  
- Stekhoven, D. & Bühlmann, P. (2012). *MissForest—non-parametric missing value imputation for mixed-type data*. Bioinformatics.
- Imbert, A., Vialaneix, N. (2018). Décrire, prendre en compte, imputer et évaluer les valeurs manquantes dans les études statistiques, Journal de la Société Française de Statistique.
