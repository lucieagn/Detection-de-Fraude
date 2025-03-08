# Detection de Fraudes dans le Domaine de l'Assurance

## 📌 Description du Projet
Ce projet vise à construire un modèle de prédiction des déclarations frauduleuses des clients d’une société d’assurance en utilisant des techniques de Machine Learning et de Data Science.

Deux ensembles de données sont fournis :
- `Data_Projet_1.csv` : Contient 1100 déclarations d’accidents avec l’indication de fraude.
- `Data_Projet_1_New.csv` : Contient 200 nouvelles déclarations pour lesquelles la fraude doit être prédite.

L'objectif est de créer un modèle de classification capable d'identifier les déclarations frauduleuses et de minimiser les risques financiers associés.

---
## 📊 Données
- **Instances** : Chaque ligne représente une déclaration d'accident.
- **Variable cible** : `FRAUDULENT` (Yes/No)


---
## 🚀 Objectifs
1. **Exploration et visualisation des données** 📊
2. **Prétraitement des données** 🛠️
3. **Construction et évaluation de modèles de classification** 🤖
4. **Sélection du meilleur modèle selon des critères d’évaluation (matrice de confusion, précision, rappel, F1-score, etc.)** 🎯
5. **Application du modèle aux nouvelles déclarations à prédire** 🔍

---
## 🏗️ Méthodologie
1. **Exploration des données** : Analyse des tendances et des distributions des variables.
2. **Prétraitement** : Gestion des valeurs manquantes, encodage des variables catégorielles, normalisation des données.
3. **Sélection des caractéristiques** : Identifier les variables les plus pertinentes pour la prédiction.
4. **Construction de modèles de classification** : Entraînement de plusieurs modèles
5. **Évaluation et comparaison des modèles** : Sélection du modèle le plus performant.
6. **Prédiction sur les nouvelles données** : Appliquer le modèle final aux nouvelles déclarations.

---
## 📂 Fichiers du Projet
- `notebooks/` : Contient les notebooks Jupyter avec les analyses et modèles.
- `data/` : Contient les fichiers de données (`Data_Projet_1.csv` et `Data_Projet_1_New.csv`).
- `results/` : Contient les résultats finaux et les prédictions sur les nouvelles déclarations.
- `report.pdf` : Rapport détaillé du projet.


---
## 📈 Évaluation des Modèles
Nous utilisons plusieurs métriques pour évaluer la performance des modèles :
- **Matrice de confusion** 🟩
- **Précision, rappel, F1-score** 📊
- **Courbe ROC-AUC** 📉

---
## 📌 Résultats Attendus
- Un modèle performant permettant de détecter les fraudes avec une haute précision.
- Un rapport détaillant l'analyse des données, la construction des modèles et les résultats obtenus.
- Un fichier `.csv` contenant les prédictions des nouvelles déclarations.
