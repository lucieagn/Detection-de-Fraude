setwd("~/Desktop/M1 IM/Semestre 1/Analyse de Données/projet")

install.packages("cluster")   # méthodes pour le clustering.
install.packages("ggplot2")
install.packages("fpc")      # méthodes pour le clustering hiérarchique.
install.packages("dbscan")   #méthodes pour le clustering basé sur la densité
install.packages("readr")
install.packages("rpart")
install.packages("C50")
install.packages("tree")
install.packages("rpart.plot")
install.packages("ROCR")
install.packages("regclass")
install.packages("kableExtra")
install.packages("smotefamily") #Rééquilibrage des classes
install.packages("CORElearn")
install.packages("DescTools")

# Activation des librairies

library(readr)
library(cluster)
library(dbscan)
library(ggplot2)
library(fpc)
library(rpart)
library(C50)
library(tree)
library(rpart.plot)
library(ROCR)
library(regclass)
library(kableExtra)
library(smotefamily)
library(CORElearn)
library(DescTools)


#---------------------------#
# PRÉTRAITEMENT DES DONNÉES #
#---------------------------#

#Chargement des données du fichier Data_Projet_1 dans la dataframe FRAUDULENT 
FRAUDULENT <- read.csv("Data_projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
View(FRAUDULENT)

FRAUDULENT_Pr <- read.csv("Data_projet_1_New.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
View(FRAUDULENT_Pr)
str(FRAUDULENT_Pr) #Vérification du chargement de données

#Compréhension des variables
str(FRAUDULENT) #cette dataframe possède 1100 instances
table(FRAUDULENT$fraudulent)# Apercu de l'effectif fraudulent=Oui et fraudulent=Non
qplot(fraudulent,data=FRAUDULENT, fill=fraudulent,binwidth=5) #histogramme, on remarque un déséquilibre de classe

#RÉÉQUILIBRAGE DE LA CLASSE FRAUDULENT
FRAUDULENT$fraudulent <- as.factor(FRAUDULENT$fraudulent) #afin que la fct SMOTE s'applique correctement
FRAUDULENT$gender <- as.numeric(FRAUDULENT$gender)
FRAUDULENT$incident_cause <- as.numeric(FRAUDULENT$incident_cause)
FRAUDULENT$claim_area <- as.numeric(FRAUDULENT$claim_area)
FRAUDULENT$police_report <- as.numeric(FRAUDULENT$police_report)
FRAUDULENT$claim_type <- as.numeric(FRAUDULENT$claim_type)
FRAUDULENT$fraudulent <- as.numeric(FRAUDULENT$fraudulent)
# Appliquez la fonction ovun.sample pour le suréchantillonnage
Fraudulent <- SMOTE(FRAUDULENT,as.numeric(FRAUDULENT$fraudulent),K=5,dup_size =0)
Fraudulent <- Fraudulent$data
Fraudulent <- Fraudulent[,-13] #on enlève la variable "class" (qui parait totalement identique à fraudulent)
str(Fraudulent)
table(Fraudulent$fraudulent) #Non 846, Oui 762
#on reconvertit les variables en variable factorielle
   ## Modifier les valeurs numériques en prenant la partie entière
Fraudulent$gender <- floor(Fraudulent$gender)
Fraudulent$incident_cause <- floor(Fraudulent$incident_cause)
Fraudulent$claim_area <- floor(Fraudulent$claim_area)
Fraudulent$police_report <- floor(Fraudulent$police_report)
Fraudulent$claim_type <- floor(Fraudulent$claim_type)
Fraudulent$fraudulent <- floor(Fraudulent$fraudulent)

Fraudulent$gender <- factor(Fraudulent$gender,levels=c(1,2),labels=c("Female","Male"))
Fraudulent$incident_cause <- factor(Fraudulent$incident_cause,levels=c(1,2,3,4,5),labels=c("Crime","Driver error","Natural causes","Other causes","Other driver error"))
Fraudulent$claim_area <- factor(Fraudulent$claim_area,levels=c(1,2),labels=c("Auto","Home"))
Fraudulent$police_report <- factor(Fraudulent$police_report,levels=c(1,2,3),labels=c("No","Unknown","Yes"))
Fraudulent$claim_type <- factor(Fraudulent$claim_type,levels=c(1,2,3),labels=c("Injury only","Material and injury","Material only"))
Fraudulent$fraudulent <- factor(Fraudulent$fraudulent,levels = c(1,2),labels = c("No","Yes"))

#Selection et analyse qualitative des différentes variables
#variables continues : age, days_to_incident, claim_amount, total_policy_claims
qplot(age,data=Fraudulent, fill=fraudulent,binwidth=5)
qplot(age,fraudulent,data=Fraudulent, color=fraudulent) + geom_jitter(width = 0.3, height = 0.3)
boxplot(age~fraudulent,data=Fraudulent,col=c("tomato","darkturquoise")) #comparaison de deux méthodes de graphe
#pour la variable age, le boxplot semble etre le plus judicieux 

qplot(days_to_incident, data = Fraudulent, fill=fraudulent,bins=13)

boxplot(claim_amount~fraudulent,data=Fraudulent,col=c("tomato","darkturquoise"))
qplot(total_policy_claims,data=Fraudulent, fill=fraudulent,binwidth=5)

#pour les variables discrètes
qplot(gender, data=Fraudulent, fill=fraudulent) 
qplot(gender,fraudulent,data=Fraudulent,fill=gender)+ geom_jitter(width = 0.2, height = 0.2)  # Nuages de points
table(Fraudulent$gender, Fraudulent$fraudulent)
prop.table(table(Fraudulent$gender, Fraudulent$fraudulent, dnn=c("Gender","Fraudulent")))

qplot(incident_cause, data = Fraudulent, fill=fraudulent)
qplot(claim_area, data = Fraudulent, fill=fraudulent)
qplot(police_report, data = Fraudulent, fill=fraudulent)
qplot(claim_type, data = Fraudulent, fill=fraudulent)

#---------------------------------#
# Test de dépendance et p-valeurs #
#---------------------------------#

#test du chi2 pour les variables discrètes 

chisq.test(Fraudulent$gender,Fraudulent$fraudulent)
#la p-value est 1.325e-14 < 0.05 donc il y a dépendance entre ces 2 variables
chisq.test(Fraudulent$incident_cause,Fraudulent$fraudulent)
#la p-value est 1.042e-09 < 0.05 donc il y a dépendance entre ces 2 variables
chisq.test(Fraudulent$claim_area,Fraudulent$fraudulent)
#la p-value est 1.358e-07 < 0.05 donc il y a dépendance entre ces 2 variables
chisq.test(Fraudulent$police_report,Fraudulent$fraudulent)
#la p-value est 2.2e-16 < 0.05 donc il y a dépendance entre ces 2 variables
chisq.test(Fraudulent$claim_type,Fraudulent$fraudulent)
#la p-value est 2.093e-13 < 0.05 donc il y a dépendance entre ces 2 variables

#conclusion : toutes es variables discrètes dépendent fraudulent pour la mesure χ2 

fisher.test(Fraudulent$gender,Fraudulent$fraudulent,simulate.p.value=TRUE)
#la p-value est 8.686e-15 < 0.05 donc il y a une dépendance entre ces 2 variables
fisher.test(Fraudulent$incident_cause,Fraudulent$fraudulent,simulate.p.value=TRUE)
#la p-value est 0.0004998 < 0.05 donc il y a une dépendance entre ces 2 variables
fisher.test(Fraudulent$claim_area,Fraudulent$fraudulent,simulate.p.value=TRUE)
#la p-value est 4.447e-08 < 0.05 donc il y a une dépendance entre ces 2 variables
fisher.test(Fraudulent$police_report,Fraudulent$fraudulent,simulate.p.value=TRUE)
#la p-value est 0.0004998 < 0.05 donc il y a dépendance entre ces 2 variables
fisher.test(Fraudulent$claim_type,Fraudulent$fraudulent,simulate.p.value=TRUE)
#la p-value est 0.0004998 < 0.05 donc il y a une dépendance entre ces 2 variables


#ce qui confirme bien le test du chi2 pour les variables discrètes

#On fait maintenant des test de corrélation pour nos variables continues 

#Si la valeur obtenue est dans l'intervalle [-0.5,0.0] donc la corrélation est négative et faible 
# et si la valeur obtenue est dans l'intervalle [0.0,0.5] donc la corrélation est positive et faible 

cor.test(Fraudulent$age,as.numeric(Fraudulent$fraudulent))
#la p-value est 5.79e-10 < 0.5 avec la corrélation est négative et faible 
cor.test(Fraudulent$days_to_incident,as.numeric(Fraudulent$fraudulent))
#la p-value est 1.997e-10 < 0.5 avec la corrélation est négative et faible 
cor.test(Fraudulent$claim_amount,as.numeric(Fraudulent$fraudulent))
#la p-value est 0.113 < 0.5 avec la corrélation est positive et faible 
cor.test(Fraudulent$total_policy_claim,as.numeric(Fraudulent$fraudulent))
#la p-value est 3.146e-05 < 0.5 avec la corrélation est positive et faible 

#évaluation de l'entropie
Entropy(table(Fraudulent$fraudulent)) # 0.9980306 (assez proche de 0)
#Plus la valeur obtenue est élevée, plus l’apprentissage d’un modèle de prédiction sera difficile
attrEval(fraudulent~., Fraudulent, estimator = "InfGain")
#les variables dont l’Information Gain est supérieur à 0.01 qui sont les variables prédictives
#Variables inutiles : claim_id et customer_id
#identifiées comme les plus utiles sont : toutes ! (police_report,gender,total_policy_claims) -(claim_area,claim_type,age)
attrEval(fraudulent~., Fraudulent, estimator = "GainRatio") #pareil (!claim_amount,days_to_incident,age) -(incident_cause,claim_type,police_report)
attrEval(fraudulent~., Fraudulent, estimator = "Relief") #+(days_to_incident,age,police_report) -(claim_type,gender,total_policy_claims)
#les mesures sont tres hétérogènes, la seule variable qui semble rester assez utile pour les 3mesures reste claim_amount 
#et la variable la moins utile serait claim_type

#-------------------------------------------------#
# CLUSTERING DES DONNÉES avec toutes les variable #
#-------------------------------------------------#

pairs(Fraudulent) #resultats tres tres brouillon
#On commence notre clustering
#ensemble d'évaluation pour le cluster :
Fraudulent_cluster <- Fraudulent[,-12]
Fraudulent_cluster <- Fraudulent_cluster[,-1]
Fraudulent_cluster <- Fraudulent_cluster[,-1]

# Calcul de la matrice de distance par la fonction daisy() pour variables heterogenes
dmatrix <- daisy(Fraudulent_cluster)

# Resume de la matrice
summary(dmatrix)

#     Clustering par partitionnement (algorithme des K-means)         #
km4 <- kmeans(dmatrix, 8)
Fraudulent_cluster_km4 <- data.frame(Fraudulent_cluster, km4$cluster)
table(km4$cluster, Fraudulent$fraudulent)
qplot(km4$cluster, data=Fraudulent_cluster_km4, fill=Fraudulent$fraudulent) #cluster purs (classes unique)

for (k in 3:10) {
  km <- kmeans(dmatrix, k)
  proportions <- table(km$cluster, Fraudulent$fraudulent)
  print(qplot(km$cluster, data=Fraudulent_cluster,geom = "bar", fill=Fraudulent$fraudulent, main=paste("kmeans avec k = ",k))+theme_minimal())
}
#meilleur k=4

#       Clustering hierarchique par agglomeration                     #

# Clustering hierarchique par fonction agnes()
agn <- agnes(dmatrix)

# Boucle for() faisant le nombre de clusters
for (k in 3:8){
  agnK <- cutree(agn, k)
  print(table(agnK, Fraudulent$fraudulent))
  print(qplot(agnK, data=Fraudulent_cluster, geom = "bar", fill=Fraudulent$fraudulent, main=paste("agnK avec k = ",k))+theme_minimal())
}  

#On se rend compte que quel que soit le nombre de points considere entre 3 et 8,les clusters obtenus sont tres heterogenes. L'utilisation de cet algorithme n'est donc pas efficace.

#          Clustering hierarchique par division               #

# Clustering hiérarchique par fonction diana()
dia <- diana(dmatrix)

# Boule for() faisant le nombre de clusters
for (k in 3:9){
  #plot(dia, which.plots = 2)     # Affichage du dendrogramme
  #rect.hclust(dia, k, border="red")     # Affichage des 4 clusters dans le dendrogramme
  diaK <- cutree(dia, k)
  print(qplot(diaK, data=Fraudulent_cluster, geom = "bar", fill=Fraudulent$fraudulent, main=paste("diaK avec k = ",k))+theme_minimal())
}  
#k
#On se rend compte que quel que soit le nombre de points considere entre 3 et 9,les clusters obtenus sont tres heterogenes. L'utilisation de cet algorithme n'est donc pas efficace.


#            CLUSTERING BASE SUR LA DENSITE                 #

# Clustering par fonction hbdscan()
hdb <- hdbscan(dmatrix, minPts = 3)

# Boule for() faisant le nombre minimal de voisins
for (m in 3:8){
  hdb <- hdbscan(dmatrix, minPts = m)
  print(table(hdb$cluster, Fraudulent$fraudulent))
  print(qplot(as.factor(hdb$cluster), data=Fraudulent_cluster, geom = "bar", fill=Fraudulent$fraudulent, main=paste("hdbscan avec k = ",m))+theme_minimal())
}  


#aucun algo semble convenir du faite du grand nombre de variable donc a va mesurer l'utilite predictive de chaque variable


#par exemple pour la methode par partionnement avec kmeans = 4.
dmatrix <- daisy(Fraudulent_cluster)
km_meilleur <- kmeans(dmatrix, 4)
data_km_meilleur <- data.frame(Fraudulent_cluster,km_meilleur$cluster)
print(qplot(km_meilleur$cluster, data=Fraudulent_cluster, fill=Fraudulent$fraudulent))#on affiche les meilleures cluster obtenus

#on peut analyser la composition de ces clusters selon les variables :

qplot(age,as.factor(km_meilleur$cluster),data=data_km_meilleur,color=Fraudulent$fraudulent)+ geom_jitter(width = 0.2, height = 0.2)
qplot(days_to_incident,as.factor(km_meilleur$cluster),data=data_km_meilleur,color=Fraudulent$fraudulent)+ geom_jitter(width = 0.2, height = 0.2)
qplot(police_report,as.factor(km_meilleur$cluster),data=data_km_meilleur,color=Fraudulent$fraudulent)+ geom_jitter(width = 0.2, height = 0.2)
qplot(claim_amount,as.factor(km_meilleur$cluster),data=data_km_meilleur,color=Fraudulent$fraudulent)+ geom_jitter(width = 0.2, height = 0.2)
qplot(total_policy_claims,as.factor(km_meilleur$cluster),data=data_km_meilleur,color=Fraudulent$fraudulent)+ geom_jitter(width = 0.2, height = 0.2)
qplot(claim_type,as.factor(km_meilleur$cluster),data=data_km_meilleur,color=Fraudulent$fraudulent)+ geom_jitter(width = 0.2, height = 0.2)
qplot(incident_cause,as.factor(km_meilleur$cluster),data=data_km_meilleur,color=Fraudulent$fraudulent)+ geom_jitter(width = 0.2, height = 0.2)
qplot(claim_area,as.factor(km_meilleur$cluster),data=data_km_meilleur,color=Fraudulent$fraudulent)+ geom_jitter(width = 0.2, height = 0.2) #peutetre enlever
#la méthode ne donne aucun bon cluster pour cette base de données


# Création Ens Appr. et Ens Test #
#--------------------------------#

#par tirage aléatoire
n <- nrow(Fraudulent)
set.seed(1)#vecteurs d'indices tirés au hasard
idx <- sample(1:n,size=1000,replace=FALSE) #tirage sans remise (pas de répétition)/ pas de double possible
Fraudulent_EA <- Fraudulent[idx,] #Ensemble d'apprentissage
print(dim(Fraudulent_EA))
Fraudulent_EA <- Fraudulent_EA[,-1:-2]# On supprime les deux colonnes claim_id et customer_id dans l'ensemble d'apprentissage car elles ne contiennent pas des données
View(Fraudulent_EA)
Fraudulent_ET <- Fraudulent[-idx,]#Ensemble de Test
print(dim(Fraudulent_ET)) 

# prévisualisation des données 
summary(Fraudulent_EA)
summary(Fraudulent_ET)
#---------------------------#
# Classification Supervisée #
#---------------------------#


#--------------------------------------------#
#    APPRENTISSAGE DE L'ARBRE DE DECISION    #
#--------------------------------------------#

#         APPRENTISSAGE tree()              #

treetd10 <- tree(fraudulent ~ ., Fraudulent_EA,control =tree.control(nrow(Fraudulent_EA),mincut = 10),split = c("deviance"))
treetg10 <- tree(fraudulent ~ ., Fraudulent_EA,split = "gini",control =tree.control(nrow(Fraudulent_EA), mincut = 35))
tree <- tree(fraudulent~.,Fraudulent_EA)
#plot
plot(treetd10)
text(treetd10,pretty=0)

plot(treetg10)
text(treetg10,pretty=0) 

plot(tree)
text(tree,pretty=0)#meilleur

#on garde tree
#résultats
test_tree = predict(tree, Fraudulent_ET, type="class") 
table(test_tree)

# Ajout des predictions de 'tree1' comme  nouvelle colonne 'predict' dans 'reponse_ET'
Fraudulent_ET$predictT = test_tree

# CALCUL DES TAUX DE SUCCES 

mc_tree =table(Fraudulent_ET$fraudulent, test_tree)
taux_succes_tree=(mc_tree[1,1]+mc_tree[2,2])/nrow(Fraudulent_ET)
taux_succes_tree
mc_tree

#         APPRENTISSAGE rpart()              #
treerg10 <- rpart(fraudulent ~ ., Fraudulent_EA,parms = list(split="gini"), control =rpart.control(minbucket = 10))
treeri10 <- rpart(fraudulent ~ ., Fraudulent_EA,parms = list(split="information"), control =rpart.control(minbucket = 15))
treer <- rpart(fraudulent ~ ., subset(Fraudulent_EA))

prp(treerg10, type=4, extra=8, box.palette = "auto")#meilleur
prp(treeri10, type=4, extra=8, box.palette = "auto")
prp(treer, type=4, extra=8, box.palette = "auto")

#on choisit treerg10
test_treerg10 = predict(treerg10, Fraudulent_ET, type="class")

# Affichage des resultats de test
table(test_treerg10)
# Ajout des predictions de 'treerp1' comme  nouvelle colonne 'predictR' 
Fraudulent_ET$predictR = test_treerg10

# Calcul du taux de succes: nombre de succes sur nombre d'exemples de test

mc_treerg10 =table(Fraudulent_ET$fraudulent, test_treerg10)
taux_succes_treerg10=(mc_treerg10[1,1]+mc_treerg10[2,2])/nrow(Fraudulent_ET)
taux_succes_treerg10
mc_treerg10

#         APPRENTISSAGE C5.0()              #
treecT10 <- C5.0(fraudulent ~ ., Fraudulent_EA,control = C5.0Control(minCases = 10,noGlobalPruning = TRUE))
treecF10 <- C5.0(fraudulent ~ ., Fraudulent_EA,control = C5.0Control(minCases = 5,noGlobalPruning = FALSE))
treec <- C5.0(fraudulent ~ ., subset(Fraudulent_EA))

plot(treecT10)
plot(treecF10)
plot(treec)

#résultats
test_treecT10 = predict(treecT10, Fraudulent_ET, type="class") 
table(test_treecT10)

# Ajout des predictions de 'tree1' comme  nouvelle colonne 'predict' dans 'reponse_ET'
Fraudulent_ET$predictC = test_treecT10

# CALCUL DES TAUX DE SUCCES 

mc_treec =table(Fraudulent_ET$fraudulent, test_treecT10)
taux_succes_treec=(mc_treec[1,1]+mc_treec[2,2])/nrow(Fraudulent_ET)
taux_succes_treec
mc_treec

# arbre plus performant arbre treec <- C5.0

#------------------------------------------#
# CHOICE OF THE MOST PERFORMING CLASSIFIER #
#------------------------------------------#
#----------------------------------#
# CALCUL DES MATRICES DE CONFUSION #
#----------------------------------#

#Matrice de confusion de tree()
mct = table(Fraudulent_ET$fraudulent, test_tree)
print(mct)
R1 = mct[2,2]/(mct[2,2]+mct[2,1]) # Mesure du Rappel(Sensibilté)
S1 = mct[1,1]/(mct[1,1]+mct[1,2]) # Mesure de Spécifité
P1 = mct[2,2]/(mct[2,2]+mct[1,2]) # Mesure de Précision
TVN1 = mct[1,1]/(mct[1,1]+mct[2,1]) # Mesure du taux de vrais négatifs
print(c(R1,S1,P1,TVN1))

#Matrice de confusion de rpart()
mcr = table(Fraudulent_ET$fraudulent, test_treerg10)
print(mcr)
R2 = mcr[2,2]/(mcr[2,2]+mcr[2,1]) # Mesure du Rappel(Sensibilté)
S2 = mcr[1,1]/(mcr[1,1]+mcr[1,2]) # Mesure de Spécifité
P2 = mcr[2,2]/(mcr[2,2]+mcr[1,2]) # Mesure de Précision
TVN2 = mcr[1,1]/(mcr[1,1]+mcr[2,1]) # Mesure du taux de vrais négatifs
print(c(R2,S2,P2,TVN2))


#Matrice de confusion de C50()
mc3 = table(Fraudulent_ET$fraudulent, test_treecT10)
print(mc3)
R3 = mc3[2,2]/(mc3[2,2]+mc3[2,1]) # Mesure du Rappel(Sensibilté)
S3 = mc3[1,1]/(mc3[1,1]+mc3[1,2]) # Mesure de Spécifité
P3 = mc3[2,2]/(mc3[2,2]+mc3[1,2])# Mesure de Précision
TVN3 = mc3[1,1]/(mc3[1,1]+mc3[2,1]) # Mesure du taux de vrais négatifs
print(c(R3,S3,P3,TVN3))


###############
# Courbes ROC #
###############

#------------------------------#
# COURBE ROC DE L'ARBRE 'tree' #
#------------------------------#

prob_tree1 = predict(treetg10, Fraudulent_ET, type="vector")

roc1 = prediction(prob_tree1[,2], Fraudulent_ET$fraudulent)  # pour tree()
print(roc1)
roc_p1 = performance(roc1, "tpr","fpr")
plot(roc_p1, add=TRUE, col = "blue")

#-------------------------------#
# COURBE ROC DE L'ARBRE 'rpart' #
#-------------------------------#


# Generation des probabilites pour chaque exemple de test pour l'arbre 'treerp1'

prob_treerg10 = predict(treerg10, Fraudulent_ET, type="prob")

# Affichage des deux vecteurs de probabilites generes

print(prob_treerg10)

# Affichage du vecteur de probabilites de prediction 'Oui'

prob_treerg10[,2]

# Affichage du vecteur de probabilites de prediction 'Non'

prob_treerg10[,1]

# Construction d'un data frame contenant classe reelle, prediction et probabilités des predictions

df_result1 = data.frame(Fraudulent_ET$fraudulent, test_treerg10, prob_treerg10[,2], prob_treerg10[,1])
View(df_result1)

# Génération des données necessaires pour la courbe ROC
roc3 = prediction(prob_treerg10[,2], Fraudulent_ET$fraudulent)  # pour rpart()
print(roc1)

# Calcul des taux de vrais positifs (tpr) et taux de faux positifs (fpr)
roc_p3 = performance(roc1, "tpr","fpr") 
print(roc_p1)

# Tracage de la  courbe ROC
plot(roc_p3,add=TRUE, col = "green")

#-------------------------------#
# COURBE ROC DE L'ARBRE 'C5.0' #
#-------------------------------#

# Génération des probabilites de prediction sur l'ensemble de test
prob_treecT10 = predict(treecT10, Fraudulent_ET, type="prob")

roc2 = prediction(prob_treecT10[,2], Fraudulent_ET$fraudulent)  # pour C50()
print(roc2)
roc_p2 = performance(roc2, "tpr","fpr")
plot(roc_p2, add = TRUE, col = "magenta") 


#----------------------------------------#
# CALCUL DES INDICES AUC DES COURBES ROC #
#----------------------------------------#

# Calcul de l'AUC à partir des données générées : arbre 'tree()'
auc_tree1 = performance(roc1, "auc")
str(auc_tree1)  # Affichage de la structure de l'objet 'auc_tree1' généré
attr(auc_tree1, "y.values")  # Affichage de la valeur de l'AUC =0.7759159


# Calcul de l'AUC de l'arbre C5.0()
auc_tree2 = performance(roc2, "auc")  
attr(auc_tree2, "y.values")  # Affichage de la valeur de l'AUC = 0.774409

# Calcul de l'AUC de l'arbre rpart()
auc_tree3 = performance(roc3, "auc")
attr(auc_tree3, "y.values")  # Affichage de la valeur de l'AUC = 0.7350205

#le plus performant est tree
##########################################
# Prédictions de l'appétence des clients #
##########################################

# Application des arbres à l'ensemble fraudulent_pr

pred <- predict(treerg10, FRAUDULENT_Pr, type="class")
table(class_tree)
pred_prob <- predict(treerg10, FRAUDULENT_Pr, type = "prob")
table(pred_prob)

FRAUDULENT_Pr$Prediction <- pred


#Ajout des colones des prédictions
FRAUDULENT_Pr$Prediction = pred
FRAUDULENT_Pr$probability_NO=pred_prob[,1]
FRAUDULENT_Pr$probability_YES=pred_prob[,2]
View(FRAUDULENT_Pr)

# APPLYING THE CLASSIFIER TO DATA
#--------------------------------
# Enregistrement du fichier de resultats au format csv
write.table(FRAUDULENT_Pr, file='fraudulent_pr.csv', sep="\t", dec=".", row.names = F)


#Note :
# Utilisation de kable pour formater le dictionnaire de données en LaTeX
#library(kableExtra)
#latex_table <- kable(data_dict, format = "latex", booktabs = TRUE)
#latex_file <- "data_dict.tex"
#writeLines(latex_table, latex_file) 