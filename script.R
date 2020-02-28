dataset = read.table("./kanada_data.txt", header = T)
dataset$label = as.factor(dataset$label)
competition = read.table("./competition_data.txt", header = T)

source("./fonctions_exam_tp.R")



# Partie préliminaire : description et analyse du jeu de données
dim(dataset)
unique(dataset$label)

# Séparation / validation / test des données de data
set.seed(20) 
nall = nrow(dataset) 
ntrain = floor(0.7 * nall) 
nvalid = floor(0.15 * nall) 
ntest = nall - ntrain - nvalid  
index = sample(nall) 

train = dataset[index[1:ntrain],] 
valid = dataset[index[(ntrain+1):(ntrain+nvalid)],]
test = dataset[index[(ntrain+nvalid+1):(ntrain+nvalid+ntest)],] 

#position du label
positionCible = 1

#nmbre de classe 
nbrClasse = 10

###########################################################
####  Première partie : utilisation des images brutes  ####
###########################################################


# 1) Arbres de décision
# Pour les arbres de décision, commencer par:
library(rpart)
source("./fonctions_exam_tp.R")
###############################################################################
#                           ARBRE DE DECISION                                 #  
##############################################################################



#on apprend le modele sur le train
modeleArbre = rpart(as.formula(paste(colnames(dataset)[positionCible],"~.",sep="")), data = train, control = list(minbucket = 1,cp = 0, minsplit = 1))

#laméthode niveauElagageErreurG retourne pour un niveau d'élagage déterminé l'erreur de validation
#Le niveau d'élagage est calculé à modeleArbre$cptable[,1]
#resultat, [2] niveau d'élagage, [3] erreur en validation
resultArbre= niveauElagageErreurG(modeleArbre,train,valid,test,positionCible,competition)



# 2) Support Vector Machine (SVM) 
library(e1071)

###############################################################
#     SVM
##############################################################

#La méthode GeneSVM retourne pour type de kernel spécifique la valeur du paramètre C choisie
#la valeur du gamma pour un kernel= radial et degré pour un kernel= poly et l'erreur en validation
#parametre c ==> je fais varier ce paramètre entre 0.001:100
#kernel = Poly ==> je fais varier le paramètre poly entre 2:5
#kernel = radial ==> je fais varier le paramètre gamma entre 0.01:5


#resultat, [2] parametre C, [3] parametre other, [4] erreur validation
resultSVM_radial = GeneSVM (train,valid,test,positionCible,"radial",competition)
resultSVM_polynomial = GeneSVM (train,valid,test,positionCible,"polynomial",competition)

svm_hard <- svm(label~.,data = train, kernel='radial',type='C-classification')
compet = predict(svm_hard,competition)
write.csv(data.frame("Id"=c(1:1000), "label"=compet), file ="./mespredictionsSVM_radial2.csv", row.names = F)



# Atention, des warnings vont s'afficher à l'écran lors de la création de modèles avec SVM.
# PAs d'affolement, ce ne sont que des warnings. Vous pouvez continuer sans problèmes.



# 3) K-plus-proches voisins (KNN)
library(class)

###############################################################
#     k-NN
##############################################################

#la méthode knnVal retourne pour une valeur de k déterminée et l'erreur en validation
#k variant entre 1:100
#resultat, [2] k, [3] erreur en validation
resultKnn = knnVal(train,valid,test,positionCible,competition)



# 4) Régression logistique

# Pour la regression logistique, vous aurez besoin des fonctions de nnets.R ( comme vu dans le TP sur la régression logistique
# pour les chiffres ). Attention, je vous conseille d'apprendre un modèle avec au maximum 150-200 images (sinon ce sera trop long)
# Et pas besoin de puissances ici car les pixels sont 0 ou 1

source("./nnets.R")
###############################################################
#     Régression logistique
##############################################################

#la méthode RelogLineaireMultiClass ou RelogLineaire2Class retourne une erreur en validation
#le modele de regression étant appris que sur le train

#resultat, [2] erreur en validation
modele = learn_nn(train[,c(1:ncol(train))[-positionCible]], train[,positionCible], 0,100, c(784, 10))
predV = predVal(modele,valid,positionCible)
errorV = sum(predV != valid[,positionCible])

predTest = predVal(modele,test,positionCible)
compet = predVal(modele,competition,1)
write.csv(data.frame("Id"=c(1:1000), "label"=compet), file ="./mespredictionsRegression.csv", row.names = F)

erreurG= sum(predTest != test[,positionCible])


resultRegMultiClass = data.frame("regLineMultiC","",errorV,erreurG)



# 5) Forêt aléatoire
library(randomForest)
###############################################################
#     Foret d'arbre aléatoires
##############################################################

#la méthode foretByMax retourne une erreur en validation et le nombre d'abres de la forêt 

#resultat,[2] nombre d'arbres,[3] erreur en validation
resultForet = foretByMax(train,valid,test,positionCible,competition)




# 6) Réseaux de neurones
library(keras)
library(tensorflow)
source("./fonctions_keras.R")

###############################################################
#     Réseaux de neurones
##############################################################

library(keras)
library(tensorflow)

library(reticulate)
os <- import("os")
os$environ['TF_CPP_MIN_LOG_LEVEL'] = '2'

#dataset1 = read.table("./segment.dat")

modelNN(train,competition)



#############################################################################
####  Deuxième partie : utilisation de la représentation HOG des images  ####
#############################################################################


## Vous allez maintenant utiliser la représentation HOG des images afin d'essayer d'améliorer votre score.
library(OpenImageR)
# Rappel : 
# La représentation HOG prend deux paramètres cells et orientation (que j'appelle cel et ori)
# Par exemple, on peut commencer avec
cel = 2
ori = 6
# Pour transformer une image (qui correspond à une ligne de votre jeu de données), il faut
idx = 1; # on transforme la première image
h = HOG(matrix(as.numeric(dataset[idx,2:785]), nrow = 28, byrow = T), cells = cel, orientations = ori)

# Pour transformer toutes les images de dataset:
hog_data = matrix(0,nrow(dataset),cel*cel*ori)
for(i in 1:nrow(dataset)){hog_data[i,] = HOG(matrix(as.numeric(dataset[i,2:785]), nrow = 28, byrow = T), cells = cel, orientations = ori)}
hog_data  = data.frame("label" = as.factor(dataset$label), hog_data)


hog_competition= matrix(0,nrow(competition),cel*cel*ori)
for(i in 1:nrow(competition)){hog_competition[i,] = HOG(matrix(as.numeric(competition[i,1:784]), nrow = 28, byrow = T), cells = cel, orientations = ori)}
hog_competition  = data.frame(hog_competition)

# La matrice hog_data contient les images transformées (en ligne). La première colonne représente la classe (label) de l'image
# c'est à dire un chiffre entre 0 et 9

# Reprenez les différentes étapes de la première partie en utilisant la représentation HOG, avec pour objectif d'améliorer
# votre score.
# Vous pouvez changer les paramètres cel et ori 


#### S�paration AVT

set.seed(20) 
nall = nrow(hog_data) 
ntrain = floor(0.7 * nall) 
nvalid = floor(0.15 * nall) 
ntest = nall - ntrain - nvalid  
index = sample(nall) 

hog_train = hog_data[index[1:ntrain],] 
hog_valid = hog_data[index[(ntrain+1):(ntrain+nvalid)],]
hog_test = hog_data[index[(ntrain+nvalid+1):(ntrain+nvalid+ntest)],] 

positionCible = 1

#### Arbre

#on apprend le modele sur le train
modeleArbre_hog = rpart(as.formula(paste(colnames(hog_data)[positionCible],"~.",sep="")), data = hog_train, control = list(minbucket = 1,cp = 0, minsplit = 1))

#laméthode niveauElagageErreurG retourne pour un niveau d'élagage déterminé l'erreur de validation
#Le niveau d'élagage est calculé à modeleArbre$cptable[,1]
#resultat, [2] niveau d'élagage, [3] erreur en validation
resultArbre_hog = niveauElagageErreurG(modeleArbre_hog,hog_train,hog_valid,hog_test,positionCible,hog_competition)



###################################################"SVM

resultSVM_radial_hog = GeneSVM (hog_train,hog_valid,hog_test,positionCible,"radial",hog_competition)
resultSVM_polynomial_hog = GeneSVM (hog_train,hog_valid,hog_test,positionCible,"polynomial",hog_competition)



############################################### KNN

resultKnn_hog = knnVal(hog_train,hog_valid,hog_test,positionCible,hog_competition)



################################################# Regression

modele = learn_nn(hog_train[,c(1:ncol(hog_train))[-positionCible]], hog_train[,positionCible], 0,100, c(24, 10))
predV = predVal(modele,hog_valid,positionCible)
errorV = sum(predV != hog_valid[,positionCible])

predTest = predVal(modele,hog_test,positionCible)
compet = predVal(modele,hog_competition,1)
write.csv(data.frame("Id"=c(1:1000), "label"=compet), file ="./mespredictionsRegression.csv", row.names = F)

erreurG= sum(predTest != hog_test[,positionCible])


resultRegMultiClass = data.frame("regLineMultiC","",errorV,erreurG)

#################################################### For�t

resultForet_hog = foretByMax(hog_train,hog_valid,hog_test,positionCible,hog_competition)
