#main.R
#This file uses a bunch of functions that live inside of the Cytoprofiling folder
#in your github directory. 

#This small package was written to get the database you made with cellprofiler,
#extact the features and build a model with them so you might be able to dump
#features into a bucket or predict what a cell will do based on its functions.


#This is where you call your functions friendo.
#I haven't made a program that calls functions in like 10 years so I'm sorry in advance



#Set your working directory to your cytoprofiling github folder
setwd('/Users/Teddy/Documents/Github/Cytoprofiling')
source('loadDB.R')
source('CellProfilerRandomForest.R')
source('CPAClustering.R')



#fileHome is where the .db file lives
fileHome <- '/Users/Teddy/Documents/MM Adhesion/2017-01-26 MM Adhesion on Collagen/DefaultDB.db'


#source loadDB.R before executing this
#all functions should live in the Cytoprofiling folder in your Github directory

#Turn that database into a dataframe
big.Data <- load.db(fileHome, return.variable = MyExpt.Per.Object)

#Sometimes you need to sample several rows instead of running a gajillion features
small.Data <- big.Data[sample(nrow(big.Data), 2500), ]
#Get important features with the randomForest function

#This changes depending on what your truth
small.Data$TNF <- 'TNFa-'
small.Data[small.Data$ImageNumber >= 13,]$TNF <- 'TNFa+'
small.Data$TNF <- as.factor(small.Data$TNF)
small.Data$Collagen <- 'Collagen'
small.Data[small.Data$ImageNumber >= 7,]$Collagen <- 'Fibronectin'
small.Data[small.Data$ImageNumber >= 13,]$Collagen <- 'Collagen'
small.Data[small.Data$ImageNumber >= 19,]$Collagen <- 'Fibronectin'
small.Data$Collagen <- as.factor(small.Data$Collagen)
small.Data$Adherent <- 'Adherent'
sus.Imgnum = c(2,3,6,7,10,11,14,15,18,19,22,23)
small.Data[small.Data$ImageNumber %in% sus.Imgnum,]$Adherent <- 'Suspended'
small.Data$Adherent <- as.factor(small.Data$Adherent)
small.Data$Truth <- paste(small.Data$TNF, small.Data$Collagen, small.Data$Adherent, sep="_")
small.Data$Truth <- as.factor(small.Data$Truth)

#Don't forget to get rid of your half-truths
small.Data$Adherent = NULL
small.Data$Collagen = NULL
small.Data$TNF = NULL


#set the randSeed to a couple of different numbers to validate your model, human
randSeed <- 17

imp.Feats <- CPRandomForest(small.Data, big.Data, randSeed, return.variable = imp.Feats)


#If you would like you can also manually do this from your CPAnalyst
# imp.Feats <- c('Cells_AreaShape_Extent',
#                  'Cells_AreaShape_Solidity',
#                  'Cells_AreaShape_Compactness',
#                  'Cells_AreaShape_FormFactor')



#Get a dataset with only your important features in it
imp.Data <- big.Data[, c(imp.Feats)]
imp.smallData <- small.Data[, c(imp.Feats)]


#Cluster your objects using the important features to get a decent separation of jimjams

#Use your best guess to figure out how many classes you want, you can change
#this when you'd like

numClasses <- 4

imp.smallData$Class <- EMClustBud(imp.smallData, numClasses, return.variable = temp1)

#Plot the things that jay thinks are cool and that are cool. library(Rtsne)

library(Rtsne)
library(EMCluster)
library(dplyr)
numeric.impData <- select_if(small.Data, is.numeric)

sneX <- Rtsne(as.matrix(numeric.impData)) 
emobj5u <- simple.init(sneX$Y, nclass = 8) 
clustResults2 <- emcluster(sneX$Y, emobj = emobj5u, pi = NULL, Mu = NULL,
LTSigma = NULL, lab = NULL, EMC = .EMC, assign.class = TRUE)

pts <- data.frame(sneX$Y)


f <- list(family = "Arial", size = 18, color = "#000000") 
x <- list(title = "x Axis",titlefont = f) 
y <- list(title = "y Axis",titlefont = f) 
Class <- clustResults2$class

library(plotly)

plot_ly(mode = 'markers', x=pts$X1, y=pts$X2, color=Class) %>% layout(xaxis =
x, yaxis = y)

