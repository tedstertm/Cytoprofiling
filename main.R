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
fileHome <- '/Users/Teddy/Documents/MM Adhesion/2017-01-19 MM Stacks Adhesion Morphology/DefaultDB.db'


#source loadDB.R before executing this
#all functions should live in the Cytoprofiling folder in your Github directory

#Turn that database into a dataframe
big.Data <- load.db(fileHome, return.variable = MyExpt.Per.Object)

#Sometimes you need to sample several rows instead of running a gajillion features
small.Data <- big.Data[sample(nrow(big.Data), 1000), ]

#Get important features with the randomForest function

#This changes depending on what your truth
small.Data$Truth <- 'Adherent'
small.Data[small.Data$ImageNumber >= 17,]$Truth <- 'Suspended'
small.Data$Truth <- as.factor(small.Data$Truth)

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

numClasses <- 3

imp.smallData$Class <- EMClustBud(imp.smallData, numClasses, return.variable = temp1)

