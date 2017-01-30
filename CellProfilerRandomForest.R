#This program uses the randomForest function to build a model using cell features
#collected from CellProfiler with a ground truth entered by you and determines which
#buckets each cell goes in. It's cool and way better than you at this shit.

#Let's turn this bad boy into a function real soon 

CPRandomForest <- function(tempSmall, tempBig, rand.seed, return.variable = NULL)
{
#Let's load that liberry
library(randomForest)


#Sets a random seed, change this number a few times for validation that your model
#actually works.
set.seed(rand.seed)

#This is meat of your jazz, use the '-' to remove columns you don't want to influence
#your model
data.rf <- randomForest(Truth ~ . -ImageNumber - ObjectNumber, 
                        data=tempSmall, importance=TRUE, proximity=TRUE)

#print out that model, friend and enjoy jay's hard work.
data.rf

#This section makes df called summary that contains the order of importance of
#all your dankaf features.
summary <- data.rf$importance
features <- row.names(summary)
summary <- data.frame(summary)
summary$feature <- features
summary <- summary[order(-summary$MeanDecreaseAccuracy),]

#type the following line to see your importance rankings
#summary$feature

# hastag dataviz
barplot(names=summary$feature, height=summary$MeanDecreaseAccuracy)

#Let's try and export the important features

#make a list of the 20 most important features
imp.Feats <- summary$feature[1:20]


}