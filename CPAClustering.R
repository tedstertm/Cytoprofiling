#This function clusters and saves data for the supervised learning
#that cellprofiler analyst does.
#I have no idea what I'm doing.


EMClustBud <- function(temp1, classNum, return.variable = temp1)
{
#Now it's time for clustering, let's make sure we have everything
library(EMCluster)

#cool, let's initialize with 5 classes
emobj4u <- simple.init(temp1, classNum)

#clustertime
clustResults <- emcluster(temp1, emobj = emobj4u, pi = NULL, Mu = NULL, LTSigma = NULL,
		lab = NULL, EMC = .EMC, assign.class = TRUE)


#Assign the classes back to your data

temp1$Class <- clustResults$class

newOrder <- 1:length(names(temp1))
newOrder <- c(1:2,max(newOrder),3:(max(newOrder)-1))
temp1 <- temp1[,newOrder]
temp1$Class <- paste0("a",temp1$Class)


}

# 
# #Plot the things that jay thinks are cool and that are cool.
# library(Rtsne)
# 
# sneX <- Rtsne(as.matrix(small.Data))
# emobj5u <- simple.init(sneX$Y, nclass = 4)
# clustResults2 <- emcluster(sneX$Y, emobj = emobj5u, pi = NULL, Mu = NULL, LTSigma = NULL,
# 					  lab = NULL, EMC = .EMC, assign.class = TRUE)
# 
# pts <- data.frame(sneX$Y)
# 
# 
# f <- list(family = "Arial", size = 18, color = "#000000")
# x <- list(title = "x Axis",titlefont = f)
# y <- list(title = "y Axis",titlefont = f)
# Class <- clustResults2$class
# 
# library(plotly)
# 
# plot_ly(mode = 'markers', x=pts$X1, y=pts$X2, color=Class) %>%
# 	layout(xaxis = x, yaxis = y)
# 
# #Reorder the smallData to have class listed
# 
# newOrder <- 1:length(names(small.Data))
# newOrder <- c(1:2,max(newOrder),3:(max(newOrder)-1))
# small.Data <- small.Data[,newOrder]
# small.Data$Class <- paste0("a",small.Data$Class)
# 
# 
# #Now save your data and open it with CPA
# write.csv(small.Data, "/Users/Teddy/Documents/MM Adhesion/2017-01-19 MM Stacks Adhesion Morphology/MyNewTrainingSet.csv", row.names = FALSE)
# 
# 
# #After you classify your jazz with analyst
# big.Data <- read.db('/Users/Teddy/Documents/MM Adhesion/2017-01-19 MM Stacks Adhesion Morphology/DefaultDB.db',
# 				return.variable = MyExpt.Per.Object)
# 
# sortedbig.Data <- big.Data[order(big.Data$ImageNumber, big.Data$ObjectNumber),]
# sortedobjectClass <-objectClass[order(objectClass$ImageNumber, objectClass$ObjectNumber),]     
# sortedbig.Data$myClass <- sortedobjectClass$class_number
# 
# realCells <- sortedbig.Data[sortedbig.Data$myClass==2 | sortedbig.Data$myClass==3,]
# 
# realCellsFancy <- realCells
# realCellsFancy$ImageNumber <- NULL
# realCellsFancy$ObjectNumber <- NULL
# realCellsFancy$myClass <- NULL
# 
# for(badCol in names(realCellsFancy)[grepl('_X', names(realCellsFancy))])
# {
# 	realCellsFancy[,badCol] <- NULL
# }
# for(badCol in names(realCellsFancy)[grepl('_Y', names(realCellsFancy))])
# {
# 	realCellsFancy[,badCol] <- NULL
# }
# for(badCol in names(realCellsFancy)[grepl('_Object_Number', names(realCellsFancy))])
# {
# 	realCellsFancy[,badCol] <- NULL
# }
# 
# #Let's try this out
# for(badCol in names(realCellsFancy)[grepl('Radius', names(realCellsFancy))])
# {
# 	realCellsFancy[,badCol] <- NULL
# }
# for(badCol in names(realCellsFancy)[grepl('Feret', names(realCellsFancy))])
# {
# 	realCellsFancy[,badCol] <- NULL
# }
# for(badCol in names(realCellsFancy)[grepl('AreaShape', names(realCellsFancy))])
# {
# 	realCellsFancy[,badCol] <- NULL
# }
# 
# 
# library(Rtsne)
# 
# sneX <- Rtsne(as.matrix(realCellsFancy))
# emobj5u <- simple.init(sneX$Y, nclass = 5)
# clustResults2 <- emcluster(sneX$Y, emobj = emobj5u, pi = NULL, Mu = NULL, LTSigma = NULL,
# 					  lab = NULL, EMC = .EMC, assign.class = TRUE)
# 
# pts <- data.frame(sneX$Y)
# 
# 
# f <- list(family = "Arial", size = 18, color = "#000000")
# x <- list(title = "x Axis",titlefont = f)
# y <- list(title = "y Axis",titlefont = f)
# Class <- clustResults2$class
# 
# library(plotly)
# 
# plot_ly(mode = 'markers', x=pts$X1, y=pts$X2, color=Class) %>%
# 	layout(xaxis = x, yaxis = y)
# 
# realCells$Class <- clustResults2$class
# realCells$myClass <- NULL
# #Reorder the smallData to have class listed
# 
# newOrder <- 1:length(names(realCells))
# newOrder <- c(1:2,max(newOrder),3:(max(newOrder)-1))
# realCells <- realCells[,newOrder]
# realCells$Class <- paste0("a",realCells$Class)
# 
# 
# write.csv(realCells, "/Users/Teddy/Desktop/Jake/MyAwesomeTrainingSet.csv", row.names = FALSE)
