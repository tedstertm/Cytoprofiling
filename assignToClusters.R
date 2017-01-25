assignToClusters <- function(data, nClusters=3, rndSeed=1234)
{
	require(EMCluster)
	set.seed(rndSeed)
	yo <- data[!is.na(data)]
	if(length(yo) > 100000)
	{
		x <- data.frame(x=sample(yo, 100000))
	}
	else
	{
		x <- data.frame(x=yo)
	}
	
	
	emobj <- simple.init(x, nclass = nClusters)
	control <- .EMControl(alpha = 0.99, short.iter = 200, short.eps = 1e-2,
					  fixed.iter = 1, n.candidate = 3,
					  EM.iter = 200, EM.eps = 1e-2, exhaust.iter = 20)
	ret <- emcluster(x, emobj, assign.class = TRUE, EMC=control)
	
	temp <- data.frame(x=x$x, LDClass=ret$class)
	tempMu <- data.frame(mu=as.vector(ret$Mu), LDClass=1:nrow(ret$Mu))
	tempMu <- tempMu[order(tempMu$mu),]
	temp2 <- temp
	tempMu2 <- tempMu
	for(i in 1:nrow(tempMu))
	{
		temp[temp2$LDClass==tempMu$LDClass[i],'LDClass'] <- i
		tempMu2$LDClass[i] <- i
	}
	
	temp$LDClass <- temp$LDClass-1
	
	thresh <- list()
	for(i in (nrow(tempMu)-1):1)
	{
		tempThresh <- min(temp[temp$LDClass == i,'x'])
		if(!length(tempThresh)==0)
		{
			thresh[[i]] <- tempThresh[1]
		}
	}
	
	return(list(data=temp, mu=tempMu2, thresh=thresh, emclusterObj=ret))
}





for(i in length(imp.features)){
	index <- imp.features[i]
	smaller.Data$index <- small.Data[, grepl(index, names(small.Data))]
}