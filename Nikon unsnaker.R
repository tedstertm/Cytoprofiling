#I have no idea what's going on.

#First let's read the file

urFileIn <- "/Users/Teddy/Desktop/LiveDead2/my_table.csv"
urFileOut <- "/Users/Teddy/Desktop/LiveDead2/my_unsnakedtable.csv"

dater <- read.csv(urFileIn)

#ok now you need to danger rope your way forwards and back

#math out the live fraciton
arry <- dater$Positive.Cell.Count/(dater$Total.Cell.Count+dater$Negative.Cell.Count)*100

#make a matrix to fit your jazz, just remember how many rows and cols it is
semiSorted <- matrix(arry, nrow = 16, ncol = 6, byrow = TRUE)

#el blanko matrix for your sorted jazz
sorted <- matrix(nrow = nrow(semiSorted), ncol = ncol(semiSorted))

#loop through the matrix, at even rows flip the contents
for(i in 1:nrow(semiSorted)){
	if(i %% 2 == 0){
		temp <- semiSorted[i,]
		for(j in 1:length(temp)){
			dumdum <- length(temp)-j+1
			sorted[i,dumdum] <- temp[j]

		}
	}else{
		sorted[i,] <- semiSorted[i,]
	}
}

write.csv(sorted, urFileOut)


