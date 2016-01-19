library(mice)
library(VIM)
library(lattice)
library(ggplot2)

# chdir
setwd("C:\\Users\\PredatorPy\\Desktop\\Office\\INT492\\SamplesDataForLecture021")

# load data
myData <- read.table("german_data.csv", header = TRUE, sep = ",")
# prepare data
iData = myData[c(2,5)]

# find mean
myMean <- apply(iData[,1:2], 2, mean)
# find IQR
dataIqr<-apply(iData[,c(1,2)],2,IQR)
print(myMean)

s = dataIqr/(2 * 0.6745)

newData = iData
for(i in 1:ncol(iData)){
  #aaa<-(iData[i]) > myMean[i]
  isOut <- abs(iData[i]-myMean[i]) > 3*s[i]
  newData[isOut, i]<-myMean[i]
  #newData[isOut, i]<-NA
}
newMean <- apply(newData[,1:2], 2, mean)
print(newMean)

# define plot columns
par(mfrow=c(2,2))
# before
boxplot(iData[,2], main="Credits before outlier treatment", range=0, horizontal=TRUE)
#after
boxplot(newData[,2], main="Credits after outliers treatment", range=0, horizontal=TRUE)

# before
boxplot(iData[,1], main="Duration before outlier treatment", range=0, horizontal=TRUE)
#after
boxplot(newData[,1], main="Duration after outliers treatment", range=0, horizontal=TRUE)



#count <- table(newData[,2])
#barplot(count, main="Bar plot CreA", xlab="amount", horiz=TRUE)
#barplot(table(iData[,2]), main="Bar plot CreA", xlab="amount", horiz=TRUE)



