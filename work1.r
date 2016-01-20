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

summary(iData)
summary(newData)

# Standardization
par(mfrow=c(1,1))
metrix <- scale(newData[,1:2])
boxplot(metrix, range=0, horizontal=TRUE, main="Standardization")

summary(metrix)
#dotchart(iData[1:20,2], labels = row.names(iData))

# Categorization
#print(metrix[1:10,2])
par(mfrow=c(1,2))
cate1 <- findInterval(metrix[,1], c(-2,-1,0,1,2,3))
summary(cate1)
cate2 <- findInterval(metrix[,2], c(-2,-1,0,1,2,3))
summary(cate2)
dotchart(cate1, main = "Dur")
dotchart(cate2, main = "CreA")

myInterval <- cut(metrix[1:10,2], c(-2,-1,0,1,2,3))
print(myInterval)


#count <- table(newData[,2])
#barplot(count, main="Bar plot CreA", xlab="amount", horiz=TRUE)
#barplot(table(iData[,2]), main="Bar plot CreA", xlab="amount", horiz=TRUE)

"
ref = http://www.r-bloggers.com/centering-and-standardizing-dont-confuse-your-rows-with-your-columns/
ref = http://stackoverflow.com/questions/12979456/r-code-to-categorize-age-into-group-bins-breaks
ref = http://stackoverflow.com/questions/26103140/how-can-i-create-a-custom-colour-scale-using-ggplot2-and-geom-tile
ref = http://stackoverflow.com/questions/6774339/r-how-do-i-put-two-box-plots-next-to-each-other-keeping-same-y-range-for-both
"

