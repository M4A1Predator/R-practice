library(mice)
library(VIM)
library(lattice)
library(ggplot2)

myData <- airquality
head(myData)
summary(myData)
md.pattern(myData)
marginplot(myData[,1:2])

#imputation using column mean
tempData1 <- mice(myData, meth='mean')
summary(tempData1)
xyplot(tempData1,Ozone ~ Wind+Temp+Solar.R)
densityplot(tempData1)

#imputation using Predictive Mean Matching(PMM)
tempData2 <- mice(myData,meth='pmm')
xyplot(tempData2,Ozone ~ Wind+Temp+Solar.R)
summary(tempData2)
densityplot(tempData2)

#return the completed dataset
completedData <- complete(tempData2,1)
summary(completedData)

#outlier
library(outliers)

newData[outlier]<-NA
newData<-completedData

summary(newData)

aa<-apply(completedData[1:4],2,mean)
dataIrq<-apply(completedData,2,IQR)
s = dataIrq/(2 * 0.6745)
for(i in 1:(ncol(completedData)-2)){
  aaa<-abs(completedData[,i]-aa[i])>3*s[i]
  newData[aaa,i]<-NA
}

newData[outlier]<-NA