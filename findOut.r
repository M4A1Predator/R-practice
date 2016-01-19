library(mice)
library(VIM)
library(lattice)
library(ggplot2)

setwd("C:\\Users\\PredatorPy\\Desktop\\Office\\INT492\\SamplesDataForLecture021")

myData <- read.table("data_akbilgic.csv", header = TRUE, sep = ",")
# find mean in each columns
aa<-apply(myData[,2:10] ,2 ,mean)
dataIrq<-apply(myData[,2:10],2,IQR)

# Define formular to define outlier
s = dataIqr/(2 * 0.6745)

# prepare data in newData
newData=myData

# Add missing to Outlier cell
for(i in 2:(ncol(myData))){
  aaa<-abs(myData[,i]-aa[i-1])>3*s[i-1]
  newData[aaa,i]<-NA
  print(aaa)
}
