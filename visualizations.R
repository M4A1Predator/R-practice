#install.packages("ggplot2") #<- This line is needed in the first run. 
library(ggplot2)
#
# Load data
mydata <- read.table("german_data.csv", header=TRUE,sep=",")
stockData <- read.table("data_akbilgic.csv", header=TRUE,sep=",")
names(mydata)
summary(mydata)
head(mydata)


# 1 scatter plot
plot(mydata$Age, mydata$CreA, col=c("red","blue")[mydata$class], 
     main="Scatterplot Example", 
     xlab="Age in years ", ylab="Credit amount", pch=19)

## ggplot2
p <- ggplot(mydata, aes(Age, CreA))
p+geom_point(aes(colour = factor(class)))

# 2. pie chart
counts <- table(mydata$Pur)
lbls <- c("car (new)",  "car (used)",  "furniture/equipment",  "radio/television",  "domestic appliances",  "repairs",  "education",  "retraining", "business", "others" )
pie(counts,labels = lbls,main="Purpose Distribution")

## ggplot2
c <- ggplot(mydata, aes(x =factor(1), fill = factor(Pur))) + geom_bar(width = 5)
c + coord_polar(theta = "y")

# 3. bar plot
counts <- table(mydata$Pur)
barplot(counts, main="Purpose Distribution", xlab="Purpose")

## ggplot2
c <- ggplot(mydata, aes(factor(Pur)))
c + geom_bar()


# 4. histogram plot
hist(mydata$CreA,breaks=20, xlab="Credit amount")

## ggplot2
c <- ggplot(data=mydata, aes(mydata$CreA)) 
c + geom_histogram()

# 5. density plot
d <- density(mydata$CreA) # returns the density data 
plot(d, main="Density plot example", xlab="Credit amount") # plots the results

## ggplot2
c <- ggplot(data=mydata, aes(mydata$CreA)) 
c+geom_density()

# 6. boxplot
boxplot(mydata[c(2,13)], names= c("Duration (month)", "Age (year)"), main="Boxplot") 

## ggplot2
library(reshape2)
c <- ggplot(melt(mydata[c(2,13)]), aes(variable, value, fill = variable)) 
c + geom_boxplot()+  guides(fill=FALSE)


# 7. feature pairwise plot
pairs(stockData[3:8], main="Density plot example")


# 8. feature pairwise with correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  test <- cor.test(x,y)   
  text(0.5, 0.5, txt, cex = cex * r) 
}
pairs(stockData[3:10], upper.panel=panel.cor)





