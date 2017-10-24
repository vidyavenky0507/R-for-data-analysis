####################################
#Date: 01/23/2017
#R intro
#author: Qiong Zhang
####################################


#Data Science

#real problem
#data
#descriptive analysis, modelling...


mydata<-read.csv('http://stat-computing.org/dataexpo/2013/data/sotc-08.csv')

head(mydata)

#how many comminities in the data?
#how many samples in each community?
communities <- table(mydata$QSB)
#table function--frequency table

#how many rows and columns in the data
dim(mydata)
nrow(mydata)
ncol(mydata)


#SAFETY, PASSION LOYALTY ECONOMY OPENESS QSB
#Philadelphia, PA

#subset five columns SAFETY, PASSION LOYALTY ECONOMY OPENESS
#from Philadelphia, PA
#subset
features<-c("SAFETY", "PASSION", "LOYALTY", "ECONOMY", "OPENNESS")
phila.data <- subset(mydata, QSB=="Philadelphia, PA", select=features)


#calculate the mean of each the column
#mean
mean(phila.data$SAFETY,na.rm=TRUE)
sapply(phila.data,mean, na.rm=TRUE)

#use a graph to show the relationship 
#between loyalty and safety
plot(LOYALTY~SAFETY,data=phila.data)


#go back to mydata
#calculate the mean of loyalty for each community
mean(mydata$LOYALTY[mydata$QSB=="Philadelphia, PA"],
na.rm=TRUE)
tapply(mydata$LOYALTY, mydata$QSB, mean, na.rm=TRUE)

















































