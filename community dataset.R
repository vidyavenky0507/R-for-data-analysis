#=======================================================
#Date:- 01/23/2017
#R intro
#author:- vidya venkatesh
#======================================================
#Load data from website
mydata<-read.csv('http://stat-computing.org/dataexpo/2013/data/sotc-08.csv')

#how many rows and columnsin the data
dim(mydata)
nrow(mydata)
ncol(mydata)

features<-c("SAFETY","PASSION","LOYALTY","ECONOMY","OPENNESS")

phila.data<-subset(mydata,QSB=="Philadelphia, PA",select=features)

#calculate mean from all the colomns
colMeans(phila.data,na.rm=TRUE)
#calculate mean each the colomns
mean(phila.data$SAFETY,na.rm = TRUE)
sapply(phila.data,mean,na.rm=TRUE)

#use a graph to show the relationship between loyalty and safety
plot(LOYALTY~SAFETY,data = phila.data)

#go back to my data
#calculate the mean of the layalty value for each community
#unique(mydata$QSB)
#...you get the row number from thiscommunity<-c(as.factor(unique(mydata$QSB)))

#get one 
mean(mydata$LOYALTY[mydata$QSB=="Philadelphia, PA"],na.rm = TRUE)

tapply(mydata$LOYALTY,mydata$QSB,mean,na.rm= TRUE)

#======================================================================================
#Lecture 2:- 1/25/2017
#======================================================================================

#how many numerical columns?
is.numeric(mydata$LOYALTY)
sapply(mydata, is.numeric)
numeric.ind<-sapply(mydata, is.numeric)
sum(numeric.ind)

#how to subset all the numeric columns from the data
numeric.subset<-subset(mydata,select = numeric.ind)
#or
mydata.numeric.sunset<-mydata[,numeric.ind]

#max value of each num column
#mean value of each num column
colMeans(numeric.subset,na.rm=TRUE)
sapply(numeric.subset,mean,na.rm=TRUE)
sapply(numeric.subset,max,na.rm=TRUE)

#to get mean median etc for all columns
summary(numeric.subset)

#
apply(numeric.subset,1,mean,na.rm=TRUE)

#Compare Loyalty of different Communities

#can you show a graph to compare the avg loyalty value of all 26 communities?
loyalty.mean.values<-tapply(mydata$LOYALTY,mydata$QSB,mean,na.rm= TRUE)
barplot(loyalty.mean.values ,las=2,cex.names=0.5,ylim = c(0.5))

#order the plot from smallest to largest
mean.qsb.ord<-loyalty.mean.values[order(loyalty.mean.values)]
barplot(mean.qsb.ord ,las=2,cex.names=0.5,ylim = c(0.5))

#h_0: IF I WANT TO TEST loyalty_PHILA = loyalty_Aberdeen
#h_1: IF I WANT TO TEST loyalty_PHILA >loyalty_Aberdeen
Loyalty_phila<-mydata$LOYALTY[mydata$QSB =="Philadelphia, PA"]
Loyalty_abreen<-mydata$LOYALTY[mydata$QSB =="Aberdeen, SD"]
t.test(Loyalty_phila,Loyalty_abreen,alternative = "greater")

#doing the same test with the loyalties, but this doesnot create any new  data frames
with(mydata, {
  Loyalty_phila1<-mydata$LOYALTY[mydata$QSB =="Philadelphia, PA"]
  Loyalty_abreen1<-mydata$LOYALTY[mydata$QSB =="Aberdeen, SD"]   
  t.test(Loyalty_phila1,Loyalty_abreen1,alternative = "greater")
})

#if p_value>0.05
#pairwise.t.test
with(mydata,pairwise.t.test(LOYALTY,QSB))

#graphically show the test result
#good community:loyalty>mu0
#H_0:Loyalty<=mu0
#H_a:Loyalty>mu0
#reject H_0

#if P_value<0.05 good community
#test
mu0<-3
t.test(Loyalty_phila,mu=mu0,alternative = "greater")
#just a pvalue
t.test(Loyalty_phila,mu=mu0,alternative = "greater")$p.value

#code a function to test
#H_0:Loyalty<=mu0
#H_a:Loyalty>mu0
#we want to calculate pvalue for each community loyalty

my.test<-funtion(loyalty,mu0){
  #test H_a:loyalty>mu0
  #
  #Args:;oyalty is a vector of loyalty values for each community
  #mu0 is given as a constant
  #Return:p_value
  
  p_value<-t.test(loyalty,mu=mu0,alternative = "greater")$p.value
  return(p_value)

}

#I am passing the loyalty value of the loyalty of the phila to the function and getting the p_value
p_value_phila<-my.test(Loyalty_phila,3)

med_Loy<-
with