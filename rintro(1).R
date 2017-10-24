####################################
#Date: 01/25/2017
#R intro II
#author: Qiong Zhang
####################################


#Data Science

#real problem
#data
#descriptive analysis, modelling...

datalink <- 'http://stat-computing.org/dataexpo/2013/data/sotc-08.csv'
mydata<-read.csv(datalink)

head(mydata)

#how many numerical columns?
#is.numeric
is.numeric(mydata$QSB)
is.numeric(mydata$LOYALTY)
numeric.ind <- sapply(mydata, is.numeric)
sum(numeric.ind)

#subset numerical columns from mydata
mydata.num <- subset(mydata, select=numeric.ind)
mydata.num <- mydata[ ,numeric.ind]


#max value of each num column
#mean value of each num column
#sapply
sapply(mydata.num, mean, na.rm=TRUE)
sapply(mydata.num, max, na.rm=TRUE)
apply(mydata.num, 2, mean, na.rm=TRUE)
# apply(mydata.num, 1, mean, na.rm=TRUE)


# compare LOYALTY of different communities

# can you show a graph to compare the
# average loyalty of all 26 communities?
# tapply
# barplot

mean.qsb <- tapply(mydata$LOYALTY, mydata$QSB, 
mean, na.rm=TRUE)

barplot(mean.qsb, las=2, cex.names=0.5, 
ylim=c(0,5))

mean.qsb.ord <- mean.qsb[order(mean.qsb)]

barplot(mean.qsb.ord, las=2, cex.names=0.5, 
ylim=c(0,5))

# H_0: LOYALTY_PHILA=LOYALTY_Aberdeen
# H_a: LOYALTY_PHILA>LOYALTY_Aberdeen
#loyalty_phila <- 
#mydata$LOYALTY[mydata$QSB=="Philadelphia, PA"]

loyalty_phila <- with(mydata, 
      LOYALTY[QSB=="Philadelphia, PA"])

loyalty_aber <- with(mydata, 
      LOYALTY[QSB=="Aberdeen, SD"])

# attach not perferred

t.test(loyalty_phila, loyalty_aber,
       alternative="greater"
)
# p-value>0.05 not significant reject H_0

#comparing all communities
# pairwise.t.test

with(mydata,
     pairwise.t.test(LOYALTY, QSB)
)
# graphically show the test result

# good community: loyalty > mu0
# H_0: loyalty <= mu0
# H_a: loyalty > mu0
# Reject H_0, good community
# p.value <= 0.05, good community

mu0 <- 3
# =, <-
# <- recommended
t.test(loyalty_phila, mu=mu0, alternative="greater")
t.test(loyalty_phila, mu=mu0, 
       alternative="greater")$p.value


#code a function to conduct test
# H_0: loyalty <= mu0
# H_a: loyalty > mu0
my.test <- function(loyalty, mu0) {
        # test H_a: loyalty > mu0
        #
        # Args:
        # loyalty: a vector of loyalty values
        # mu0: a given constant
        #
        # Retrun:
        # p.value
        p.value <- t.test(loyalty, mu=mu0, 
       alternative="greater")$p.value
        return(p.value)
}

my.test(loyalty_phila, 3)

med_Loy <- median(mydata$LOYALTY,na.rm=TRUE)

with(mydata,
     tapply(LOYALTY, QSB, my.test, mu0=med_Loy)
)

# graphically show these pvalues




















































































































 






























































































