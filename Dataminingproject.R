#============

training<-read.csv("training.csv",sep = ',', header = TRUE, stringsAsFactors = FALSE)


training$class.material<-ifelse(training$class =="zero", "Yes", "No")
training$class.purification<-ifelse(training$class =="one", "Yes", "No")
training$class.crystallization<-ifelse(training$class =="two", "Yes", "No")
training$class.crystallizable<-ifelse(training$class =="three", "Yes", "No")

write.csv(training, file = "training5.csv",row.names = FALSE,sep = ',')

read.table("breast-cancer-wisconsin.data", fileEncoding="UTF-16", dec=",")



library(ggplot2)
library(gridExtra)
library(car)

summary(training[,1:20])

#p <- ggplot(training, aes(x=AAcomp_A))
#p+geom_density()+
 # geom_histogram(aes(y=..density..), alpha=0.2)

p <- ggplot(training, aes(x=class.material, y=AAcomp_N))
p+geom_boxplot()+coord_flip()


lmf <- aov(class.material~AAcomp_N, data = training)
summary(lmf)
Anova(lmf)

lm2 <- glm(class.material~AAcomp_N, training, 
           family=binomial(link=probit))