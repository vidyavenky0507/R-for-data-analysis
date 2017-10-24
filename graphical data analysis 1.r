#############################
# Graphical data analysis
# date: 02/08/2017
##############################

##########################
# graphical systems in R
###########################


# plot, boxplot, barplot in base packages
# lattice
# ggplot2 ---dplyr

data(iris)

# Petal.width ~ Petal.length
# different color for different species

cols <- c("red", "green", "blue")

plot(Petal.Width~Petal.Length, data=iris,
     col=cols[as.numeric(Species)])
legend("topleft", levels(iris$Species), 
       col=cols, pch=1)


# Lattice
library(lattice)
xyplot(Petal.Width~Petal.Length, data=iris,
  groups=Species, auto.key=TRUE)



#
#install.packages("ggplot2")
library(ggplot2)
ggplot(iris, aes(x=Petal.Length, y=Petal.Width))+
  geom_point(aes(color=Species))+
  geom_smooth(aes(group=Species))+
  theme_bw()



########################
# single continous data
# Petal.Length
########################


# boxplot (boxplot in basepackage)
# histogram
# density_curve
# qqplot

boxplot(iris$Petal.Length)

p <- ggplot(iris, aes(x=Petal.Length))
p+geom_histogram(color="black", bins=20, fill="white")
p+geom_histogram(aes(y=..density..))+
  geom_vline(xintercept=median(iris$Petal.Length), 
             color="red")


p+geom_density()+
  geom_histogram(aes(y=..density..), alpha=0.2)


p <- ggplot(iris, aes(sample=Petal.Length))
p+geom_point(stat="qq")+
  geom_abline(slope=1, color="red",
              intercept=median(iris$Petal.Length))





























































  




























































































