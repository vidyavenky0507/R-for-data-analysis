#############################
# Graphical data analysis
# date: 02/13/2017
##############################

library(dplyr)
library(ggplot2)
library(nycflights13)
data(iris)

# single continuous data
# histogram
# qqplot
# boxplot
# density
# ...

# single categrical data
# carrier in flights
# Species in iris
# how many categories?
# counts/freqency
# barplot---count/freqency

flights %>%
  ggplot(aes(x=carrier))+
  geom_bar(fill="blue")

# ggsave("bar.png")

# ordered bars

carriers <- flights %>%
  group_by(carrier) %>%
  summarize(
     count=n(),
     freq=n()/nrow(.)
  ) %>%
  arrange(freq) %>%
  mutate(
      carrier=factor(carrier, carrier)
  ) 

carriers %>%
  ggplot(aes(x=carrier))+
  geom_bar(aes(y=freq), stat="identity")+
  scale_y_continuous(labels=scales::percent)



########################
# Two columns
#######################

# continuous vs countinuous 
# iris Sepal.Length vs Sepal.Width
# linear and nonlinear
# clusters
# outliers
# gap
# ...
# scatter plot

p <- ggplot(iris, aes(x=Petal.Length, 
                      y=Petal.Width))

p <- p+geom_point()


p+geom_density2d()
p+geom_smooth(se=FALSE)+theme_bw()
p+geom_smooth(method="lm")


########################
# Qualitative vs Qualitative
# e.g., origin and carrier
# association

p <- ggplot(flights, aes(x=carrier))
p+geom_bar(aes(fill=origin))
p+geom_bar(aes(fill=origin), position="dodge")


# show the two-way count table
with(flights,
     table(origin, carrier))
# test association: chisq.test

# show two-way count table in a graph

flights %>%
   group_by(origin, carrier) %>%
   summarize(
      freq=n()/nrow(.)
   ) %>%
   ggplot(aes(x=carrier, y=origin))+
   geom_tile(aes(fill=freq))+
   scale_fill_continuous(low="yellow", 
                         high="red",
                         labels=scales::percent)


#############################
# Qualitative vs Quantitative
# e.g., Petal.Length vs Species


# 1) boxplot
p <- ggplot(iris, aes(x=Species, y=Petal.Length))
p+geom_boxplot()+coord_flip()

# 2) distingush by color
p <- ggplot(iris, aes(x=Petal.Length))
p+geom_density(aes(color=Species, 
                   fill=Species),
                   alpha=0.3)



# geom_histogram

# 3) faceting
p <- ggplot(iris, aes(x=Petal.Length))+
  geom_histogram()
p+facet_grid(.~Species)
p+facet_grid(Species~.)


################################
# multiple quantitative
#install.packages("GGally")
library(GGally)
# draft

ggparcoord(iris, columns=1:4, 
           scale="uniminmax",
           groupColumn="Species")


iris %>%
  ggpairs(columns=1:4)































































































  
  
  
  
  
  
  
  
  
  
  
  
  


























































































































  
  
  
  
  
  
  
  
  
  
  
































































