#####################

# HW2 groups:

# Group 1: Patrick Link and Jifang Zhao

# Group 2: Manlei Wu and Jing Zhang

# Group 3: Michael Valentine and Wei Yang

# Group 4: Kishan Khana and Bethany Young

# Group 5: Tianchi Zhang

######################################

####################################

# HW2: due 9:30 am Feb 27            

# Group Members:                             

# Filename: GroupNumberHW2.r           

####################################


#Explore AirData (load from pm.Rdata)

load("pm.Rdata")

library(dplyr)

library(ggplot2)

library(tidyr)

library(gridExtra)


# 1) generate a line graph, use date as x-axis, average AQI as y-axis, 

# different color to distinguish year 05 and 14


# 2) Generate scatter plots, use Latitude as x-axis, Longitude as y-axis, 

# put data in year 05 and 14 in different windows

# use different color to denote whether or not this location 

# have more than 100 days with AQI> 50



# Now explore imdb movie dataset
movies<- read.table("movies.tab",sep="\t", header=TRUE, quote="", comment="")


# 3) Draw a histogram for the lengths of movies 

# Draw a histogram for the lengths of movies  

# after removing 5% of outliers.

# Put these two graphs together for comparison


#4) The variable "Short" indicates whether a film was classified 

# as a "short" film (1--yes, 0--no). Draw a bar plot to show 

# the frequencies of  short movies and long movies. 

# Also draw a graph to show the rule which defines "short" movies. 

# Put these two graphs together for comparison



# 5) For long movies made after 2000,

# show graphs to display the relationship between 

# rating and budget, would this relationship vary

# for different movie genre?