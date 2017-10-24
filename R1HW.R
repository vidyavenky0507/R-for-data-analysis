#####################################

# HW1: due 9:30 AM  Feb 13          #

# Group Members:                    #

# Filename: HW1_(groupnumber).R     #

# Submit R script to blackboard     #

# Submit hardcopy R script and      #

# output results                    #

#####################################


# Load 2014 and 2005 Air Quality Data

# from "pm.Rdata"

# Answer the following questions
#load 2014 data
load("pm.Rdata")

# 1) For 2005 data, find the states that contains

# more than 40 local sites for measuring air quality

# Note: the name of sites are recorded in column

# Local.Site.Name 

with(data05,{
     num <-tapply(Local.Site.Name,State.Name,function(x) length(unique(x)))
     num<-num[which(num>40)]
     num})
#OR 
Number05.local.site <-tapply(data05$Local.Site.Name,data05$State.Name,function(x) length(unique(x)))
State05.local40<-Number05.local.site[which(Number05.local.site>40)]
State05.local40



# 2) From 2005 to 2014, which states have

# the number of local sites increased?

Number14.local.site <-tapply(data14$Local.Site.Name,data14$State.Name,function(x) length(unique(x)))
State05.local <- data.frame(State=names(Number05.local.site),
                      Number.local.site05=Number05.local.site)

State14.local <- data.frame(State=names(Number14.local.site),
                            Number.local.site14=Number14.local.site)

State.Local.Site<- merge(State05.local,State14.local,by="State")

states.local.increased<- subset(State.Local.Site,Number.local.site05<Number.local.site14,select = State)



# 3) Creat columns in both data05 and data14

# to record the month of each row

# Drow boxplots of AQI with regard to Month

# for both 2005 and 2014 datasets


data05$Month <- with(data05,
                        format(as.Date(Date.Local), "%m"))
data14$Month <- with(data14,
                     format(as.Date(Date.Local), "%m"))

mean05.AQI<- tapply(data05$AQI, data05$Month, 
                       mean, na.rm=TRUE)
mean14.AQI<- tapply(data14$AQI, data14$Month, 
                    mean, na.rm=TRUE)

par(mfrow=c(1,2))
barplot(mean05.AQI, las=2, 
        cex.names=0.5, ylim=c(0, 60))
barplot(mean14.AQI, las=2,
        cex.names=0.5, ylim=c(0, 60))


# 4) Generate a data frame containing the following

# columns: Med.AQI, State.Name, Month, Year

# This data frame collects the median AQI

# of each state, each month for year 2005

# and year 2014

# Display the first six rows of this data frame only

# How many rows in this data frame :- 626 rows
install.packages("dplyr")
install.packages("nycflights13")

library(dplyr)
library(nycflights13)
data05$Year <- with(data05,
                     format(as.Date(Date.Local), "%y"))
data14$Year <- with(data14,
                     format(as.Date(Date.Local), "%y"))

data05.MedAQI<-data05 %>%
  group_by(State.Name,Month,Year) %>%
  summarize(
    Med.AQI05= median(AQI, na.rm=TRUE)
     ) %>%
  tbl_df() 
slice(data05.MedAQI,1:6)
data14.MedAQI<-data14 %>%
  group_by(State.Name,Month,Year) %>%
  summarize(
    Med.AQI14= median(AQI, na.rm=TRUE)
  ) %>%
  tbl_df() 

slice(data14.MedAQI,1:6)
# 5) For each state, pair the AQI records

# with regard to each month, and test

# Ha (Alternative): monthly median AQI in 2005

# > monthly median AQI in 2014

# Show the p.values in a barplot

data.MedAQI<- merge(data05.MedAQI,data14.MedAQI,by = c("State.Name","Month"))
data.MedAQI<-tbl_df(data.MedAQI)
data.MedAQI %>%
  group_by(State.Name,Month) %>%
  summarize(
    p.value=t.test(Med.AQI05,Med.AQI14,paired = TRUE,alternative = "greater")$p.value
     )%>%
  tbl_df()

  