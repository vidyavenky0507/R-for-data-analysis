#################################
# Advanced Data Manipulation in R
# Date: 02/01/2017
#################################

# install.packages("dplyr")
# install.packages("nycflights13")

library(dplyr)
library(nycflights13)

# dplyr  Hadley Wickham
# data.table

class(flights)
# tibble
flights <- tbl_df(flights)

print(flights, n=2, width=Inf)

# head
?flights

# subset rows 
filter(flights, dest == "ORD", 
                month == 2,
                day== 1,
                carrier== "UA",
                dep_time < 1200)

subset(flights, dest == "ORD"& 
                month == 2&
                day== 1&
                carrier== "UA"&
                dep_time < 1200)


slice(flights, 10:20)
slice(flights, which.min(arr_delay))


# subset columns
select(flights, month, day, dep_time)
select(flights, month:arr_time)
# starts_with ends_with matches contains
select(flights, contains("time"))


# arrange
# order
arrange(flights, desc(dep_time))
arrange(flights, month, dep_time)

# distinct
# basic: unique
distinct(select(flights, origin, dest))


# summarize 
# tapply, sapply, apply
summarize(flights, 
     delay=mean(arr_delay, na.rm=TRUE),
     dist=mean(distance, na.rm=TRUE))


# group_by summarize
# tapply
flights1 <- group_by(flights, carrier, origin)
summarize(flights1, 
     delay=mean(arr_delay, na.rm=TRUE),
     dist=mean(distance, na.rm=TRUE))

# system.time

# pipe
print(flights, n=2, width=Inf)
flights %>%
  print(n=2, width=Inf)

flights %>%
  group_by(carrier) %>%
  summarize(
     delay=mean(arr_delay, na.rm=TRUE),
     dist=mean(distance, na.rm=TRUE)) %>%
  slice(which.min(delay))


flights %>%
  group_by(carrier) %>%
  summarize(
     delay=mean(arr_delay, na.rm=TRUE),
     dist=mean(distance, na.rm=TRUE))

# summarize(
#     flights %>%
#  group_by(carrier),
#     delay=mean(arr_delay, na.rm=TRUE),
#     dist=mean(distance, na.rm=TRUE))

# Q: find the flights from EWR ORD
# shortest ave.arrival delay

flights %>%
   filter(origin == "EWR", dest == "ORD") %>%
   group_by(carrier, flight) %>%
   summarize(
         ave.delay=mean(arr_delay, na.rm=TRUE)
   ) %>%
   tbl_df() %>%
   slice(which.min(ave.delay))
   
# Q: how many flights arrive ORD for each carrier
flights %>%
   filter(dest == "ORD") %>%
   group_by(carrier) %>%
   summarize(Nflights=n())


flights %>%
   filter(dest == "ORD") %>%
   group_by(carrier) %>%
   summarize(Nflights=n_distinct(flight))
   
# number of distinct plaines for each carrier
# tailnum

flights %>%
   group_by(carrier) %>%
   summarize(Nplanes=n_distinct(tailnum))

# add new columns

flights %>%
  mutate(
     speed=distance/air_time,
     flight=paste(carrier, flight, sep="")
  ) %>%
  select(speed, flight)


# add a new column
# 
flights1 <- flights %>%
  mutate(
     dperiod=cut(dep_time, 
            breaks=c(-0.01, 1200, 1800, 2400),
            labels=c("M", "A", "N"))
  )


flights1 %>% select(dperiod)

# dest ORD
# Q: when should I depart?

flights1 %>%
  filter(dest == "ORD") %>%
  group_by(dperiod) %>%
  summarize(
      delay.med=median(arr_delay, na.rm=TRUE),
      delay.mean=mean(arr_delay, na.rm=TRUE),
      delay.var=var(arr_delay, na.rm=TRUE)
  )






































   




























































































