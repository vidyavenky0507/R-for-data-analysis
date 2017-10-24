##########################
# Linear Model III
# Date: 03/01/2017
##########################

library(car)
bschool <- read.csv("Bschool.csv")

bschool$smallsize <- 1*(bschool$size<=20)

lmf0 <- lm(score~., bschool)
lmf <- lm(score~.+smallsize*size, bschool)

Anova(lmf)
# type II strong heredity

Anova(lmf, type=3)
# type III weak heredity


# goal: 
# score=beta0+beta1*title....
lmf$residuals


##########################
# Generalized Linear Model 
# Date: 03/01/2017
##########################
library(tidyverse)

ceo <- read.table("ceo.txt", header=TRUE)
# change=beta0+beta1*title
# not prefered

ceo %>%
  select(-INDUSTRY) %>%
  gather(Index, Value, -YEAR, -CODE, -CHANGE) %>%
  tbl_df() %>%
  group_by(Index) %>%
  filter(
    Value <=quantile(Value, 0.95, na.rm=TRUE),
    Value >=quantile(Value, 0.05, na.rm=TRUE)
  ) %>%
  ggplot(aes(x=factor(CHANGE), y=Value))+
  geom_boxplot()+
  facet_wrap(~Index, scales="free_y")


# subdata from year 2002
ceo1 <- filter(ceo, YEAR==2002) %>%
  select(-YEAR, -CODE, -INDUSTRY)


## simple analysis
# H0: roa(CHANGE==1)=roa(CHANGE==0)
sapply(
  ceo1[ , 2:15], 
  function(u) t.test(u~ceo1$CHANGE)$p.value
)


# link function
# Prob(change==1)=qnorm(beta0+beta1*Tenture+...+...)

lm1 <- glm(CHANGE~ROE, ceo1, 
           family=binomial(link=probit))
summary(lm1)

pred <- predict(lm1)

ceo1 %>%
  mutate(
    pred=pnorm(pred)
  ) %>%
  ggplot(aes(x=ROE, y=pred))+geom_line()


lm2 <- glm(CHANGE~CONTROL, ceo1, 
           family=binomial(link=logit))

# logit(x)=exp(x)/(1+exp(x))
# Prob(change==1)=logit(beta0+beta1*Tenture+...+...)
# barplot (x=control, y=prob(change==1))


lmf <- glm(CHANGE~., ceo1, 
           family=binomial(link=probit))
Anova(lmf)



lm.aic <- step(lmf, trace=0, k=2)
lm.bic <- step(lmf, trace=0, k=log(nrow(ceo1)))


# prediction: evaluate models
ceo.test <-  filter(ceo, YEAR==2003) %>%
  select(-YEAR, -CODE, -INDUSTRY)

pred <- predict(lmf, ceo.test)
prob <- pnorm(pred)

boxplot(prob~ceo.test$CHANGE)

# need a threshold 
table(ceo.test$CHANGE, prob>0.5)


# some other case, 100 observation, 
# 1 observation with change==1

# AUC-- area under curve





































  








