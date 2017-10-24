##########################
# Linear Model II
# Date: 02/22/2017
##########################

library(car)
bschool <- read.csv("Bschool.csv")

lmf <- lm(score~., bschool)
summary(lmf)

Anova(lmf)

# F=
# contribution from each variable/noise can not be explained
# Type II anova: additional contribution added by
# each variable

anova(lmf)

# sum sq same for size in Type II and Type I
# sequential 

lmf1 <- lm(score~size+semester+year+student+gender+title,
           data=bschool)

anova(lmf1)


# interaction in the model

bschool$smallsize <- 1*(bschool$size <= 20)
library(ggplot2)
p <- ggplot(bschool, aes(x=size, y=score))
p+geom_point(aes(color=factor(smallsize)))+
  geom_smooth(method="lm", 
              aes(color=factor(smallsize)))


lm1 <- lm(score~size*smallsize, bschool)
summary(lm1)

# score=beta0+beta1*size+beta2*I(smallsize=1)+
# beta3*size*I(smallsize=1)
# intercept and slope for red line?
# beta0=16.88, beta1=-0.0007
# blue line?
# beta0+beta2, beta1+beta3

# H0: same slope
# p-value=0.001708
Anova(lm1)
Anova(lm1, type=3)

# Two principles
# strong heredity Type III
# weak heredity Type II

lmf2 <- lm(score~.+size*smallsize, bschool)
summary(lmf2)
Anova(lmf2)

# AIC and BIC
lm.aic <- step(lmf2, trace=FALSE)
summary(lm.aic)


lm.bic <- step(lmf2, trace=FALSE, 
               k=log(nrow(bschool)))
summary(lm.bic)

# normally: bic gives small model
# aic gives large model

# model diagnostic
plot(lm.bic, which=1)
plot(lm.bic, which=2)
par(mfrow=c(2,2))
plot(lm.bic, which=1:4)

# prediction
newdata <- data.frame(
     title="Associate Professor", 
     gender="m",
     student="Graduate",
     year=2005,
     semester="spring",
     size=3,
     smallsize=1
)

predict(lm.bic, newdata)

# score=beta0+beta1*......

bschool$y <- qnorm(bschool$score/20.0001)


lmf3 <- lm(y~size*smallsize, bschool)
newpred <- predict(lmf3, newdata)

pnorm(newpred)*20.0001


# student*gender
ggplot(bschool, aes(x=student, y=score))+
  geom_boxplot()+
  facet_grid(.~gender)

# interaction: student*gender significant or not

lm5 <- lm(score~gender*student, bschool)
Anova(lm5)
summary(lm5)

# score=beta0+beta1*I(gender=m)
# +beta2*I(student=MBA)
# +beta3*I(student=Under)
# +beta4*I(student=MBA)I(gender=m)
# +beta5*I(student=under)I(gender=m)
# average score f*MBA?
# beta0+beta2
# average score m*graduate?























































































































































