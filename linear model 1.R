#############################
# Linear Model
#############################

library(ggplot2)
library(gridExtra)
library(car)

bschool <- read.csv("Bschool.csv")

# draw graphs 
# score with all other columns

p <- ggplot(bschool, aes(y=score))
p1 <- p+geom_point(aes(x=size))
p2 <- p+geom_boxplot()
p21 <- p2+aes(x=title)
p22 <- p2+aes(x=gender)
p23 <- p2+aes(x=student)
p24 <- p2+aes(x=factor(year))
p25 <- p2+aes(x=semester)

grid.arrange(p1, p21, p22, p23, p24, p25, ncol=3)

# target: how could remove the effect from title, gender...

# score=beta_0+beta_1*size
# quantitative covariates with score
lm1 <- lm(score~size, data=bschool)
summary(lm1)

# 17.462096   0.181826  96.037   <2e-16 ***
# H_0: beta_0=0
# beta_0=17.46

# -0.009516   0.003716  -2.561   0.0109 * 
# H_0: beta_1=0
# beta_1=-0.0095


# p-value: 0.01088
# H_0: beta_1=0


# score vs student
# score=beta_0+beta_1*I(student=MBA)+
# beta_2*I(student=under)

lm2 <- lm(score~student, data=bschool)
summary(lm2)

# beta_0=17.73
# beta_1=-1.23
# beta_2=-0.99

# student graduate: 
# mean score=beta0
# MBA 
# mean score=beta0+beta1
# under beta0+beta2

# H_0: beta_1=0
# H_0: graduate=MBA
# beta_2=0 graduate=under

# H_0: under=MBA

linearHypothesis(lm2, 
                 "studentMBA=studentUndergraduate")


linearHypothesis(lm2, 
                 "2*studentMBA=studentUndergraduate")


linearHypothesis(lm2,
                 c(
                  "studentMBA=0",
                  "studentUndergraduate=0"
                 ))



# year: qualitative or quantitative
# 1)score~beta0+beta1*year
# 2)score~beta0+beta1*I(year=2003)+beta2*I(year=2004)
# score2004-score2002
# in 1) 2*beta1
# in 2) beta2
# score2003-score2002
# in 1) beta1
# in 2) beta1





lm3 <- lm(score~factor(year), data=bschool)
summary(lm3)

# H0: no difference to treat year as 
# qualitative or quantitative
linearHypothesis(lm3, 
        "2*factor(year)2003=factor(year)2004"
                 )


# score~title
lm4 <- lm(score~title, bschool)
summary(lm4)
# H0: mean score of professor=mean score of 
# associate
# H0: assistant=associate
linearHypothesis(lm4, 
                 "titleAssociate Professor = 0")
linearHypothesis(lm4, 
                 "titleAssistantProfessor = 
                 titleAssociate Professor")


# score=beta0+beta1* I(assistant)+beta2*I(associate)
# beta0=17.5757     0.1511 116.295  < 2e-16
# beta1=-1.0660     0.2869  -3.716 0.000237
# beta2=-0.8209     0.2217  -3.702 0.000250


# full model
lmf <- lm(score~., data=bschool)
summary(lmf)

# how many linear coefficients?
# H0: spring=fall
# H0: male=f

# p-value: 1.497e-10
# H_0 all covariates are not effective
# H_a: some of them are effective

Anova(lmf)
# H0: title is not effective 
# after including g, s, y, s, size

summary(lmf)
# size    pvalue  0.65540
# score=beta0+beta1...+beta8*size
# H0: beta8=0
# Ha: beta8 not equal to 0


# gender  p=0.78242
# score=beta0+...+beta3*I(gender=m)+...
# H0: beta3=0
# Ha: beta3 !=0



















































































































































































































