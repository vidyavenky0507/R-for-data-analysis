
train<- read.csv("train.csv",header = TRUE)
test<- read.csv("test.csv",header = TRUE)

test.Survived<-data.frame(Survived=rep("none",nrow(test)),test[,])
#since survived is in the first column we cannot combine two tables.
test.Survived<-test.Survived[c(2,1,3,4:12)]

#combining both sets
data.combined<-rbind(train,test.Survived)

#datatypes in my table
str(data.combined)
table(data.combined$Survived)
#effect of Pclass on survived
train$Pclass<-as.factor(train$Pclass)

#loads ggplot into temporary mem
library(ggplot2)
#ploting histogram to know to effect of pclass
ggplot(train,aes(x=Pclass,fill=factor(Survived)))+geom_bar(width=0.5)+
  xlab("Pclass")+ylab("Total count")+labs(fill="Survived")

#turn the few of the data at Name variable
head(as.character(train$Name))

#how many unique names are there accross both train and test?
length(unique(as.character(data.combined$Name)))

#we need to find which names are duplicated and store them in dup.names
dup.names<-as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#let's pull the rows from data.combined where the $Name is in the dup.names
data.combined[which(data.combined$Name%in%dup.names),]
#this is to see whether they are really two different people of missed duplicate entries

#let's see what other varialbles have relationaship with the Survived variable
#we will start with Title
library(stringr)
misses<-data.combined[which(str_detect(data.combined$Name,"Miss.")), ]
misses[1:5,]

#same with Mrs title
mrses<-data.combined[which(str_detect(data.combined$Name,"Mrs.")), ]
mrses[1:5,]

#males
males<-data.combined[which(train$Sex=="male"),]
males[1:5,]

extractTitle <- function(Name){
  if(length(grep("Miss.",Name))>0){
    return("Miss.")
  }
  else if(length(grep("Mrs.",Name))>0){
    return("Mrs.")
  }
  else if(length(grep("Mr.",Name))>0){
    return("Mr.")
  }
  else if(length(grep("Master.",Name))>0){
    return("Master.")
  }
  else return("Other")
}

titles<- NULL
for(i in 1:nrow(data.combined)){
  titles<-c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$Title<-as.factor(titles)

#Plot the data title vs pclass vs survived
ggplot(data.combined[1:891,],aes(x=Title,fill=factor(Survived)))+
geom_bar(width = 0.5)+
facet_wrap(~Pclass)+
ggtitle("Pclass")+
xlab("Tittle")+
ylab("TotalCount")+
labs(fill = "Survived")

#let's see the ratio of males to female
table(data.combined$Sex)

#Plot 3-way relationship sex vs pclass vs survived
ggplot(data.combined[1:891,],aes(x=Sex,fill=factor(Survived)))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("TotalCount")+
  labs(fill = "Survived")

#Let's see the age variable over the entire data
ggplot(data.combined[1:891,],aes(x=Age,fill=factor(Survived)))+
  geom_bar(width = 1)+
  facet_wrap(~Sex+Pclass)+
  xlab("Age")+
  ylab("TotalCount")
  
#validate that Master is a proxy for male children
boys<- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

#Let's see the misses part
misses<- data.combined[which(data.combined$Title=="Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived!= "none",],aes(x=Age,fill=factor(Survived)))+
  facet_wrap(~Pclass)+geom_histogram(binwidth=5)+ggtitle("Age of Miss by Pclass")+
  xlab("Age")+ylab("total count")

#misses travelling alone
misses.alone<-misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age<=14.5))

#lets summarize th sibsp variable
summary(data.combined$SibSp)

#can we turn Sibsp into factor
length(unique(data.combined$SibSp))

#turn it into an factor 
data.combined$SibSp<-as.factor(data.combined$SibSp)

#plot the graph with 4-way survival rates vs pclass vs tittle vs sibsp
ggplot(data.combined[1:891,],aes(x=SibSp,fill=factor(Survived)))+
  facet_wrap(~Pclass+Title)+geom_bar(width=1)+
  ggtitle("pclass,title")+
  xlab("Sibsp")+ylab("total count")+
  ylim(0,300)+labs(fill="Survived")

#lets do the same with parch variable
summary(data.combined$Parch)
length(unique(data.combined$Parch))
data.combined$Parch<-as.factor(data.combined$Parch)

#plot the graph with 4 way survival rates vs pclass vs tittle vs parch
ggplot(data.combined[1:891,],aes(x=Parch,fill=factor(Survived)))+
  facet_wrap(~Pclass+Title)+geom_bar(width=1)+
  ggtitle("pclass,title")+
  xlab("Parch")+ylab("total count")+
  ylim(0,300)+labs(fill="Survived")

#we see that family size is an very important feature/variable in deciding 
#the survival rates; we are extracting the family size
temp.sibsp<-c(train$SibSp,test$SibSp)
temp.parch<-c(train$Parch,test$Parch)

data.combined$Family.size<-as.factor(temp.parch+temp.sibsp+1)

#now let's do a ggplot family.size vs survival vs pclass vs title
ggplot(data.combined[1:891,],aes(x=Family.size,fill=factor(Survived)))+
  facet_wrap(~Pclass+Title)+geom_bar(width=1)+
  ggtitle("pclass,title")+
  xlab("Family.size")+ylab("total count")+
  ylim(0,300)+labs(fill="Survived")


#===========================================================================
#third vedio---> Let's see the ticket no..Take a look at structure
#=======================================================================
str(data.combined$Ticket)

#We will start with taking a look at 1st char for each of the string

ticket.first.char<-ifelse(data.combined$Ticket==" "," ",substr(data.combined$Ticket,1,1))
#get the unique first char 
unique(ticket.first.char)

#since the value of the first char are very few we can convert them into factors
data.combined$ticket.first.char<-as.factor(ticket.first.char)

#let's plot ticket.first.char vs survival
ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=factor(Survived)))+
  geom_bar()+
    xlab("ticket.first.char")+ylab("total count")+
  ylim(0,150)+labs(fill="Survived")

#we see that those from P and S ticket have 100% survival rate
#therefore, we investigate more to know the relation between ticket and pclass
ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=factor(Survived)))+
  geom_bar()+facet_wrap(~Pclass)+ggtitle("Pclass")+
  xlab("ticket.first.char")+ylab("total count")+
  ylim(0,150)+labs(fill="Survived")

#pclass vs title vs survival rates vs ticket.first.char
ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=factor(Survived)))+
  facet_wrap(~Pclass+Title)+
  geom_bar()+
  ggtitle("pclass,title")+
  xlab("ticket.first.char")+ylab("total count")+
  ylim(0,200)+labs(fill="Survived")

#Let's look at the fare variable of the data
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#plot only fare variable
ggplot(data.combined,aes(x=Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("combined fare distribution")+
  xlab("Fare")+ylab("total count")+
  ylim(0,20)

#let's see if the fare has any predictive power
ggplot(data.combined[1:891,],aes(x=Fare,fill=factor(Survived)))+
  facet_wrap(~Pclass+Title)+
  geom_bar(width =5)+
  ggtitle("pclass,title")+
  xlab("fare")+ylab("total count")+
  ylim(0,50)+labs(fill="Survived")

#Let's see cabin variable
str(data.combined$Cabin)

#cabin is not a factor , lets make it a factor and display first 100
data.combined$Cabin<-as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#take a look at first character as a factor
cabin.first.char<-as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

#Since we have lot of blanks in the cabin-> we need to replce them with "U"
data.combined[which(data.combined$Cabin==""),"Cabin"]<-"U"
data.combined$Cabin[1:100]
#after replacement again 
cabin.first.char<-as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

#now lets add a column having the cabin first char into the data set
data.combined$cabin.first.char<-cabin.first.char
data.combined$cabin.first.char[1:100]

#high level plot -> cabin.first.char vs survival
ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=factor(Survived)))+
  geom_bar()+
  ggtitle("survivivability by cabin.first.char")+
  xlab("cabin.first.char")+ylab("total count")+
  ylim(0,750)+labs(fill="Survived")

#cabin.first.char vs survival vs pclass 
ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=factor(Survived)))+
  geom_bar()+facet_wrap(~Pclass)+
  ggtitle("survivivability by cabin.first.char")+
  xlab("cabin.first.char")+ylab("total count")+
  ylim(0,500)+labs(fill="Survived")

#cabin.first.char vs survival vs pclass vs title
ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=factor(Survived)))+
  geom_bar()+facet_wrap(~Pclass+Title)+
  ggtitle("survivivability by cabin.first.char")+
  xlab("cabin.first.char")+ylab("total count")+
  ylim(0,500)+labs(fill="Survived")

#what about the people with multiple cabins
data.combined$Cabin.multiple<-as.factor(ifelse(str_detect(data.combined$Cabin," "),"Y","N"))

#plot multiple cabin vs pclass vs title vs survival
ggplot(data.combined[1:891,],aes(x=Cabin.multiple,fill=factor(Survived)))+
  geom_bar()+facet_wrap(~Pclass+Title)+
  ggtitle("pclass,title")+
  xlab("Cabin.multiple")+ylab("total count")+
  ylim(0,500)+labs(fill="Survived")


#Lasty does survivivability depend on embarked variable
str(data.combined$Embarked)
#Since embarrked variable has only 3 variable
ggplot(data.combined[1:891,],aes(x=Embarked,fill=factor(Survived)))+
  geom_bar()+facet_wrap(~Pclass+Title)+
  ggtitle("pclass,title")+
  xlab("Embarked")+ylab("total count")+
  ylim(0,300)+labs(fill="Survived")

# So final conclusion is that the variable that effect the survivability rates too
# is pclass,title,sibsp+parch=family size

#================================================================================
#Forth Vedio - Eploratory modelling
#===============================================================================

library(randomForest)

#train the data pclass & title
rf.train.1<-data.combined[1:891,c("Pclass","Title")]
rf.label1<-as.factor(train$Survived)

set.seed(1234)
rf.1<-randomForest(x=rf.train.1,y=rf.label1,importance=TRUE,ntree = 1000)

rf.1  
varImpPlot(rf.1)

#train second set of data with variables pclass, title & sibsp
rf.train.2<-data.combined[1:891,c("Pclass","Title","SibSp")]

set.seed(1234)
rf.2<-randomForest(x=rf.train.2,y=rf.label1,importance=TRUE,ntree = 1000)

rf.2 
varImpPlot(rf.2)

#train third set of data with variables pclass, title & parch
rf.train.3<-data.combined[1:891,c("Pclass","Title","Parch")]

set.seed(1234)
rf.3<-randomForest(x=rf.train.3,y=rf.label1,importance=TRUE,ntree = 1000)

rf.3
varImpPlot(rf.3)

#train four set of data with variables pclass, title, parch & sibsp
rf.train.4<-data.combined[1:891,c("Pclass","Title","Parch","SibSp")]

set.seed(1234)
rf.4<-randomForest(x=rf.train.4,y=rf.label1,importance=TRUE,ntree = 1000)

rf.4
varImpPlot(rf.4)


#train five set of data with variables pclass, title, family.size
rf.train.5<-data.combined[1:891,c("Pclass","Title","Family.size")]

set.seed(1234)
rf.5<-randomForest(x=rf.train.5,y=rf.label1,importance=TRUE,ntree = 1000)

rf.5
varImpPlot(rf.5)


#train six set of data with variables pclass, title, family.size, sibsp
rf.train.6<-data.combined[1:891,c("Pclass","Title","Family.size", "SibSp")]

set.seed(1234)
rf.6<-randomForest(x=rf.train.6,y=rf.label1,importance=TRUE,ntree = 1000)

rf.6
varImpPlot(rf.6)

#train seven set of data with variables pclass, title, family.size, parch
rf.train.7<-data.combined[1:891,c("Pclass","Title","Family.size", "Parch")]

set.seed(1234)
rf.7<-randomForest(x=rf.train.7,y=rf.label1,importance=TRUE,ntree = 1000)

rf.7
varImpPlot(rf.7)

#train five set has the best accuraracy 
rf.5


#=========================================================================
#vedio #5 - Exploratory model 2
#=========================================================================

#subset our test records and features
test.submit.df<-data.combined[892:1309,c("Pclass","Title","Family.size")]

#Making predictions
rf.5.preds<-predict(rf.5,test.submit.df)
table(rf.5.preds)

# Let's look at the cross validation caret package and see if we get any accurate
#estimate
library(caret)
library(doSNOW)

#Leverage caret to create 100 total folds, but ensure that the ratio of
#those survived and perished in each fold matches the overall training set
#This is known as Stratified cross validation--> provides better results
set.seed(2348)
cv.10.folds<-createMultiFolds(rf.label1,k=10,times = 10)

#Just to be clear , Let's check the stratification
table(rf.label1)
342/549

#thats around 62.3%
table(rf.label1[cv.10.folds[[33]]])
308/494
#so we recheched the stratification and its correct
#set up caret's train control object per above
ctrl.1<-trainControl(method = "repeatedcv",number = 10,repeats = 10,index = cv.10.folds)

#Set up doSnow package for multicore training, This is helpful as we are going 
#training lot of tress
cl<-makeCluster(6,type = "SOCK")
registerDoSNOW(cl)

#Set seed for reproducability and train 
set.seed(34324)
rf.5.cv.1<-train(x=rf.train.5,y=rf.label1,method ="rf",tuneLength = 3,ntree=1000,trControl=ctrl.1)

#Stop the cluster
stopCluster(cl)

#check the results
rf.5.cv.1

#try the same for 5 fold
#Now let's try 3 fold
set.seed(37596)
cv.3.folds<-createMultiFolds(rf.label1,k=3,times=10)

ctrl.3<-trainControl(method = "repeatedcv",number = 3,repeats = 10,index = cv.3.folds)

cl<-makeCluster(6,type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3<-train(x=rf.train.5,y=rf.label1,method ="rf",tuneLength = 3,ntree=64,trControl=ctrl.3)

#Shutdown Cluster
stopCluster(cl)

#check results
rf.5.cv.3


#=====================================================================
# Exploratory modelling 2
#====================================================================

#Let's use single decision tree to better understand what's ging on with our features
#Obviously random forests are far more powerful than single trees,
#but trees are easier to understand

#Let's install rpart and rpart plot
install.packages("rpart")
install.packages("rpart.plot")

#invoke them into temporary mem
library(rpart)
library(rpart.plot)

#we will use 3-fold CV repeated 10 times
#Let's create a utility function for performing 3-fold CV repeated 10 times

rpart.cv<-function(seed,training,labels,ctrl){
  
  cl<-makeCluster(6,type = "SOCK")
  registerDoSNOW(cl)
  set.seed(seed)
  
  #Leverage formula interface for training
  
  rpart.cv<-train(x=training,y=labels,method = "rpart",tuneLength = 30,trControl=ctrl)
  
  #Shutdown the cluster
  stopCluster(cl)
  return(rpart.cv)
}


#Grab Features
features<-c("Pclass","Title","Family.size")
rpart.train.1<-data.combined[1:891,features]

#run CV and Check results
rpart.1.cv.1<-rpart.cv(94622,rpart.train.1,rf.label1,ctrl.3)
rpart.1.cv.1

#Plot
prp(rpart.1.cv.1$finalModel,type = 0,extra = 1,under = TRUE)

#We have little analysis here of the tree, I have written in the book

#we see that both rf and rpart confirm that title is important.
table(data.combined$Title)

library(stringr)
#Parse out last name and title
data.combined[1:25,"Name"]
name.splits<-str_split(data.combined$Name,",")
name.splits[1]

last.names<-sapply(name.splits,"[",1)

last.names[1:10]

#Add last names to dataframe in case we find it useful later
data.combined$Last.name<-last.names

#Let's do the same for titles
name.splits<-str_split(sapply(name.splits,"[",2)," ")
titles<-sapply(name.splits,"[",2)
unique(titles)

#Let's see what's up with "the" title?
data.combined[which(titles=="the"),]

#Re-map the titles again
titles[titles %in% c("Dona.","the")]<-"Lady."
titles[titles %in% c("Ms.","Mlle.")]<-"Miss."
titles[titles %in% c("Jonkheer.","Don.")]<-"Sir."
titles[titles %in% c("Col.","Capt.","Major.")]<-"Officer."
titles[titles == "Mme."]<-"Mrs."
table(titles)

#Make tiltle a factor
data.combined$New.title<-as.factor(titles)

#visualize new version of title
ggplot(data.combined[1:891,],aes(x=New.title,fill=factor(Survived)))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("survived rates for new.title by Pclass")

#Collapse titles based on visual analysis
indexes<-which(data.combined$New.title=="Lady.")
data.combined$New.title[indexes]<-"Mrs."

indexes<-which(data.combined$New.title=="Dr."|
                 data.combined$New.title=="Rev."|
                 data.combined$New.title=="Sir."|
                 data.combined$New.title=="Officer.")
data.combined$New.title[indexes]<-"Mr."

#Visualize it
ggplot(data.combined[1:891,],aes(x=New.title,fill=factor(Survived)))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("survived rates for new.title by Pclass")

#Grab Features
features<-c("Pclass","New.title","Family.size")
rpart.train.2<-data.combined[1:891,features]

#run CV and Check results
rpart.2.cv.1<-rpart.cv(94622,rpart.train.2,rf.label1,ctrl.3)
rpart.2.cv.1

#Plot
prp(rpart.2.cv.1$finalModel,type = 0,extra = 1,under = TRUE)

#Dive in the 1st Class into "Mr."
indexes.first.mr<-which(data.combined$New.title=="Mr." & data.combined$Pclass=="1")
first.mr.df<-data.combined[indexes.first.mr,]
summary(first.mr.df)

#one female????
first.mr.df[first.mr.df$Sex=="female",]

#she is a Dr. --> our asumption was incorrect
#update the new.title feature
indexes<-which(data.combined$New.title=="Mr." & data.combined$Sex == "female")
data.combined$New.title[indexes]<-"Mrs."

#Any other slip-ups
length(which(data.combined$Sex == "female" & 
               (data.combined$New.title=="Master."|
                  data.combined$New.title=="Mr.")))

#Refresh the data frame
indexes.first.mr<-which(data.combined$New.title=="Mr." & data.combined$Pclass=="1")
first.mr.df<-data.combined[indexes.first.mr,]

#Let's look at the surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived=="1",])
View(first.mr.df[first.mr.df$Survived=="1",])

#Take a look at some high fares
indexes<-which(data.combined$Ticket== "PC 17755"|
                 data.combined$Ticket== "PC 17611"|
                 data.combined$Ticket== "113760")
View(data.combined[indexes,])

#we see that all with high fare survived so lets plot
ggplot(first.mr.df,aes(x=Fare,fill=factor(Survived)))+
  geom_density(alpha=0.5)+
  ggtitle("1st class 'Mr.'Survival rates by Fare")

#Engineer feaature ebased on all th passengers with same ticket
ticket.party.size<-rep(0,nrow(data.combined))
avg.fare<-rep(0.0,nrow(data.combined))
tickets<-unique(data.combined$Ticket)
length(tickets)

for(i in 1:length(tickets)){
  current.ticket<-tickets[i]
  party.indexes<-which(data.combined$Ticket==current.ticket)
  current.avg.fare<-data.combined[party.indexes[i],"Fare"]/length(party.indexes)
  
  for(k in 1:length(party.indexes)){
    ticket.party.size[party.indexes[k]]<-length(party.indexes)
    avg.fare[party.indexes[k]]<-current.avg.fare
  
  }
}

data.combined$Ticket.party.size<-ticket.party.size
data.combined$Avg.fare<-avg.fare

#refresh the 1st class Mr. dataframe
first.mr.df<-data.combined[indexes.first.mr,]
summary(first.mr.df)

#Visualise
ggplot(first.mr.df[first.mr.df$Survived != "none",],aes(x=Ticket.party.size,fill=factor(Survived)))+
  geom_density(alpha=0.5)+
  ggtitle("1st class 'Mr.'Survival rates by Ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "none",],aes(x=Avg.fare,fill=factor(Survived)))+
  geom_density(alpha=0.5)+
  ggtitle("1st class 'Mr.'Survival rates by Avg.fare")

#Hypothesis - ticket.party.size might be related to avg fare
summary(data.combined$Avg.fare)

#One value is missing? take a look
data.combined[is.na(data.combined$Avg.fare),]

#get records for similar passegers and summarise
indexes <-with(data.combined,which(pclass=="3" & tittle == "Mr" & familr.size == 1 & ticket != "3701"))
similar.na.passegers<-data.combined[indexes,]
summary(similar.na.passegers)
data.combined[is.na(Avg.fare),"Avg.fare"]<-7.840

preproc.data.combined<-data.combined[,c("ticket,partysize","Avg.fare")]
preproc<-preProcess(preproc.data.combined,method=c("center","scale"))
postproc.data.combined<-predict(preProc,preproc.data.combined)

#Hypothesis refuted 
cor(postproc.data.combined$ticket.party.size,data.combined$avg.fare)
#just 1st class al up
cor(postproc.data.combined$ticket.party.size[indexes],postproc.data.combined$avg.fare[indexes])

features<-c("Pclass","New.title","family.size","Ticket.party.size","Avg.fare")
rpart.train.3<-data.combined[1:891,features]

#run CV
rpart.3.cv.1<-rpart.cv(94622,rpart.train.3,rf.label1,ctrl.3)
#plot
prp(rpart.3.cv.1$finalModel,type=0,extra=1,under=TRUE)

#testing
test.submit.df<-data.combined[892:1309,features]
rpart.3.preds<-predict(rpart.3.cv.1$finalModel,test.submit.df,type="class")
table(rpart.3.preds)
