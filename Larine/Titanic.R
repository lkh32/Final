library(ggplot2)
##I got this dataset from Kaggle: it is a current Competition they have running. They gave me a train and test dataset. 
#The Train data set had x,y,z, the test had everything except for if they survived. The 3rd data set provided was a "gender submission"
#Here, I merge all of the datasets to have one "Titanic" dataset to manipulate. I later read in the data untouched for train and test to also do some ML manipulation 
#But I wanted to look at the data and do some visualization to show concepts from earlier in the course. 
TitanicObject <- function () {
  Titanic1 <- read.csv("train.csv", header = T, sep = ",")
  Titanic2 <- read.csv("test.csv", header = T, sep = ",")
  Titanic2Answers <- read.csv("gender_submission.csv", header = T, sep = ",")
  Titanic2$Survived <- Titanic2Answers$Survived
  Titanic <- rbind(Titanic1, Titanic2)
  return(Titanic)
}

###Data Manipulation 
createAgeGroups <- function(t) {
  t$AgeGroup <- "NA"
  t$AgeGroup[t$Age >= 65] <- 'Elderly'
  t$AgeGroup[t$Age < 65 & t$Age >= 18] <- 'Adult'
  t$AgeGroup[t$Age > 2 & t$Age < 18] <- 'Child'
  t$AgeGroup[t$Age < 2] <- 'Infant'
  return(t)
}

createFamilySize <- function(t) {
  t$FamilySize <- t$SibSp + t$Parch + 1
  return(t)
}

summary(t$Fare)

createFareRange <- function(t) {
  t$FareRange <- '30+'
  t$FareRange[t$Fare < 30 & t$Fare >= 20] <- '20-30'
  t$FareRange[t$Fare < 20 & t$Fare >= 10] <- '10-20'
  t$FareRange[t$Fare < 10] <- '<10'
  return(t)
}

createFirstClass <- function(t) {
  t$FirstClass <- 0
  t$FirstClass[t$Pclass == 1] <- 1
  t$FirstClass <- as.factor(t$FirstClass)
  return(t) 
}


addTitle <- function(t) {
  t$Title <- sapply(as.character(t$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  t$Title <- sub(' ', '', t$Title)
  table(t$Title)
  
  t$Title[t$Title %in% c('Miss', 'Mrs', 'Ms','Mme', 'Mlle')] <- 'Miss'
  t$Title[t$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Rev', 'Col', 'Dr', 'Master', 'Jonkheer')] <- 'Sir'
  t$Title[t$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
  t$Title <- factor(t$Title)
  return(t)
}

###Functions for Analysis 

SurvivalByVariable <- function(t, survived, variable){
  data <- as.data.frame(aggregate(survived ~ variable, data=t , FUN=sum))
  xLabel <- deparse(substitute(variable))
  g <- ggplot(data, aes(x= variable, y = survived, fill = variable)) + geom_bar(stat = "identity") + labs(x=xLabel, y="Number of Survivors", fill = xLabel)
  return(print(g))
}

SurvivalByVariablePercentage <- function(t, survived, variable){
  asPercentage <- as.data.frame(aggregate(survived ~ variable, data=t , FUN=function(x) {sum(x)/length(x)}))
  xLabel <- deparse(substitute(variable))
  g <- ggplot(asPercentage, aes(x= variable, y = survived, fill = variable)) + geom_bar(stat = "identity") + labs(x=xLabel, y="Percentage of Survivors", fill = xLabel)
  return(print(g))
}

SurvivalTwoVariables <- function(t, survived, variable1, variable2){
  data <- as.data.frame(aggregate(survived ~ variable1 + variable2, data=t , FUN=sum))
  xLabel <- deparse(substitute(variable1))
  legendLabel <- deparse(substitute(variable2))
  g <- ggplot(data, aes(x= variable1, y = survived, fill = variable2)) + geom_bar(stat = "identity") + labs(x=xLabel, "Number of Survivors", fill = legendLabel)
  return(print(g))
}

SurvivalTwoVariablesPercentage <- function(t, survived, variable1, variable2){
  asPercentage <- as.data.frame(aggregate(survived ~ variable1 + variable2, data=t , FUN=function(x) {sum(x)/length(x)}))
  xLabel <- deparse(substitute(variable1))
  legendLabel <- deparse(substitute(variable2))
  g <- ggplot(asPercentage, aes(x= variable1, y = survived, fill = variable2)) + geom_bar(stat = "identity", position=position_dodge()) +  labs(x=xLabel, y="Percentage of Survivors", fill = legendLabel)
  return(print(g))
}

###Code 
t <- createAgeGroups(t)
t <- createFamilySize(t)
t <- createFareRange(t)
t <- createFirstClass(t)

SurvivalByVariablePercentage(t, t$Survived, t$Population)
SurvivalByVariablePercentage(t, t$Survived, t$Pclass)
SurvivalByVariable(t, t$Survived, t$AgeGroup)
SurvivalByVariable(t, t$Survived, t$FamilySize)
SurvivalByVariable(t, t$Survived, t$AgeGroup)
SurvivalByVariablePercentage(t, t$Survived, t$AgeGroup)
SurvivalByVariable(t, t$Survived, t$AgeGroup)
SurvivalByVariable(t, t$Survived, t$FareRange)
SurvivalByVariablePercentage(t, t$Survived, t$FareRange)
SurvivalByVariable(t, t$Survived, t$Embarked)
SurvivalByVariablePercentage(t, t$Survived, t$Embarked)
SurvivalTwoVariables(t, t$Survived, t$FirstClass, t$Embarked)
SurvivalTwoVariablesPercentage(t, t$Survived, t$FirstClass, t$Embarked)
SurvivalByVariable(t, t$Survived, t$Title)
SurvivalByVariablePercentage(t, t$Survived, t$Title)

#####p <- ggplot(t, aes(t$Embarked, t$Pclass)) + geom_tile(scale_fill_gradient(low = "white", high = "blue"))
#data <- data.frame(table(survived, variable), Variable = variable, Surviv
#g <- ggplot(data, aes(x = variable, y = survived, fill = variable)) + geom_bar(stat = "identity")

