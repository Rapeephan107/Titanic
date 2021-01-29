library(funModeling) #df_status
library(tidyverse) #selection data
library(Hmisc) #describe()
library(psych)
library(dplyr)#create range of data


#read data
passengers <- read.csv("Lo2train.csv")

#check data status
data_status <- df_status(passengers)
write.table(data_status, file = "data_status.csv", sep = ",", row.names = FALSE) 


#remove NA (891 -> 714 )
passengers <- passengers[!(is.na(passengers$Age)),]

#Create a table of descriptive statistics
select_data <- passengers %>% select(Age, Survived, Pclass, SibSp, Parch, Fare)
descriptive <- describe(select_data)
write.table(descriptive, file = "descriptive.csv", sep = ",") 

#create Pclass graph
PclassTable <- table(passengers$Pclass,passengers$Survived)
colnames(PclassTable)<- c("Died","Survived") #rename
rownames(PclassTable)<- c("Upper","Middle","Lower") #remane
PclassTable <- prop.table(PclassTable, 1)
PclassTable <- t(PclassTable)
par(mar = c(5,5,5,5))
barplot(PclassTable, xlab= "Socio-economic status", ylab = "Proportion", col = 2:3,
        legend =  rownames(PclassTable) ,args.legend = list(x = "topleft")
        , main = "Socio-economic Status and Proportion of Survival " )


#create Gender graph
GenderTable <- table(passengers$Sex,passengers$Survived)
colnames(GenderTable)<- c("Died","Survived") #rename
GenderTable <- prop.table(GenderTable, 1)
GenderTable <- t(GenderTable)
barplot(GenderTable, xlab= "Gender  ", ylab = "Proportion", col = 2:3, 
        legend =  rownames(GenderTable) ,args.legend = list(x = "topleft")
        ,main = "Genders and Proportion of Survival")

#create sibsp graph
SibSptable <- table(passengers$SibSp,passengers$Survived)
colnames(SibSptable)<- c("Died","Survived") #rename
SibSptable <- prop.table(SibSptable, 1)
SibSptable <- t(SibSptable)
par(mar = c(6,6,6,6))
barplot(SibSptable, xlab= "Number of Sibling and Spouse (people)",
        ylab = "Proportion", col = 2:3, legend =  rownames(SibSptable)
        ,main = "Number of Sibling and Spouse and Proportion of Survival")

#create ParCh graph
ParChtable <- table(passengers$Parch,passengers$Survived)
colnames(ParChtable)<- c("Died","Survived") #rename
ParChtable <- prop.table(ParChtable, 1)
ParChtable <- t(ParChtable)
barplot(ParChtable, xlab= "Number of Parent and Child (people)", 
        ylab = "Proportion", col = 2:3, legend =  rownames(ParChtable)
        , main = "Number of Parent and Child and Proportion of Survival ")

#create Embark graph
Emb <- subset(passengers, passengers$Embarked!= '') #remove blank
Embarktable <- table(Emb$Embarked,Emb$Survived)
colnames(Embarktable)<- c("Died","Survived") #rename
rownames(Embarktable)<- c("Cherbourg","Queenstown", "Southampton")
Embarktable <- prop.table(Embarktable, 1)
Embarktable <- t(Embarktable)
barplot(Embarktable, xlab= "Port of Embarkation", ylab = "Proportion", 
        col = 2:3, legend =  rownames(Embarktable),
        main = "Port of Embarkation and Port of Embarkation")

#create Age graph
hist(passengers$Age, main = "Histogram of Age", xlab = "Age (years)")
AgeGroup <- passengers %>% mutate(Agegroup = case_when(Age >= 61   ~ '>61',
                                                       Age >= 51  & Age <= 60 ~ '51 - 60',     
                                                       Age >= 41  & Age <= 50 ~ '41 - 50',
                                                       Age >= 31  & Age <= 40 ~ '31 - 40',
                                                       Age >= 21  & Age <= 30 ~ '21 - 30',
                                                       Age >= 11  & Age <= 20 ~ '11 - 20',
                                                       Age >= 0  & Age <= 10 ~ '0 - 10'))

AgeTable <- table(AgeGroup$Agegroup,AgeGroup$Survived)
colnames(AgeTable)<- c("Died","Survived") #rename
AgeTable <- prop.table(AgeTable, 1)
AgeTable <- t(AgeTable)
AgeTable <- AgeTable[,c(2,3,4,5,6,1)] #swap columns
par(mar = c(5,5,5,5))
barplot(AgeTable, xlab= "Age range (years)", ylab = "Proportion",
        col = 2:3, legend =  rownames(AgeTable), main = "Age and Proportion of Survival")
#create Passenger fare graph
hist(people$Fare, main = "Histogram of Fare", xlab= "Fare (GBP)")
FareRange <- passengers %>% mutate(Farerange = case_when(
        Fare >= 101   ~ '> 101',
        Fare >= 51  & Fare <= 100 ~ '51 - 100',
        Fare >= 0  & Fare <= 50 ~ '0 - 50'))
FareRange <- table(FareRange$Farerange,FareRange$Survived)
colnames(FareRange)<- c("Died","Survived") #rename
FareRange <- prop.table(FareRange, 1)
FareRange <-t(FareRange)
FareRange <- FareRange[,c(2,3,1)] #swap columns
barplot(FareRange, xlab= "Fare range (GBP)", ylab = "Proportion", 
        col = 2:3, legend =  rownames(FareRange),
        main = "Fare and Proportion of Survival")

