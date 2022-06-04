library(naivebayes)
library(ggplot2)
library(lattice)
library(caret)
library(Formula)
library(plotrix)
library(TeachingDemos)
library(plotmo)
library(earth)
library(mlbench)

data<-read.csv("C:\\Users\\keert\\OneDrive\\Documents\\clg notes\\sem 6\\pt2\\pt2 class\\empsatind.csv")
print(data)

#data review
dim(data)
str(data)
length(data)
head(data)
head(data,n=10)
tail(data)
tail(data,n=10)
data[3:6,]
data[5,]
data[,c(10,11,12,13)]
data[,6]

#data preprocessing
summary(data)

data$Dept=as.factor(data$Dept)
data$location =as.factor(data$location )
data$education=as.factor(data$education)
data$recruitment_type=as.factor(data$recruitment_type)
data$satisfied =as.factor(data$satisfied)

summary(data)

data<-data[,-c(1)]
data


#visulaization
barplot(table(data$satisfied),main="Employee Satisfied",
        col=c("pink","light blue"))

x1<-data$age
hist(x1, main = "Age of Employees",xlab="Age", ylab="No.of Employees",
     col = c("Violet","light green"))

x2<- data$Dept
barplot(table(x2),
        main="Department", ylim=c(0,120),col="pink")

x3<- data$recruitment_type
barplot(table(x3),
        main="Recruitment type",xlab="Type",ylab="No.of.Employees",
        ylim=c(0,140),col=c("light blue","pink","violet","light green"))


#attribute importance 
att_imp<-glm(satisfied~.,data,family="binomial")
attribute_imp<-varImp(att_imp)
attribute_imp


#sampling of datasets
set.seed(1)
ind<-sample(2,nrow(data),replace= TRUE,prob=c(0.9,0.1))
trainData <- data[ind==1,]
testData <- data[ind==2,]

print(trainData)
print(testData)

#naive_bayes_model
naive_model <- naive_bayes(satisfied ~ age+Dept+location+education+recruitment_type+job_level+
                             rating+onsite+ awards+certifications+ salary, 
                           data=trainData, type="C-classification")
print(naive_model)
plot(naive_model)


#prediction on test data
testData1 = testData[,-c(12)]
testData1
testPred <- predict(naive_model, newdata = testData1)
print(testPred)
print(testData$satisfied)

#Accuracy
confusionMatrix(testData$satisfied,testPred)

#manual Prediction
a<-data.frame(age=25,Dept="HR",location= "Suburb", education="UG",
              recruitment_type ="On-Campus", job_level= 2, rating= 6, onsite= 4,
              awards=3, certifications= 2, salary = 23098)
result <- predict(naive_model,a)
print(result)



