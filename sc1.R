
setwd('G:/research work/sem 8')
print(getwd())

HRdata <- read.csv("IBMHrData.csv")
names(HRdata)

colnames(HRdata)[1] <- "Age" # Renaming the column
names(HRdata)

library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
set.seed(12345)
inTrain <- createDataPartition(HRdata$Attrition,p=0.75,list = FALSE)
Training <- HRdata[inTrain,]
Testing <- HRdata[-inTrain,]

str(Training)

library(ggplot2)
ggplot(Training,aes(x=as.factor(Attrition),y=(..count..)/sum(..count..)*100,fill=Attrition))+geom_bar(width = 0.30)+scale_fill_brewer(palette="Set2")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=1.5)+labs(title = "Attrition Frequency", y = "Percent", x = "Attrition")+scale_y_continuous()+theme(plot.title = element_text(hjust = 0.5))
prop.table(table(Training$Attrition)) #Percentage of Attrition

library(ggplot2)
library(grid)
library(gridExtra)
agePlot <- ggplot(Training,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)+scale_fill_brewer(palette="Set2")+labs(title = "Age Vs Attrition", y = "Density", x = "Age")+theme(plot.title = element_text(hjust = 0.5))
travelPlot <- ggplot(Training,aes(BusinessTravel,fill=Attrition))+geom_bar(width=0.25)+scale_fill_brewer(palette="Accent")+labs(title = "Business Travel Vs Attrition", y = "Percentage", x = "Business Travel")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=1)+theme(plot.title = element_text(hjust = 0.5))
ratePlot <- ggplot(Training,aes(DailyRate,Attrition))+geom_point(size=4,alpha = 0.05)+scale_fill_brewer(palette="Set2")+labs(title = "Daily Rate Vs Attrition", y = "Attrition", x = "Daily Rate")+theme(plot.title = element_text(hjust = 0.5))
depPlot <- ggplot(Training,aes(Department,fill = Attrition))+geom_bar(width =0.25)+scale_fill_brewer(palette="Accent")+labs(title = "Department Vs Attrition", y = "Frequency", x = "Department")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=1)+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(agePlot,travelPlot,ratePlot,depPlot,ncol=2,top = "Fig 1")

distPlot <- ggplot(Training,aes(DistanceFromHome,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+labs(title = "Distance from Home Vs Attrition", y = "Frequency", x = "Distance from Home")+theme(plot.title = element_text(hjust = 0.5))
eduPlot <- ggplot(Training,aes(Education,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+labs(title = "Education Vs Attrition", y = "Frequency", x = "Education")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=1)+theme(plot.title = element_text(hjust = 0.5))
edufieldPlot <- ggplot(Training,aes(EducationField,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+labs(title = "Education Field Vs Attrition", y = "Frequency", x = "Educational Field")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=0.85)+theme(plot.title = element_text(hjust = 0.5))
envPlot <- ggplot(Training,aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+labs(title = "Environment Satisfaction Vs Attrition", y = "Frequency", x = "Environment Satisfaction")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))
genPlot <- ggplot(Training,aes(Gender,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+labs(title = "Gender Vs Attrition", y = "Frequency", x = "Gender")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)
grid.arrange(distPlot,eduPlot,edufieldPlot,envPlot,genPlot,ncol=2,top = "Fig 2")

hourlyPlot <- ggplot(Training,aes(HourlyRate,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Set3")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Hourly Rates Vs Attrition", y = "Frequency", x = "Hourly Rate")+theme(plot.title = element_text(hjust = 0.5))
jobInvPlot <-  ggplot(Training,aes(JobInvolvement,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Set2")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Job Involvement Vs Attrition", y = "Frequency", x = "Job Involvement")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-.75)+theme(plot.title = element_text(hjust = 0.5))
jobLevelPlot <-   ggplot(Training,aes(JobLevel,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Set3")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Job Level Vs Attrition", y = "Frequency", x = "Job Level")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))
jobSatPlot <-   ggplot(Training,aes(JobSatisfaction,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Job Satisfaction Vs Attrition", y = "Frequency", x = "Job Satisfaction")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(hourlyPlot,jobInvPlot,jobLevelPlot,jobSatPlot,ncol=2,top = "Fig 3")

#marPlot <- 
  ggplot(Training,aes(MaritalStatus,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Oranges")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Marital Status Vs Attrition", y = "Frequency", x = "Marital Status")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))+facet_grid(~Gender)
#monthlyIncPlot <- 
  ggplot(Training,aes(MonthlyIncome,fill=Attrition))+facet_grid(~Gender)+geom_density()+scale_fill_brewer(palette="Purples")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Monthly Income Vs Attrition", y = "Frequency", x = "Environment Satisfaction")+theme(plot.title = element_text(hjust = 0.5))
monthlyRatePlot <- ggplot(Training,aes(MonthlyRate,fill=Attrition))+geom_density()+scale_fill_brewer(palette="Pastel1")+theme(plot.title = element_text(hjust = 0.5))
numCompPlot <- ggplot(Training,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Accent")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Number of Companies worked previously Vs Attrition", y = "Frequency", x = "Number of Companies Worked Previously")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(marPlot,monthlyIncPlot,monthlyRatePlot,numCompPlot,ncol=2,top = "Fig 4")

overTimePlot <- ggplot(Training,aes(OverTime,fill=Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Overtime Involvement Vs Attrition", y = "Frequency", x = "Overtime Involvement")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))
hikePlot <- ggplot(Training,aes(PercentSalaryHike,Attrition))+geom_point(size=4,alpha = 0.1)+scale_fill_brewer(palette="Pastel1")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Salary Hike Vs Attrition", y = "Attrition", x = "% Salary Hike")+theme(plot.title = element_text(hjust = 0.5))
perfPlot <- ggplot(Training,aes(PerformanceRating,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Paired")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Performance Rating Vs Attrition", y = "Frequency", x = "Performance Rating")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))
RelSatPlot <-   ggplot(Training,aes(RelationshipSatisfaction,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Paired")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Relationship Satisfaction Vs Attrition", y = "Frequency", x = "Relationship Satisfaction")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(overTimePlot,hikePlot,perfPlot,RelSatPlot,ncol=2,top = "Fig 5")

#StockPlot <- 
  ggplot(Training,aes(StockOptionLevel,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Set3")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Stock Option Vs Attrition", y = "Frequency", x = "Stock Options")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-1)+theme(plot.title = element_text(hjust = 0.5))
#workingYearsPlot <- 
  ggplot(Training,aes(TotalWorkingYears,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Set1")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Working Years Vs Attrition", y = "Frequency", x = "Total Working Years")+theme(plot.title = element_text(hjust = 0.5))
#TrainTimesPlot <- 
  ggplot(Training,aes(TrainingTimesLastYear,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Set2")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Training Duration(Previous Year) Vs Attrition", y = "Frequency", x = "Training Duration")+theme(plot.title = element_text(hjust = 0.5))
#WLBPlot <- 
  ggplot(Training,aes(WorkLifeBalance,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Paired")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Work Life Balance Vs Attrition", y = "Frequency", x = "Work Life Balance")+geom_text(stat='count', aes(label=scales::percent((..count..)/sum(..count..))), vjust=-0.5)+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(StockPlot,workingYearsPlot,TrainTimesPlot,WLBPlot,ncol=2,top = "Fig 6")

#YearAtComPlot <- 
ggplot(Training,aes(YearsAtCompany,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Years at Company Vs Attrition", y = "Frequency", x = "Years At Company")+theme(plot.title = element_text(hjust = 0.5))
#YearInCurrPlot <- 
  ggplot(Training,aes(YearsInCurrentRole,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Years in Present Role Vs Attrition", y = "Frequency", x = "Years")+theme(plot.title = element_text(hjust = 0.5))
#YearsSinceProm <- 
  ggplot(Training,aes(YearsSinceLastPromotion,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Pastel1")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Years Since Recent Promotion Vs Attrition", y = "Frequency", x = "Years")+theme(plot.title = element_text(hjust = 0.5))
#YearsCurrManPlot <- 
  ggplot(Training,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()+scale_fill_brewer(palette="Paired")+theme(plot.title = element_text(hjust = 0.5))+labs(title = "Years With Current Manager Vs Attrition", y = "Frequency", x = "Years")+theme(plot.title = element_text(hjust = 0.5))
grid.arrange(YearAtComPlot,YearInCurrPlot,YearsSinceProm,YearsCurrManPlot,ncol=2,top = "Fig 7")+scale_fill_brewer(palette="Pastel1")+theme(plot.title = element_text(hjust = 0.5))

#Feature Engineering Starts here

Training1_os <- Training

Training1_os$TenurePerJob <- ifelse(Training1_os$NumCompaniesWorked!=0, Training1_os$TotalWorkingYears/Training1_os$NumCompaniesWorked,0)
Training1_os$YearWithoutChange <- Training1_os$YearsInCurrentRole - Training1_os$YearsSinceLastPromotion
Training1_os$YearsWithoutChange2 <- Training1_os$TotalWorkingYears - Training1_os$YearsSinceLastPromotion

tenurePlot <- ggplot(Training1_os,aes(TenurePerJob))+geom_density()+facet_grid(~Attrition)
changePlot <- ggplot(Training1_os,aes(YearWithoutChange))+geom_density()+facet_grid(~Attrition)
change2Plot <- ggplot(Training1_os,aes(YearsWithoutChange2))+geom_density()+facet_grid(~Attrition)
grid.arrange(tenurePlot,changePlot,change2Plot,ncol=2,top = "Fig 8")

Med_HR <- median(Training1_os[Training1_os$Department == 'Human Resources',]$MonthlyIncome)
Med_RnD <- median(Training1_os[Training1_os$Department == 'Research & Development',]$MonthlyIncome)
Med_Sales <- median(Training1_os[Training1_os$Department == 'Sales',]$MonthlyIncome)


Med_LabTech <- median(Training1_os[Training1_os$JobRole == 'Laboratory Technician',]$MonthlyIncome)

TrainLabTech <- Training1_os[Training1_os$JobRole == 'Laboratory Technician',]
TrainLabTech$comparole <- TrainLabTech$MonthlyIncome/Med_LabTech

Med_overall <- median(Training1_os$MonthlyIncome)

Training1_os$CompaRatioDep <- ifelse(Training1_os$Department == 'Human Resources',Training1_os$MonthlyIncome/Med_HR,ifelse(Training1_os$Department=='Research & Development',Training1_os$MonthlyIncome/Med_RnD,Training1_os$MonthlyIncome/Med_Sales))

Training1_os$CompaRatioOverall <- Training1_os$MonthlyIncome/Med_overall

Training1_os$CompaOverallGroup <- ifelse(Training1_os$CompaRatioOverall>4,4,ifelse(Training1_os$CompaRatioOverall>3,3,ifelse(Training1_os$CompaRatioOverall>2,2,ifelse(Training1_os$CompaRatioOverall>1,1,ifelse(Training1_os$CompaRatioOverall>0.5,0.5,0)))))

Training1_os$CompaDepGroup <- ifelse(Training1_os$CompaRatioDep>4,4,ifelse(Training1_os$CompaRatioDep>3,3,ifelse(Training1_os$CompaRatioDep>2,2,ifelse(Training1_os$CompaRatioDep>1,1,ifelse(Training1_os$CompaRatioDep>0.5,0.5,0)))))


CompaOverallPlot <- ggplot(Training1_os,aes(CompaRatioOverall))+geom_density()+facet_grid(~Attrition)
CompaDepPlot <- ggplot(Training1_os,aes(CompaRatioDep))+geom_density()+facet_grid(~Attrition)
grid.arrange(CompaOverallPlot,CompaDepPlot,ncol=2,top = "Fig 9")


# Adding the variables for Testing Set

Testing$TenurePerJob <- ifelse(Testing$NumCompaniesWorked!=0, Testing$TotalWorkingYears/Testing$NumCompaniesWorked,0)
Testing$YearWithoutChange <- Testing$YearsInCurrentRole - Testing$YearsSinceLastPromotion
Testing$YearsWithoutChange2 <- Testing$TotalWorkingYears - Testing$YearsSinceLastPromotion


Testing$CompaRatioDep <- ifelse(Testing$Department == 'Human Resources',Testing$MonthlyIncome/Med_HR,ifelse(Testing$Department=='Research & Development',Testing$MonthlyIncome/Med_RnD,Testing$MonthlyIncome/Med_Sales))

Testing$CompaRatioOverall <- Testing$MonthlyIncome/Med_overall

Testing$CompaOverallGroup <- ifelse(Testing$CompaRatioOverall>4,4,ifelse(Testing$CompaRatioOverall>3,3,ifelse(Testing$CompaRatioOverall>2,2,ifelse(Testing$CompaRatioOverall>1,1,ifelse(Testing$CompaRatioOverall>0.5,0.5,0)))))

Testing$CompaDepGroup <- ifelse(Testing$CompaRatioDep>4,4,ifelse(Testing$CompaRatioDep>3,3,ifelse(Testing$CompaRatioDep>2,2,ifelse(Testing$CompaRatioDep>1,1,ifelse(Testing$CompaRatioDep>0.5,0.5,0)))))

#Testing$AvgSatis <- with(Testing,(EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+RelationshipSatisfaction+WorkLifeBalance)/4)

Training1_os$AgeGroup <- with(Training1_os,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1)))))))) #Creating Age Groups

Training1_os$DistanceGroup <- with(Training1_os,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1)))))) #Creating Distance Groups

Training1_os$YearsWithManagerGroup <- with(Training1_os,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1))))) #Creating YearsWithManager Groups


Training1_os$TenureGroup <- with(Training1_os,ifelse(TenurePerJob>35,9,ifelse(TenurePerJob>30,8,ifelse(TenurePerJob>25,7,ifelse(TenurePerJob>20,6,ifelse(TenurePerJob>15,5,ifelse(TenurePerJob>10,4,ifelse(TenurePerJob>5,3,ifelse(TenurePerJob>2,2,1))))))))) #Creating Tenure Per Job groups

Training1_os$Change2Group <- with(Training1_os,ifelse(YearsWithoutChange2>10,3,ifelse(YearsWithoutChange2>5,2,1))) #Creating Years Without Change2

Training1_os$Change1Group <- with(Training1_os,ifelse(YearWithoutChange>2.5,3,ifelse(YearWithoutChange>-2.5,2,1))) #Creating Years Without Change 1

#Training1_os$AvgSatisGroup <- with(Training1_os,ifelse(AvgSatis<2.5,1,2)) # Create Average Satisfaction Groups

Training1_os$WorkYearGroup <- with(Training1_os,ifelse(TotalWorkingYears>35,9,ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,ifelse(TotalWorkingYears>2,2,1)))))))))

Training1_os$NumCompGroup <- with(Training1_os,ifelse(NumCompaniesWorked>4,3,ifelse(NumCompaniesWorked>2,2,1))) #Creating Number of Companies Worked

# For Testing Set

Testing$AgeGroup <- with(Testing,ifelse(Age>55,8,ifelse(Age>50,7,ifelse(Age>45,6,ifelse(Age>40,5,ifelse(Age>35,4,ifelse(Age>30,3,ifelse(Age>25,2,1)))))))) #Creating Age Groups

Testing$DistanceGroup <- with(Testing,ifelse(DistanceFromHome>25,6,ifelse(DistanceFromHome>20,5,ifelse(DistanceFromHome>15,4,ifelse(DistanceFromHome>10,3,ifelse(DistanceFromHome>5,2,1)))))) #Creating Distance Groups

Testing$YearsWithManagerGroup <- with(Testing,ifelse(YearsWithCurrManager>15,5,ifelse(YearsWithCurrManager>10,4,ifelse(YearsWithCurrManager>5,3,ifelse(YearsWithCurrManager>2,2,1))))) #Creating YearsWithManager Groups


Testing$TenureGroup <- with(Testing,ifelse(TenurePerJob>35,9,ifelse(TenurePerJob>30,8,ifelse(TenurePerJob>25,7,ifelse(TenurePerJob>20,6,ifelse(TenurePerJob>15,5,ifelse(TenurePerJob>10,4,ifelse(TenurePerJob>5,3,ifelse(TenurePerJob>2,2,1))))))))) #Creating Tenure Per Job groups

Testing$Change2Group <- with(Testing,ifelse(YearsWithoutChange2>10,3,ifelse(YearsWithoutChange2>5,2,1))) #Creating Years Without Change2

Testing$Change1Group <- with(Testing,ifelse(YearWithoutChange>2.5,3,ifelse(YearWithoutChange>-2.5,2,1))) #Creating Years Without Change 1

#Testing$AvgSatisGroup <- with(Testing,ifelse(AvgSatis<2.5,1,2)) # Creating avg satisfaction group

Testing$WorkYearGroup <- with(Testing,ifelse(TotalWorkingYears>35,9,ifelse(TotalWorkingYears>30,8,ifelse(TotalWorkingYears>25,7,ifelse(TotalWorkingYears>20,6,ifelse(TotalWorkingYears>15,5,ifelse(TotalWorkingYears>10,4,ifelse(TotalWorkingYears>5,3,ifelse(TotalWorkingYears>2,2,1)))))))))

Testing$NumCompGroup <- with(Testing,ifelse(NumCompaniesWorked>4,3,ifelse(NumCompaniesWorked>2,2,1))) #Creating Number of Companies Worked

library(corrplot)
library(psych)

Training_cor <- Training

for(i in 1:ncol(Training_cor)){
  
  Training_cor[,i]<- as.integer(Training_cor[,i])
}

corrplot(cor(Training_cor))

#plot(cor.ci(Training_cor))

# Removing higly correlated Variables, Variables for which binning has been done and near Zero Variance variables 
Train <- Training1_os[,c(2,3,5,7,8,11,12,14,15,16,17,18,21,23,24,26,28,29,30,31,41:48)]

Train <- Training1_os[,c(2,3,5,7,8,11,12,14,15,16,17,18,21,23,24,26,29,30,31,41:48)]


Test <- Testing[,-2]
# Coding the categorical Variables

Train$BusinessTravel <- as.integer(Train$BusinessTravel)
Train$Department <- as.integer(Train$Department)
Train$Gender <- as.integer(Train$Gender)
Train$MaritalStatus <- as.integer(Train$MaritalStatus)
Train$OverTime <- as.integer(Train$OverTime)
Train$JobRole <- as.integer(Train$JobRole)
Train$EducationField <- as.integer(Train$EducationField)

Test$BusinessTravel <- as.integer(Test$BusinessTravel)
Test$Department <- as.integer(Test$Department)
Test$Gender <- as.integer(Test$Gender)
Test$MaritalStatus <- as.integer(Test$MaritalStatus)
Test$OverTime <- as.integer(Test$OverTime)
Test$JobRole <- as.integer(Test$JobRole)
Test$EducationField <- as.integer(Test$EducationField)

Train1 <- Train
for(i in 1:ncol(Train1)){
  Train1[,i] <- as.factor(Train1[,i])
}

fit_rpart <- train(Attrition ~.,Train,method = 'rpart', trControl = trainControl(method = 'cv',number = 3)) # A simple Decision Tree
set.seed(123)
fit_rf <- train(Attrition ~.,Train,method = 'rf', trControl = trainControl(method = 'repeatedcv',number = 3)) # Random Forest

xgbGrid <- expand.grid(nrounds = 300,
                       max_depth = 1,
                       eta = 0.3,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9)

set.seed(12)
fit_xgb <- train(Attrition ~.,Train,method = 'xgbTree',tuneGrid = xgbGrid,trControl = trainControl(method = 'repeatedcv',number = 3,classProbs = TRUE)) 

fit_gbm <- train(Attrition ~.,Train,method = 'gbm',trControl = trainControl(method = 'repeatedcv',number = 3,classProbs = TRUE)) 

fit_treebag <- train(Attrition ~.,Train,method = 'treebag',trControl = trainControl(method = 'repeatedcv',number = 3,classProbs = TRUE)) 

fit_nn <- train(Attrition ~.,Train,method = 'pcaNNet',trControl = trainControl(method = 'repeatedcv',number = 3),tuneGrid = expand.grid(size = 25,decay = 0.01))

fit_glm <- train(Attrition~.,Train,method = 'glm',trControl = trainControl(method = 'repeatedcv',number = 3))

fit_svm <- train(Attrition~.,Train,method = 'svmRadial',trControl = trainControl(method = 'repeatedcv',number = 3))

fit_knn <- train(Attrition~.,Train,method = 'knn',trControl = trainControl(method = 'repeatedcv',number = 3))

fit_glmBoost <- train(Attrition~.,Train,method = 'glmboost',trControl = trainControl(method = 'repeatedcv',number = 3))

Predictions_rpart <- predict(fit_rpart,Test)
Predictions_rf <- predict(fit_rf, Test)
Predictions_xgb <- predict(fit_xgb, Test)
Predictions_nn <- predict(fit_nn, Test)
Predictions_glm <- predict(fit_glm, Test)
Predictions_svm <- predict(fit_svm,Test)
Predictions_knn <- predict(fit_knn,Test)
Predictions_glmboost <- predict(fit_glmBoost,Test)
Predictions_treebag <- predict(fit_treebag,Test)
Predictions_gbm <- predict(fit_gbm,Test)

confusionMatrix(Predictions_xgb,Testing$Attrition)
confusionMatrix(Predictions_treebag,Testing$Attrition)
confusionMatrix(Predictions_gbm,Testing$Attrition)
confusionMatrix(Predictions_rf,Testing$Attrition)
confusionMatrix(Predictions_rpart,Testing$Attrition)
confusionMatrix(Predictions_nn,Testing$Attrition)
confusionMatrix(Predictions_glm,Testing$Attrition)
confusionMatrix(Predictions_svm,Testing$Attrition)
confusionMatrix(Predictions_glmboost,Testing$Attrition)
confusionMatrix(Predictions_knn,Testing$Attrition)

