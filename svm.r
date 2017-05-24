print(date())
print("Start")
#install.packages("PivotalR")
library("PivotalR")
#con <- db.connect(port = 5432, dbname = "aae_data", host = "pvrtp01", user="a589565", password="*India12345")
#prdf1=db.data.frame("a432976.madlib_svm")

data <- read.csv("P:\\PI Sensors\\NM_SPS_A\\NM_SPS_EXTRACT_NEW.csv",header = T)
fix(data)

library(e1071) 

data$id<-rownames(data) #creating rownumbers to later split data into training and test
data$id1<- as.numeric(data$id) 
fix(data)

data_train<-data[data$id1<140,] #this is all the past data i.e. not having forecasted driver values
# View(data_train)
data_test<-data[data$id1>139,] # this is forecasted data of drivers
print(date())
print("Data Loading")

#### Deleting the id columns

data_train$id<-NULL 
data_train$id1<-NULL
data_test$id<-NULL
data_test$id1<-NULL
data$id<-NULL
data$id1<-NULL


data2<-data #creating copy of dataset
data2$NM_SPS_A_1WSUM <-NULL # deleting the KPI column
print(date())
print("Data Preparation")

model <- svm(NM_SPS_A_1WSUM ~ .
             , data) #building SVM model

predictedY <- predict(model, data2) # computing predicted values for entire data
predictedY1<-as.data.frame(predictedY) #changing object to data frame
predictedY1$date<-data2$date

delete("a589565.sensor_scores1")
"as.db.data.frame"(predictedY1,table.name="a589565.sensor_scores1",verbose = FALSE,conn.id=1,add.row.names=TRUE)
print(date())
print("Scoring")
############ tuning parameters ##################3

# perform a grid search

tuneResult <- tune(svm, NM_SPS_A_1WSUM ~ .,data = data,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult) # this gives tuned paramter values, select these and plug into svm function
# plot(tuneResult)
print(date())
print("Tuning")
#########  Rebuilding SVM model with tuned parameters ##############3
model2 <- svm(NM_SPS_A_1WSUM ~ ., data,epsilon=tuneResult$best.parameters[,1],cost=tuneResult$best.parameters[,2]) #building the SVM model with tuned paramters

predictedY <- predict(model2, data2)
predictedY1<-as.data.frame(predictedY,d)

write.csv(predictedY1,"C:\\Users\\a592594\\Desktop\\svm_results_new.csv")
predictedY1$date<-data2$date

#delete("a589565.sensor_scores2")
#"as.db.data.frame"(predictedY1,table.name="a589565.sensor_scores2",verbose = FALSE,conn.id=1,add.row.names=TRUE)
#print(date())
#print("End")



