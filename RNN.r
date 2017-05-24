/***************Create an Univariate- TS Model using RNN*********************/
#Read the Dataset for the KPI
setwd("C:\\Users\\a589565\\Desktop\\PI Sensors\\Data")
getwd()
em_sps<-read.csv("Exiting_money_SPS_AAR_univariate.csv",header=TRUE)
head(em_sps)
describe(em_sps)
summary(em_sps)
class(em_sps)
#Plot the series

colnames(em_sps)
#Convert it into a TS object and plot
data<-em_sps[,c(1:2)]
data<-as.numeric(unlist(em_sps[2]))
head(data)
data_ts<-ts(matrix(data),
            start=c(2013,48),
            end=c(2017,14),
            frequency = 52)
head(data_ts)
class(data_ts)
require(graphics)
#Generate Time Series Plot
plot(data_ts, ylab="data_ts$AAR_EXM_SPS_A_1WSUM", xlab="data_ts$date", main="AAR_EXM_SPS_A_1WSUM")
# plot(data_ts)
# plot(data_ts,)
plot(data_ts,
     xlab='date',
     ylab='AAR_EXM_SPS_A_1WSUM',
     col='darkblue')
pacf<-pacf(data_ts)
pacf

#Exploring and preparing the data
data_em<-em_sps
data_em_1<-data_em[,-1]
require(quantmod)
data_em_2<-as.zoo(data_em_1)
data_em_2
class(data_em_2)
x1<-Lag(data_em_2,k=1)
x2<-Lag(data_em_2,k=2)
x3<-Lag(data_em_2,k=3)
x4<-Lag(data_em_2,k=4)
x12<-Lag(data_em_2,k=12)
x24<-Lag(data_em_2,k=24)
x52<-Lag(data_em_2,k=52)

# Create the Overall data structure
x<-cbind(x1,x2,x3,x4,x12,x24,x52,data_em_2)
class(x)
# View(x)
head(x)

# Remove the missing values
x<-x[-(1:52),]
View(head(x))
# Convert the data with Log transforation
# x<-log(x)
View(head(x))
# Preparing the data for input of the RNN
x<-data.matrix(x)
range_data<-function(x){(x-min(x))/(max(x)-min(x))}
min_data<-min(x)
max_data<-max(x)
x<-range_data(x)
View(head(x))
max(x)
min(x)
# Creation of Test and Train Samples
# Lets make sure that the data are in proper matrix format
colnames(x)
x1<-as.matrix(x[,1])
x2<-as.matrix(x[,2])
x3<-as.matrix(x[,3])
x4<-as.matrix(x[,4])
x12<-as.matrix(x[,5])
x24<-as.matrix(x[,6])
x52<-as.matrix(x[,7])
y<-as.matrix(x[,8])
nrow(x)
# The model will predict last 6 weeks/months of price
n_train<-152
y_train<-as.matrix(y[1:n_train])
x1_train<-as.matrix(t(x1[1:n_train,]))#feature data are transposed to meet the requirement for RNN
x2_train<-as.matrix(t(x2[1:n_train,]))
x3_train<-as.matrix(t(x3[1:n_train,]))
x4_train<-as.matrix(t(x4[1:n_train,]))
x12_train<-as.matrix(t(x12[1:n_train,]))
x24_train<-as.matrix(t(x24[1:n_train,]))
x52_train<-as.matrix(t(x52[1:n_train,]))

x1_train
y_train
nrow(x)
ncol(x1_train)
ncol(x2_train)
ncol(x3_train)
ncol(x4_train)
ncol(x12_train)
ncol(x24_train)
ncol(x52_train)

#Test Dataset Creation
y_test<-as.matrix(y[(n_train+1):nrow(x4)])#All the features should be transposed but not the target var
nrow(y_test)
x1_test<-as.matrix(t(x1[(n_train+1):nrow(x1),]))#feature data are transposed to meet the requirement for RNN
x2_test<-as.matrix(t(x2[(n_train+1):nrow(x2),]))
x3_test<-as.matrix(t(x3[(n_train+1):nrow(x3),]))
x4_test<-as.matrix(t(x4[(n_train+1):nrow(x4),]))
x12_test<-as.matrix(t(x12[(n_train+1):nrow(x12),]))
x24_test<-as.matrix(t(x24[(n_train+1):nrow(x24),]))
x52_test<-as.matrix(t(x52[(n_train+1):nrow(x52),]))
ncol(x1_test)
ncol(x2_test)
ncol(x3_test)
ncol(x4_test)
ncol(x12_test)
ncol(x24_test)
ncol(x52_test)

#Training a model on the data
x_train<-array(c(x1_train,x2_train,x3_train,x4_train,x12_train,x24_train,x52_train),dim=c(dim(x1_train),7))
x_test<-array(c(x1_test,x2_test,x3_test,x4_test,x12_test,x24_test,x52_test),dim=c(dim(x1_test),7))
dim(x_train)
dim(x_test)

require(rnn)
set.seed(2018)
model1<-trainr(Y=t(y_train),
               X=x_train,
               learningrate = 0.05,
               hidden_dim = c(4,3),
               numepochs = 600,
               network_type = 'rnn',
               sigmoid = "logistic")
# 14.6823767474965
model2<-trainr(Y=t(y_train),
               X=x_train,
               learningrate = 0.05,
               hidden_dim = c(4,3),
               numepochs = 500,
               network_type = 'rnn',
               sigmoid = "tanh")
#
model3<-trainr(Y=t(y_train),
               X=x_train,
               learningrate = 0.05,
               hidden_dim = c(4,3),
               numepochs = 500,
               network_type = 'rnn',
               sigmoid = "Gompertz")
#16.607925576497
set.seed(2018)
model4<-trainr(Y=t(y_train),
               X=x_train,
               learningrate = 0.1,#Initially was set at 0.05
               hidden_dim = 5,#initially used 3
               numepochs = 600,
               network_type = 'rnn',
               sigmoid = "logistic")
#11.6214066128031 --So far the best
#Examining Error by Epoch
error_1<-t(model1$error)
error_2<-t(model2$error)
error_3<-t(model3$error)
error_4<-t(model4$error)

rownames(error_1)<-1:nrow(error_1)
colnames(error_1)<-"error_1"
plot(error_1)

rownames(error_2)<-1:nrow(error_2)
colnames(error_2)<-"error_2"
plot(error_2)

rownames(error_3)<-1:nrow(error_3)
colnames(error_3)<-"error_3"
plot(error_3)

rownames(error_4)<-1:nrow(error_4)
colnames(error_4)<-"error_4"
plot(error_4)

#Evaluating the Model Performance
pred1_train<-t(predictr(model1,x_train))
pred1_train
pred2_train<-t(predictr(model2,x_train))
pred2_train
pred3_train<-t(predictr(model3,x_train))
pred3_train
pred4_train<-t(predictr(model4,x_train))
pred4_train
#We use correlation coeffs to judge how closely the predictions are
round(cor(y_train,pred1_train),5)
round(cor(y_train,pred2_train),5)
round(cor(y_train,pred3_train),5)
round(cor(y_train,pred4_train),5)
plot(y_train,pred1_train,ylab="pred1_train")
plot(y_train,pred2_train,ylab="pred2_train")
plot(y_train,pred3_train,ylab="pred3_train")
plot(y_train,pred4_train,ylab="pred4_train")
/**********************Creating the Final Prediction**************/
pred1_test<-t(predictr(model1,x_test))
pred2_test<-t(predictr(model2,x_test))
pred3_test<-t(predictr(model3,x_test))
pred4_test<-t(predictr(model4,x_test))
pred1_test
pred2_test
pred3_test
pred4_test
pred1_test_df<-as.data.frame(pred1_test)
View(pred1_test_df)
#Unscaling the data
max_data<-max(data_em_2)
max_data
min_data<-min(data_em_2)
min_data
unscale_data<-function(x,max_x,min_x){x*(max_x-min_x)+min_x}
#Creating the Prediction Template
pred1_actual<-unscale_data(pred1_test,max_data,min_data)
pred2_actual<-unscale_data(pred2_test,max_data,min_data)
pred3_actual<-unscale_data(pred3_test,max_data,min_data)
pred4_actual<-unscale_data(pred4_test,max_data,min_data)
# pred1_actual<-exp(pred1_actual)
pred1_actual
pred2_actual
pred3_actual
pred4_actual

y_actual<-unscale_data(y_test,max_data,min_data)
result_all<-cbind(y_actual,round(pred1_actual,2),
                  round(pred3_actual,2),round(pred4_actual,2))
colnames(result_all)<-c("actual","model1","model3","model4")
View(as.data.frame(result_all))
final_forecast<-as.data.frame(result_all)
write.csv(final_forecast,"final_forecast.csv")
#pred1_actual_n<-as.data.frame(pred1_actual)
#pred1_actual_n
