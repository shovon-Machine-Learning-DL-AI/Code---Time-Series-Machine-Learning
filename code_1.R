library(forecast)

data<-read.csv("P:\\PI Sensors\\mit_meet\\Exiting_money_SPS_AAR_univariate.csv",header = T) #reading the data

data$id<-rownames(data) #creating rownumbers to later split data into training and test
data$id1<- as.numeric(data$id) 

View(data)

data_train<-data[data$id1<205,] #this is all the past data i.e. not having forecasted driver values
data_test<-data[data$id1>204,] # this is forecasted data of drivers
View(data_train) #Viewing the data
View(data_test) 
colnames(data_train)

################### ELM
library(TStools)

d1_3<-as.ts(data_train$AAR_EXM_SPS_A_1WSUM)
elm.fit<-elm(d1_3)
plot(elm.fit)
elm.frc<-forecast(elm.fit,h=4)
write.csv(elm.frc,"P:\\PI Sensors\\mit_meet\\out_elm.csv")

#################### MLP

mlp.fit<-mlp(d1_3,hd.auto.type="valid",hd.max=10)

mlp.frc<-forecast(mlp.fit,h=4)
plot(mlp.frc) 
write.csv(mlp.frc,"P:\\PI Sensors\\mit_meet\\out_mlp.csv")

############ PROPHET
install.packages("prophet")
library(prophet)

d1_2<-as.data.frame(data)

d1_2$y<-d1_2$AAR_EXM_SPS_A_1WSUM
d1_2$ds<-d1_2$date

View(d1_2)

d1_4<-d1_2[c(4,3)]


d1_4$id<-rownames(d1_4) #creating rownumbers to later split data into training and test
d1_4$id1<- as.numeric(d1_4$id) 


d1_4_train<-d1_4[d1_4$id1<205,] #this is all the past data i.e. not having forecasted driver values
d1_4_test<-d1_4[d1_4$id1>204,] 

d1_4_train<-d1_4_train[c(1,2)]
View(d1_4_train)
d1_4_test<-d1_4_test[c(1,2)]



d1_4_train$ds<-as.Date(as.character(d1_4_train$ds),format="%m/%d/%Y")
d1_4_test$ds<-as.Date(as.character(d1_4_test$ds),format="%m/%d/%Y")
d1_4_train$y<-as.ts(d1_4_train$y)
d1_4_test$y<-as.ts(d1_4_test$y)


my_model<-prophet(d1_4_train)

my_forecast <- predict(my_model, d1_4_test)
write.csv(my_forecast,"P:\\PI Sensors\\mit_meet\\out_prophet.csv")

###NNETAR

set.seed(201)
data.nnetar<-nnetar(d1_4_train$y,repeats=20,p=10,P=1,size=6)

data.nnetar.pred<-forecast(data.nnetar,h=4)

m<-accuracy(data.nnetar.pred,d1_4_test$y) #error

View(data.nnetar.pred)

write.csv(data.nnetar.pred,"P:\\PI Sensors\\mit_meet\\forecast_nnetar_v1.csv")


####STL
y_train<-ts(data_train$AAR_EXM_SPS_A_1WSUM,freq=52,start=c(2013,17))
stl.run<-stl(y_train,s.window = "periodic")
plot(stl.run)

seasonal.comp<-stl.run$time.series[,1]
deseasonalized.ts<-(y_train-seasonal.comp)
plot(deseasonalized.ts)
arima.fit.deas<-auto.arima(deseasonalized.ts)
arima.fit.deas.pred<-forecast(arima.fit.deas,h=4)
seasonal.comp.pred<-snaive(seasonal.comp,h=4)
alt.forecast<-arima.fit.deas.pred$mean+seasonal.comp.pred$mean


write.csv(alt.forecast,"P:\\PI Sensors\\mit_meet\\stl.csv")


