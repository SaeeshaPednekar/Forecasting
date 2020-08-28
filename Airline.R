library(readr)
airline<-read.csv("F:/Excelr/Assignments/dataset/forecasting/Airlines+Data.csv")
str(airline)
plot(airline$Passengers,type="o")


months_12<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(months_12)
colnames(months_12)<-month.abb # Assigning month names 
View(months_12)

air_data<-cbind(airline,months_12)
View(air_data)
air_data["t"]<- 1:96
air_data["log_airline"]<-log(air_data["Passengers"])
air_data["t_square"]<-air_data["t"]*air_data["t"]
attach(air_data)
View(air_data)

train<-air_data[1:84,]

test<-air_data[85:96,]


########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
str(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear #  53.19924
#sqrt(mean(reg_sqrt$residuals^2))
#v<-sqrt(mean(linear_model$residuals^2))

######################### Exponential #################################

expo_model<-lm(log_airline~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.05736
######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad #48.05189

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132.8198

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear #35.34896

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36082

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_airline~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.0632

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_airline~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 10.51917

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

new_model<-lm(log_airline~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = air_data)

summary(new_model)

#########predicting
test_data<-read.csv("F:/Excelr/Assignments/dataset/forecasting/pred_airlines.csv")
View(test_data)

new_model_pred<-predict(new_model,newdata = test_data)
new_model_pred

test_data["forecasted passengers"]<-new_model_pred

View(test_data)


resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 12)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
acf(k$residuals)

test_data["Forcasted errors"]<-pred_res$pred
test_data["Final forcast"]<-test_data$'forecasted passengers'+test_data$'Forcasted errors'




######################################arima model
#find q(lag value before 1st insignificant lag)
acf(airline$Passengers,lag.max=30)

#for p(1st significant lag)
pacf(airline$Passengers,lag.max=12)#checks for lag of errors)

arima_k<-arima(airline$Passengers,order = c(1,1,26))
pred_arima4<-predict(arima_k,n.ahead = 12)
pred_arima4$pred

test_data["forecast by arima"]<-pred_arima4$pred
View(test_data)



#without adjustments  with auto regression of orders
arima_k3<-arima(airline$Passengers,order = c(1,0,26))
pred_arima3<-predict(arima_k3,n.ahead=12)
pred_arima3$pred





test_data["forecast by arima without i"]<-pred_arima3$pred
