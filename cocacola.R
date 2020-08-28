install.packages("readxl")
library(readxl)
install.packages("forecast")
library(forecast)


library(readr)
cocacola<-read.csv("F:/Excelr/Assignments/dataset/forecasting/CocaCola_Sales_Rawdata.csv")
plot(cocacola$Sales,type="o")
Q1 <-  ifelse(grepl("Q1",cocacola$ï..Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cocacola$ï..Quarter),'1','0')

Q3 <-  ifelse(grepl("Q3",cocacola$ï..Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cocacola$ï..Quarter),'1','0')

cocacola_sales<-cbind(cocacola,Q1,Q2,Q3,Q4)
View(cocacola_sales)
cocacola_sales["t"]<-1:42
cocacola_sales["t_square"]<-cocacola_sales$t*cocacola_sales$t
cocacola_sales["log_sales"]<-log(cocacola_sales$Sales)
View(cocacola_sales)
attach(cocacola_sales)


train<-cocacola_sales[1:30,]
test<-cocacola_sales[31:42,]



########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
str(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear #  714.0144
#sqrt(mean(reg_sqrt$residuals^2))
#v<-sqrt(mean(linear_model$residuals^2))

######################### Exponential #################################

expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo #552.2821
######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 646.2715

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #  1778.007

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear #637.9405

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #  586.0533

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #  1828.924
######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea # 410.2497





# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
str(cocacola_sales)
str(test_data)
  # Additive seasonality with quadratic has least RMSE value

new_model <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=cocacola_sales)
summary(new_model)


test_data<-read.csv("F:/Excelr/Assignments/dataset/forecasting/cocacola_pred_new.csv")
test_data$Q1<-as.factor(test_data$Q1)
test_data$Q2<-as.factor(test_data$Q2)
test_data$Q3<-as.factor(test_data$Q3)
test_data$Q4<-as.factor(test_data$Q4)

pred_new_cocacola<-predict(new_model,newdata = test_data)

test_data["forecasted sales"]<-pred_new_cocacola
View(test_data)
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)


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
test_data["Final forcast"]<-test_data$'forecasted sales'+test_data$'Forcasted errors'





###############arima model
#Arima =p i q
#find q(lag value before 1st insignificant lag)
acf(cocacola$Sales,lag.max=12)

#for p(1st significant lag)
pacf(cocacola$Sales,lag.max = 12)#checks for lag of errors)

arima_k<-arima(cocacola$Sales,order = c(1,1,9))
pred_arima4<-predict(arima_k,n.ahead = 12)
pred_arima4$pred

test_data["forecast by arima"]<-pred_arima4$pred
View(test_data)



#without adjustments  with auto regression of orders
arima_k3<-arima(cocacola$Sales,order = c(1,0,9))
pred_arima3<-predict(arima_k3,n.ahead=12)
pred_arima3$pred

test_data["forecast by arima without i"]<-pred_arima3$pred

