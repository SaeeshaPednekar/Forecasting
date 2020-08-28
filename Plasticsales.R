library(readr)
plasticsales<-read.csv("F:/Excelr/Assignments/dataset/forecasting/PlasticSales.csv")
View(plasticsales)
plot(plasticsales$Sales,type="o")

months_12<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
View(months_12)
colnames(months_12)<-month.abb # Assigning month names 
View(months_12)

sales_data<-cbind(plasticsales,months_12)
View(sales_data)
sales_data["t"]<-1:60
sales_data["t_square"]<-sales_data["t"]*sales_data["t"]
sales_data["log_sales"]<-log(sales_data["Sales"])
#sales_data["log_sales"]<-log(sales_data$Sales)
View(sales_data)
attach(sales_data)
train<-sales_data[1:48,]
test<-sales_data[49:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
str(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear #  260.9378
#sqrt(mean(reg_sqrt$residuals^2))
#v<-sqrt(mean(linear_model$residuals^2))

######################### Exponential #################################

expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo #268.6938
######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad #297.4067

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 235.6027

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear #135.5536

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #  218.1939

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 239.6543
######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea #160.6833





# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


# Additive seasonality with linear has least RMSE value

new_model <- lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = sales_data)
summary(new_model)


test_data<-read.csv("F:/Excelr/Assignments/dataset/forecasting/pred_plasticsales.csv")

str(test_data)
View(test_data)

pred_new<-predict(new_model,newdata=test_data)
View(pred_new)
test_data["forecasted sales"]<-pred_new 
View(test_data)
pred_new$fit <- pred_new$fit+pred_res$pred





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
acf(plasticsales$Sales,lag.max=12)

#for p(1st significant lag)
pacf(plasticsales$Sales,lag.max = 12)#checks for lag of errors)

arima_k<-arima(plasticsales$Sales,order = c(1,1,3))
pred_arima4<-predict(arima_k,n.ahead = 12)
pred_arima4$pred

test_data["forecast by arima"]<-pred_arima4$pred
View(test_data)



#without adjustments  with auto regression of orders
arima_k3<-arima(plasticsales$Sales,order = c(1,0,3))
pred_arima3<-predict(arima_k3,n.ahead=12)
pred_arima3$pred

test_data["forecast by arima without i"]<-pred_arima3$pred
