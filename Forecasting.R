##Loading RMySQl package to get data from MySQL Database
library("RMySQL")
##Loading Required Packages
library("forecast")
library("dplyr")
##Connecting to MySQL Database
conn <- dbConnect(MySQL(),
                  user="root", password="PWD",
                  dbname="capacity", host="localhost")
##Writing Query to get data from database
result1 = dbSendQuery(conn, "SELECT POWER_METER,date from capacity group by date_format(date,'%Y-%m')")
data.frame1 = fetch(result1)
##Forecasting
data2=data.frame1
View(data2)
##Calculating Mean Absolute Error Rate
mape = function(act, pred){
  res = mean(abs((act-pred)/act))
  return(res)
}
##Creating Time Series Object
data2_ts <- ts(data2[,1],start=2017,frequency = 12)
cycle(data2_ts)
#Plotting time series
plot(data2_ts)
#Plot Power_Demand time series into three components - seasonal, trend and remainder
plot(data_2)
##HoltWinters Method
data2_hws <- HoltWinters(data2_ts)
##Smoothing is controlled by three parameters: alpha, beta, and gamma, for the estimates of the level, slope b of the trend component, 
##and the seasonal component, respectively, at the current time point
data2_hws
plot(data2_hws)
##Predicted and Actual
pred = as.numeric(data2_hws$fitted[,1])
pred
actual= as.numeric(data2_ts)
actual
## In Sample Error
mape(actual, pred)
##Forecasted Value
pred1 = as.data.frame(forecast(data2_hws,h=6, prediction.interval = T, level = 0.95))
##Viewing Forecasted Value
View(pred1)
##Plot of forecasted value
plot(data2_hws, pred1)
####Data Visualisation and Data_Entry####
date= c(seq(as.Date("2019-01-01"), as.Date("2019-06-01"), by="months"))
p1=data.frame(date,pred1[,1])
data.frame1$date=as.Date(data.frame1$date)
n=full_join(data.frame1,p1,by="date")
##Writing Forecasted Values to Table defined in MySQL
dbWriteTable(conn = conn,name = 'predict',value =n,overwrite=T)

