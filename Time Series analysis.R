library(stats)
# simulte ar(1) using ts function as simple mode for AR or MA
set.seed(2016); N=1000; phi=1;
z = rnorm(N,0,1);X = NULL
X[1] = z[1]
for (t in 2:N){
  X[t] = z[t]+phi*X[t-1]
}
X.ts = ts(X)
par(mfrow = c(2,1))
plot(X.ts, main = "AR(1) Time Series on white noise ,phi = 0.4")
X.acf = acf(X.ts, main = "AR(1) Time Series on white noise ,phi = 0.4" )

# simulate AR(2) using arima model 

set.seed(2017)
X.ts <- arima.sim(list(ar = c(0.7,0.2)), n = 1000)
par(mfrow = c(2,1))
plot(X.ts, main = "AR(2) Time Series phi1 = .7, phi2 =0.2 ")
X.acf = acf(X.ts, main = "Autocorrelation of AR(2) Time Series")




#--------------week5--------------------
#loblolly data set
data("Loblolly")
rm(list = ls(all = TRUE))
set.seed(43)
data = arima.sim(list(order=c(2,0,0),ar = c(0.7,-0.2)),n = 2000)
par(mfrow = c(1,2))
acf(data,main = "ACF of AR data of second Order")
acf(data,type = "partial",main = "PACF of time series")
my_data <- read.table(file = "Jefferson Property.xlsx", 
                      header=TRUE)
head(my_data)
library("readxl")
my_data <- read_xlsx("Quantico.xlsx" ,sheet = 1)
colnames(my_data)
stdandart <- (my_data$`Sample Dataset 1`-min(my_data$`Sample Dataset 1`))/(max(my_data$`Sample Dataset 1`)-min(my_data$`Sample Dataset 1`)) 
min(my_data)
max(my_data)

# for week five lecture theory in arma model
# build a useful model for time series as an arma model
# simulate aram models
# swap between aram, ma, ar
set.seed(500)
data = arima.sim(list(order = c(1,0,1),ar = 0.7, ma=.2),n = 1000000)
par(mfcol =c(3,1))
plot(data , main  = "ARMA(1,1) time series phi1=0.7,theta1=0.2",xlim = c(0,400))
acf(data, main = "Autocorrelation of ARMA(1,1) phi1 = .7, theata1 = .2")
acf(data, type = "partial",main = "Partial Autocorrelation of ARMA(1,1) phi1 = .7, theata1 = .2")

#aram examples and properties 
rm(list = ls(all = TRUE))
par(mfrow = c(1,2))
plot(discoveries, main = "time series of Number of Major Scientific Discoveries in a Year")
stripchart(discoveries, method = "stack", offset = 0.5, at = .15 , pch = 19, 
           main = "number of discoveries sotplot",
           xlab = "Number of Major scientific Discoveries in a Year",
           ylab = "Frequancy")
