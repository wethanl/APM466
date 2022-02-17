
library(readxl)
library(tidyverse)
library(dplyr)
library(jrvFinance)


# Data Collection

ten_clean <- read_xlsx("APM466 Data.xlsx",sheet = 'clean')

ten_clean <- as.data.frame(ten_clean)
Closing_price = matrix(c(ten_clean$`10-J`,ten_clean$`11-J`,ten_clean$`12-J`,ten_clean$`13-J`,
                         ten_clean$`14-J`,ten_clean$`17-J`,ten_clean$`18-J`,ten_clean$`19-J`,
                         ten_clean$`20-J`,ten_clean$`21-J`), nrow=10, ncol = 10, byrow=TRUE)
Close_price_date <- c("2022-1-10","2022-1-11","2022-1-12","2022-1-13","2022-1-14",
                      "2022-1-17","2022-1-18","2022-1-19","2022-1-20","2022-1-21")
maturity_date <-as.Date( c('2022-03-01','2022-10-31','2023-05-01','2023-10-31','2024-03-01',
                           '2024-09-01','2025-05-31','2025-08-31','2026-02-28','2026-09-01'))
Coupon <- as.numeric(ten_clean$COUPON)
ten_dirty <- ten_clean
for (j in 1:10){
  for (i in 1:10){
    ten_dirty[i,j+5] =
      as.numeric(Closing_price[j,i]+
                   bond.TCF(Close_price_date[j],maturity_date[i],
                            Coupon[i],freq = 2,redemption_value = 100)$accrued)
  }
}


ten_dirty <- as.data.frame(ten_dirty) 
n_of_year = matrix('numeric', nrow=10, ncol=10)

for (i in c(1:10)){
  for (j in c(1:10)){
    n_of_year[i,j] = yearFraction(Close_price_date[i], maturity_date[j])
  }
}
month = 30
n_of_maturity <- vector("numeric",10)
for (i in 1:10){
  n_of_days = -as.numeric(as.Date(Sys.Date()))+ as.numeric(as.Date(maturity_date[i]))
  n_of_maturity[i]=n_of_days/month
}

last_coupon <- c("2021-08-28","2021-10-31","2021-11-01","2021-10-31","2021-09-30",
                 "2021-09-01","2021-10-31","2021-08-31","2021-08-28","2021-09-01")
n_of_coupon <- vector("numeric",10)
for (i in 1:10){
  n_of_days = as.numeric(as.Date(Sys.Date()))- as.numeric(as.Date(last_coupon[i]))
  n_of_coupon[i]=n_of_days/month
}

#Yield Curve
Yield_matrix = matrix(0, nrow=10, ncol=10)

for (j in 1:10){
  close_price = Closing_price[,j]
  for (i in 1:10){
    Yield_matrix[i,j] = as.numeric(bond.yield(settle=Close_price_date[i], 
                                              mature = maturity_date[i],
                                              coupon = Coupon[i],
                                              price = close_price[i],
                                              freq = 2,#from this line it is all by default
                                              convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                                              redemption_value = 100,
                                              comp.freq = 2))}
  }
year <- seq(0.5,5,0.5)
plot(year,Yield_matrix[1,],type = 'l', xlab = 'Year', ylab = 'Yield to Maturity', main = 'YTM curve',col='red', ylim=c(-0.1,0.1))
lines(year,Yield_matrix[2,],col=1,lty=1,type = 'l')
lines(year,Yield_matrix[3,],col=2,lty=1,type = 'l')
lines(year,Yield_matrix[4,],col=3,lty=1,type = 'l')
lines(year,Yield_matrix[5,],col=4,lty=1,type = 'l')
lines(year,Yield_matrix[6,],col=5,lty=1,type = 'l')
lines(year,Yield_matrix[7,],col=6,lty=1,type = 'l')
lines(year,Yield_matrix[8,],col=7,lty=1,type = 'l')
lines(year,Yield_matrix[9,],col=8,lty=1,type = 'l')
lines(year,Yield_matrix[10,],col=9,lty=1,type = 'l')
legend('topleft', legend=Close_price_date, col=c("red","1", "2", "3", "4", "5", "6", "7","8","9"), lty=1, cex=0.5)

# Spot Curve

spot_matrix <- data.frame (a = c(0,0,0,0,0,0,0,0,0,0),b = c(0,0,0,0,0,0,0,0,0,0),
                           c  = c(0,0,0,0,0,0,0,0,0,0),
                           d  = c(0,0,0,0,0,0,0,0,0,0),
                           e  = c(0,0,0,0,0,0,0,0,0,0),
                           f  = c(0,0,0,0,0,0,0,0,0,0),
                           g  = c(0,0,0,0,0,0,0,0,0,0),
                           h  = c(0,0,0,0,0,0,0,0,0,0),
                           i  = c(0,0,0,0,0,0,0,0,0,0),
                           j  = c(0,0,0,0,0,0,0,0,0,0))


for (i in 1:10)
{
  price=ten_dirty[1,5+i]
  value=100
  coupon=ten_dirty[1,4]*value/2
  spot_matrix[1,i]=2*((price/(coupon+value))^(-1/(2*n_of_maturity[1]))-1)
}

for (i in c(2:10)){
  for (j in c(1:10)){
    price=ten_dirty[i,5+j]
    value=100
    coupon=ten_dirty$COUPON[i]*value/2
    l=0
    cpn_time=seq((6-n_of_coupon[i])/12, (n_of_maturity[i]-1)/12, 1/2)
    for (k in c(1:length(cpn_time))){
      l=coupon*(1+spot_matrix[k,j]/2)^(-2*cpn_time[k])+l
    }
    new_price=price-l
    l=0
    spot_matrix[i,j]=2*((new_price/(coupon+value))^(-1/(2*n_of_maturity)[i])-1)
  }
}

plot(year, spot_matrix[1,],type='l',main="Spot Curve", col='red', xlab='Years', ylab='Spot Rate ',ylim = c(0,0.005))
lines(year,spot_matrix[2,],col=1,lty=1,type = 'l')
lines(year,spot_matrix[3,],col=2,lty=1,type = 'l')
lines(year,spot_matrix[4,],col=3,lty=1,type = 'l')
lines(year,spot_matrix[5,],col=4,lty=1,type = 'l')
lines(year,spot_matrix[6,],col=5,lty=1,type = 'l')
lines(year,spot_matrix[7,],col=6,lty=1,type = 'l')
lines(year,spot_matrix[8,],col=7,lty=1,type = 'l')
lines(year,spot_matrix[9,],col=8,lty=1,type = 'l')
lines(year,spot_matrix[10,],col=9,lty=1,type = 'l')
legend('topleft', legend=Close_price_date, col=c("red","1", "2", "3", "4", "5", "6", "7","8","9"), lty=1, cex=0.5)

# Forward Curve

forward_rate <- data.frame (a = c(0,0,0,0,0,0,0,0,0,0),
                            b = c(0,0,0,0,0,0,0,0,0,0),
                            c  = c(0,0,0,0,0,0,0,0,0,0),
                            d  = c(0,0,0,0,0,0,0,0,0,0))
for (i in c(1:4)){
  for (j in c(1:10)){
    nyear=(1+spot_matrix[2*i,j]/2)^(2*i)
    oneyear_f=(1+spot_matrix[2+2*i,j]/2)^(2+2*i)
    forward_rate[j,i]=2*((oneyear_f/nyear)^(0.5)-1)
  }
}



plot(c(2,3,4,5), forward_rate[1,],type='l',main="Forward Curve", col='red', xlab='Years', ylab='Forward Rate',ylim = c(-0.0005,0.004))
lines(c(2,3,4,5),forward_rate[2,],col=1,lty=1,type = 'l')
lines(c(2,3,4,5),forward_rate[3,],col=1,lty=1,type = 'l')
lines(c(2,3,4,5),forward_rate[4,],col=1,lty=1,type = 'l')
lines(c(2,3,4,5),forward_rate[5,],col=1,lty=1,type = 'l')
lines(c(2,3,4,5),forward_rate[6,],col=1,lty=1,type = 'l')
lines(c(2,3,4,5),forward_rate[7,],col=1,lty=1,type = 'l')
lines(c(2,3,4,5),forward_rate[8,],col=1,lty=1,type = 'l')
lines(c(2,3,4,5),forward_rate[9,],col=1,lty=1,type = 'l')
lines(c(2,3,4,5),forward_rate[10,],col=1,lty=1,type = 'l')
legend('topleft', legend=Close_price_date, col=c("red","1", "2", "3", "4", "5", "6", "7","8","9"), lty=1, cex=0.5)

# Q5

x1=x2=x3=x4=x5=vector("numeric",9)
for (i in c(1:9)){
  x1[i]=log(as.numeric(abs(ytm_matrix[2,i]))/as.numeric(abs(ytm_matrix[2,i+1])))
  x2[i]=log(as.numeric(abs(ytm_matrix[4,i]))/as.numeric(abs(ytm_matrix[4,i+1])))
  x3[i]=log(as.numeric(abs(ytm_matrix[6,i]))/as.numeric(abs(ytm_matrix[6,i+1])))
  x4[i]=log(as.numeric(abs(ytm_matrix[8,i]))/as.numeric(abs(ytm_matrix[8,i+1])))
  x5[i]=log(as.numeric(abs(ytm_matrix[10,i])/as.numeric(abs(ytm_matrix[10,i+1]))))
}

covariance <- cov(data.frame(x1,x2,x3,x4,x5), data.frame(x1,x2,x3,x4,x5))


x6=x7=x8=x9=x10=x11=x12=x13=x14=x15=vector("numeric",3)
for (i in c(1:3)){
  x6[i]=log(as.numeric(abs(forward_rate[1,i]))/abs(as.numeric(forward_rate[1,i+1])))
  x7[i]=log(as.numeric(abs(forward_rate[2,i]))/abs(as.numeric(forward_rate[2,i+1])))
  x8[i]=log(as.numeric(abs(forward_rate[3,i]))/abs(as.numeric(forward_rate[3,i+1])))
  x9[i]=log(as.numeric(abs(forward_rate[4,i]))/abs(as.numeric(forward_rate[4,i+1])))
}
data.frame(x6,x7,x8,x9)
covariance_b <- cov(data.frame(x6,x7,x8,x9), data.frame(x6,x7,x8,x9))
