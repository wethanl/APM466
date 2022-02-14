library(readxl)
library(tidyverse)
#install.packages('jrvFinance')
library(jrvFinance)

all_bonds <- read_xlsx('APM466 Data.xlsx',sheet = '10 selected')
close_matrix = matrix(c(all_bonds$`CDA 2023(1)`,all_bonds$`CDA 2022...2`,all_bonds$`CDA 2023`,all_bonds$`CDA 2024`,all_bonds$`CANADA 21/23`,all_bonds$`CANADA 21/23`,all_bonds$`CDA 2022...6`,all_bonds$`CDA 2027`,all_bonds$`CANADA 19/25`,all_bonds$`CANADA 21/26`,all_bonds$`CANADA 21/27`),nrow = 10, ncol = 10, byrow =FALSE)
maturity = c('2023-06-01','2022-03-01','2023-03-01','2024-03-01','2023-08-01,','2022-06-01','2027-05-31','2025-03-01','2026-09-01','2027-03-01')
coupon = c(0.0015,0.0005,0.0175,0.0225,0.0025,0.0275,0.001,0.0125,0.001,0.0125)

close_price_date = c('2021-01-10','2021-01-11','2021-01-12','2021-01-13','2021-01-14','2021-01-17','2021-01-18','2021-01-19','2021-01-20','2021-01-21')
ytm_matrix = matrix('numeric',nrow=10,ncol=10)
for (j in c(1:10)){
  close_price = close_matrix[,j]
  for (i in c(1:10)){
    ytm_matrix[i,j] <- bond.yield(settle = close_price_date[i],mature = maturity[j],coupon = coupon[j],freq = 2, close_price[i],convention = c("30/360", "ACT/ACT", "ACT/360", "30/360E"),
                                  comp.freq = 2, redemption_value = 100)
  }
}

year <- seq(0.5,5,0.5)
plot(year,ytm_matrix[1,],type = 'o', xlab = 'Year', ylab = 'Yield to Maturity', main = 'YTM curve',col='red')
lines(year,ytm_matrix[2,],col=1,lty=1)
lines(year,ytm_matrix[3,],col=2,lty=1)
lines(year,ytm_matrix[4,],col=3,lty=1)
lines(year,ytm_matrix[5,],col=4,lty=1)
lines(year,ytm_matrix[6,],col=5,lty=1)
lines(year,ytm_matrix[7,],col=6,lty=1)
lines(year,ytm_matrix[8,],col=7,lty=1)
lines(year,ytm_matrix[9,],col=8,lty=1)
lines(year,ytm_matrix[10,],col=9,lty=1)
legend('topleft', legend=c("CDA 2023(1)","CDA 2022","CDA 2023","CDA 2024", "CANADA 21/23",	"CDA 2022",	"CDA 2027",	"CANADA 19/25", "CANADA 21/26",	"CANADA 21/27"),col=c("red","1", "2", "3", "4", "5", "6", "7","8","9"), lty=1, cex=0.8)
