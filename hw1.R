

library(moments) 
library(pdfetch)
library(graphics)
library(psych)
library(tidyquant)
options(digits=4)

## retrieve the price data from yahoo.finance
tickers = c("AAPL","WMT","IBM","FB","TGT")
stockprice = pdfetch_YAHOO(tickers,fields="adjclose",from="2018-01-01", to="2020-12-31", interval= "1wk")
print(head(stockprice))

## calculate log returns
returns = na.omit(diff(log(stockprice)))
print(head(returns))
dims = dim(returns)
n = dims[1]
l = dims[2]

## create time series plot and normal plot
for (i in 1:l){
  par(mfrow=c(1,2))
  names=colnames(returns)
  plot(ts(returns[,i]),main=paste(names[i],"' return over time"))
  qqnorm(as.vector(returns[,i]),datax=TRUE,main=names[i])
  qqline(as.vector(returns[,i]),datax=TRUE)
}

## simulation 100 days forward

days = 100
future_price = matrix(0,nrow=days,ncol=l)
colnames(future_price) = colnames(stockprice)
for (i in 1:l)
{
  mu = mean(as.vector(returns[,i]))/7
  std = sd(as.vector(returns[,i]))/sqrt(7)
  dret = rnorm(days,mu,std)
  cumret = cumsum(dret)
  price100 = as.data.frame(stockprice)[n,i]*exp(cumret)
  future_price[,i] = price100
}
summary(future_price)
par(mfrow=c(1,1))
ts.plot(future_price,col = c("blue","red","green","orange","purple"))

## simulate each stock 100 days forward over 20 iterations

iter = 20
simu20 = matrix(0,nrow=days,ncol=iter)
price_simu20 = matrix(0,nrow=days,ncol=l)
colnames(price_simu20) = colnames(stockprice)
for (i in 1:l){
  mu = mean(as.vector(returns[,i]))/7
  std = sd(as.vector(returns[,i]))/sqrt(7)
  for (j in 1:iter){
    dret = rnorm(days,mu,std)
    cumret = cumsum(dret)
    price100 = as.data.frame(stockprice)[n,i]*exp(cumret)
    simu20[,j] = price100
  }
  pricemu = rowMeans(simu20)
  price_simu20[,i] = pricemu
}
summary(price_simu20)

