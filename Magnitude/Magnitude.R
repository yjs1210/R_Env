#Question 5
#Import packages
library("quantmod")
library("xlsx")

#get all the symbols data
data <- getSymbols(c('WFC','AAPL','VWO','AGN','NFLX','GM','BABA','AMZN','FB','GS','^SP500TR','^GSPC'),src='yahoo', from = '2015-01-02', to ='2016-08-31')

#get only closed price data
#dataframeo <> data frame original. Dataframef = data frame final
dataframeo <-data.frame(WFC[,4],AAPL[,4],VWO[,4],AGN[,4],NFLX[,4],GM[,4],BABA[,4],AMZN[,4],FB[,4],GS[,4],SP500TR[,4],GSPC[,4])

#NFLX 7-to-1 stock split adjustment, 2015-07-15
dataframeo[1:133,5] <- dataframeo[1:133,5]/7

#getting rid of SPY and SPY tracker
dataframef <- dataframeo[,-11:-12]

#getting rid of top row to store returns(has one less row)
dataframef = dataframef[-1,]

#calculate daily returns
for (i in 1:10){
  
  returns <- diff(dataframeo[,i])/dataframeo[,i][-length(dataframeo[,i])]
  
  dataframef[,i] <-  returns
  
}

#function to calculate standard deviation of the portfolio.  https://stat.ethz.ch/pipermail/r-help/2011-January/265128.html
lvar <- function(x, weights, na.rm = TRUE) {
  if (missing(weights)) {
    weights <- rep(1, ncol(x))
  }
  
  covmat <- var(x = x, na.rm = na.rm)
  utc <- upper.tri(covmat)
  wt.var <- sum(diag(covmat) * weights^2)
  wt.cov <- sum(weights[row(covmat)[utc]] *
                  weights[col(covmat)[utc]] *
                  covmat[utc])
  variance <- wt.var + 2 * wt.cov
  return(variance)
}

#include weights, assuming cash is 0 standard deviation asset. Question 10 results(final ending allocations)
lvar(x=dataframef,     weights = c(.0729,.0762,.0741,.0710,.1534,.0719,.0736,.1957,.1262,.0684))

