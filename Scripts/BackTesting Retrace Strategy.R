#library(glm)
library(Quandl)
library(quantmod)

#Quotes <- c("SPY","XOM","AAPL","BAC","SIRI","AMD","PG","TWTR","WFC","CHK","CAB")
#trailing30 <- c(0:29)

#for (stocks in Quotes){
  
  
 # getSymbols(stocks)
#  realtime<- getQuote(paste("QQQQ;",stocks), what=yahooQF("Last Trade (Price Only)"))
#  realtime<- as.numeric(as.character(realtime$Last[2]))
#  Stock<-eval(parse(text = stocks)) 
#  data <- to.daily(Stock[5:length(Stock[,1])])
  
  
  
  
#}



#testing here

getSymbols("SPY")
realtime<- getQuote(paste("QQQQ;","SPY"), what=yahooQF("Last Trade (Price Only)"))
realtime<- as.numeric(as.character(realtime$Last[2]))
Stock<-eval(parse(text = "SPY")) 

data <- to.daily(Stock[5:length(Stock[,1])])


strategydata <- data.frame(open=NA, 
                     close=NA,
                     top =NA,
                     bottom = NA,
                     bar=NA, 
                     barthreshold =NA,
                     strategytriggered =NA, 
                     strategycontd =NA,
                     days =NA,
                     strategyentered =NA,
                     strategyclosed =NA,
                     enterrange =NA,
                     exitrange = NA,
                     stoploss= NA,
                     buyprice = NA,
                     sellprice = NA,
                     PNL =NA)[numeric(0),]


trailing30 <- c(0:29)
top<-0
bottom<-0
open<-0
close<-0
barthreshold <-0
strategyinit <-0
strategycont <-0
strategyentered <-0
buyprice <- 0
strategyclosed <-0
sellprice <-0
enterrange <-0
exitrange <-0
pnl<-0
days<-0
stoploss<-0

for (i in 1:nrow(data)){
 
  top<- coredata(data[i,2])
  bottom<- coredata(data[i,3])
  open <- coredata(data[i,1])
  close <- coredata(data[i,4])
  bar = close-open

  #skip for first 30 days
  if (i <31){
      trailing30[i] = bar
      strategydata = rbind(strategydata, c(open,close,top,bottom,bar,barthreshold,strategyinit,strategycont,
                                           days,strategyentered,strategyclosed,enterrange,exitrange,stoploss,
                                           buyprice,sellprice,pnl
                                    ))      
      next
      
  }

  #start logic here
  else {
   
     #identify trailing 30 days 
    trailing30<- trailing30[-1]
    trailing30<- c(trailing30,bar)
    barthreshold <- quantile(trailing30, .90)
    
    
    #if we entered a trade then look for exit 
    if (strategyentered==1){
      if(
        #exit range is within the range 
        ((bottom<exitrange & exitrange<top) 
        #it gapped up and exit range is now lower than the entire price range 
        ####We should adjust the exit price for this case scenario)
        #### if it gaps lower, that's okay we will handle that case at the stop loss case
       || (exitrange<bottom & exitrange<top))
        ){
        sellprice <- exitrange
        pnl <- sellprice-buyprice
        strategyclosed<-1
        strategydata = rbind(strategydata, c(open,close,top,bottom,bar,barthreshold,strategyinit,strategycont,
                                             days,strategyentered,strategyclosed,enterrange,exitrange,stoploss,
                                             buyprice,sellprice,pnl
        ))      
        
        strategyinit <-0
        strategycont <-0
        strategyentered <-0
        strategyclosed<-0
        buyprice <- 0
        strategyclosed <-0
        sellprice <-0
        enterrange <-0
        exitrange <-0
        pnl<-0
        days<-0
        stoploss<-0
      
        next
      }
      #if we entered trade and hit the stop-loss then exit
      ####for the gapping case, consider a separate pricing scenario 
      else if((bottom<stoploss & stoploss<top) || (stoploss>bottom & stoploss>top)){
        sellprice <- stoploss
        pnl <- sellprice-buyprice
        strategyclosed<-1
        strategydata = rbind(strategydata, c(open,close,top,bottom,bar,barthreshold,strategyinit,strategycont,
                                             days,strategyentered,strategyclosed,enterrange,exitrange,stoploss,
                                             buyprice,sellprice,pnl
        ))   
        strategyinit <-0
        strategycont <-0
        strategyentered <-0
        strategyclosed<-0
        buyprice <- 0
        strategyclosed <-0
        sellprice <-0
        enterrange <-0
        exitrange <-0
        pnl<-0
        days<-0
        stoploss<-0
        next
    }
      #no exit prices have been reached, position remains, move onto the next day
       next 
    }
    
    
 #no trade has been entered now move on with trigger logic 
    
     #meets 10% criteria, new bar appears ignore last bar since we don't have trades entered
     if (bar>barthreshold){
      
       #start a new iteration 
       strategyinit <- 1
       #discontinue last iteration
       strategycont <- 0
       #set price enter ranges
   #    enterrangebottom<-quantile(c(open,close), .4)
    #   enterrangetop<- quantile(c(open,close), .6)
     enterrange<- mean(c(open,close))
     exitrange <- quantile(c(open,close), .95)
    
     #stoploss set a little lower than the bar 
     stoploss <- quantile(c(open,close),.0)*.99
      
       #initialize
       days <-0
       strategyentered <-0
       strategyclosed <-0
       
       #bar has been identified, we don't enter same day trade now exit the loop
       strategydata = rbind(strategydata, c(open,close,top,bottom,bar,barthreshold,strategyinit,strategycont,
                                            days,strategyentered,strategyclosed,enterrange,exitrange,stoploss,
                                            buyprice,sellprice,pnl
       ))
     next
     }
   
    #if it does not meet the 10%, either we have to buy in this block or new bar appears 
  else{
    
    #if the strategy triggered last day or we are still continuing 
    if((strategyinit == 1 || strategycont ==1) & days<10 & strategyentered ==0){
 #second day, kill the initialize trigger, and make continue 1
      strategyinit = 0 
      strategycont = 1
      
  #enterrange has been hit
      if(bottom<enterrange & enterrange<top)
      {
        strategyentered = 1
        buyprice <- enterrange
        
        #intraday trade model logic, this is imperfect
        if (close > exitrange){
          exitprice <- exitrange
          pnl <- exitprice - buyprice
          strategyclosed<-1
      
          #sell, reset the parameters and close the loop 
          strategydata = rbind(strategydata, c(open,close,top,bottom,bar,barthreshold,strategyinit,strategycont,
                                               days,strategyentered,strategyclosed,enterrange,exitrange,stoploss,
                                               buyprice,sellprice,pnl
          ))
          strategyinit <-0
          strategycont <-0
          strategyentered <-0
          strategyclosed<-0
          buyprice <- 0
          strategyclosed <-0
          sellprice <-0
          enterrange <-0
          exitrange <-0
          pnl<-0
          days<-0
          stoploss<-0
          
          next
          
      }
        #no intraday selling, but trade has been entered, now we move onto next day and hope it hits our prices
        strategyinit <-0
        strategycont <-0
        strategyentered <-1
        strategyclosed<-0
        sellprice <-0
        pnl<-0
        days<-0
        strategydata = rbind(strategydata, c(open,close,top,bottom,bar,barthreshold,strategyinit,strategycont,
                                             days,strategyentered,strategyclosed,enterrange,exitrange,stoploss,
                                             buyprice,sellprice,pnl
        ))
        next
        
      }
 #no strategy entered, we move onto the next day
    else{
      days = days +1
      strategycont = 1
      strategyinit = 0 
      
      strategydata = rbind(strategydata, c(open,close,top,bottom,bar,barthreshold,strategyinit,strategycont,
                                           days,strategyentered,strategyclosed,enterrange,exitrange,stoploss,
                                           buyprice,sellprice,pnl
      ))
      next
      
    }
    }
    
    #strategy never triggered or resetted due to 10 day rule
    else{
    strategycont = 0
    strategyinit = 0
    days = 0
    
    strategydata = rbind(strategydata, c(open,close,top,bottom,bar,barthreshold,strategyinit,strategycont,
                                         days,strategyentered,strategyclosed,enterrange,exitrange,stoploss,
                                         buyprice,sellprice,pnl
    ))
    next
    
    }

          }
      
    }
}
#end of for loop  
     

colnames(strategydata) <- c("open","close","top","bottom","bar","barthreshold","strategytriggered"
                            ,"strategycontd","days","strategyentered","strategyclosed","enterrange"
                            ,"exitrange","stoploss","buyprice","sellprice","PNL")


print(sum(strategydata$PNL))