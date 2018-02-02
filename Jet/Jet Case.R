library(readxl)
library(plyr)
library(ggplot2)

##Import Combined Data
Merchant_Data <- read_excel("C:/Users/James/Desktop/Programming/R Environment/Analytics_Interview_Case_-_With_Sales_Data.xlsx", 
                         sheet = "Merchant Data")

#Get mean price for each JET SKU ID
SKU_ID_Avg <- setNames(aggregate(Merchant_Data[, 4],Merchant_Data[,3],mean ),c("Jet SKU ID", "Average_Price"))

#Combine average prices into the original Table
Merchant_Data_With_Avg <- (merge(Merchant_Data,SKU_ID_Avg, by = 'Jet SKU ID'))

#Compute % deviations of prices from the means
Merchant_Data_With_Avg_Ndeviations <- within(Merchant_Data_With_Avg, 
                                            Perc_Deviation <-
                                              (Price - Average_Price)/Average_Price)
#ANSWER TO #1
##Calculate Average deviations by Merchant
Merch_Perc_Deviation_Avg <- setNames(aggregate(Merchant_Data_With_Avg_Ndeviations$Perc_Deviation,
                                      by= list(Merchant = Merchant_Data_With_Avg_Ndeviations$Merchant),
                                      mean),c("Merchant", "Average_Price_Deviation"))
        
#1.b Weighted Average by Average Price * Sales
#Calculate Weights
Merchant_Data_With_Avg_Ndeviations <- within(Merchant_Data_With_Avg_Ndeviations,  
                                             Weight <- Price * Sales)

#Perform weighted average analysis
ddply(na.omit(Merchant_Data_With_Avg_Ndeviations), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))
  
#ANsWER TO #2
#2. Calculate Standard Deviations on Product Level

  
 stdev<- ddply(na.omit(Merchant_Data_With_Avg_Ndeviations), .(Merchant_Data_With_Avg_Ndeviations$`Jet SKU ID`), summarize, 
        stdev = sd(Price))
  
                       
#3

#Calculate Vendor Competitiveness by Category

ddply(na.omit(Merchant_Data_With_Avg_Ndeviations), c(.(Category),.(Merchant)), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))

#Plotting Price Deviations vs Sales 

Leos_Bodega <- Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Merchant == 'Leos Bodega',] 
Alex_Store <- Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Merchant == 'Alexs Store',] 
Jasmines_Shop <- Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Merchant == 'Jasmines Shop',] 

#Plot overall 
qplot(Sales, Perc_Deviation
      ,colour = Category, shape = Merchant,
      main = "All Vendors Percentage Price Deviations from Avg Price vs Sales",
      data = Merchant_Data_With_Avg_Ndeviations
      )

#Plot Jasmine's
qplot(Sales, Perc_Deviation
      ,colour = Category,
      main = "Jasmine's Percentage Price Deviations from Avg Price vs Sales",
      data = Jasmines_Shop)

#Plot Alex's
qplot(Sales, Perc_Deviation
      ,colour = Category,
      main = "Alex's Store Percentage Price Deviations from Avg Price vs Sales",
      data = Alex_Store)

#Plot Leo's
qplot(Sales, Perc_Deviation
      ,colour = Category,
      main = "Leo's Percentage Price Deviations from Avg Price vs Sales",
      data = Leos_Bodega)

#Calculate Vendor Competitiveness Amongst Clusters

ddply(na.omit(Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Sales <200 &
                Merchant_Data_With_Avg_Ndeviations$Category=='Electronics',]), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))

ddply(na.omit(Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Sales >=200 &
                                                   Merchant_Data_With_Avg_Ndeviations$Category=='Electronics',]), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))

ddply(na.omit(Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Sales <350 &
                                                   Merchant_Data_With_Avg_Ndeviations$Category=='Home',]), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))

ddply(na.omit(Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Sales >=350 &
                                                   Merchant_Data_With_Avg_Ndeviations$Category=='Home',]), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))


ddply(na.omit(Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Sales <500 &
                                                   Merchant_Data_With_Avg_Ndeviations$Category=='Consumables',]), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))


ddply(na.omit(Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Sales >=500 &
                                                   Merchant_Data_With_Avg_Ndeviations$Sales <=600 &
                                                   Merchant_Data_With_Avg_Ndeviations$Category=='Consumables',]), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))


ddply(na.omit(Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Sales >=1000 &
                                                   Merchant_Data_With_Avg_Ndeviations$Sales <=1100 &
                                                   Merchant_Data_With_Avg_Ndeviations$Category=='Consumables',]), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))



ddply(na.omit(Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Sales >1100 &
                                                   Merchant_Data_With_Avg_Ndeviations$Category=='Consumables',]), .(Merchant), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))


##kmeans Calculation

#Isolate Electronics Data
ElectronicsData <- Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Category=='Electronics',]

#Identify Appropriate Number of Clusters
mydata <-ElectronicsData
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata[,c(6,8)],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters for Electronics",
     pch=20, cex=2)

#Group data into best number of clusters
KCluster <- kmeans( ElectronicsData[,c(6)]
                    , 5, nstart = 20)

#Plot the data with clusters
qplot(Sales, Perc_Deviation
      , color = KCluster$cluster,
      data =ElectronicsData
)

ElectronicsData$Cluster <- KCluster$cluster
ddply(na.omit(ElectronicsData), c(.(Cluster),.(Merchant)), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))


##Consumables##
#Isolate Consumables Data
ConsumablesData <- Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Category=='Consumables',]

#Identify Appropriate Number of Clusters
mydata <-ConsumablesData
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata[,c(6)],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters for Consumables",
     pch=20, cex=2)

#Group data into best number of clusters
KCluster <- kmeans( ConsumablesData[,c(6)]
                    ,5,nstart = 20)

#Plot the data with clusters
qplot(Sales, Perc_Deviation
      , color = KCluster$cluster,
      data =ConsumablesData
)


KCluster$centers


#Calculate Vendors Competitiveness in Each Clusters
ConsumablesData$Cluster <- KCluster$cluster
ddply(na.omit(ConsumablesData), c(.(Cluster),.(Merchant)), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))


##HOME##
#Isolate Home Data
HomeData <- Merchant_Data_With_Avg_Ndeviations[Merchant_Data_With_Avg_Ndeviations$Category=='Home',]

#Identify Appropriate Number of Clusters
mydata <-HomeData
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata[,c(6)],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters for Home",
     pch=20, cex=2)

#Group data into best number of clusters
KCluster <- kmeans( HomeData[,c(6,8)]
                    , 2, nstart = 20)

#Plot the data with clusters
qplot(Sales, Perc_Deviation
      , color = KCluster$cluster,
      data =HomeData
)


#Calculate Vendors Competitiveness in Each Clusters
HomeData$Cluster <- KCluster$cluster
ddply(na.omit(HomeData), c(.(Cluster),.(Merchant)), summarize, 
      mean = mean(Perc_Deviation),
      wmean = weighted.mean(Perc_Deviation, w=Weight))


  