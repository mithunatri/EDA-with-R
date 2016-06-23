require(gdata)

#The files must be present in the same folder as the script.
borough_list <- c("Manhattan","Bronx","Queens", "Statenisland")
borough_data <- read.xls("rollingsales_brooklyn.xls",pattern="BOROUGH")
borough_data$borough_name = "Brooklyn"
#Create data frame containing info for all boroughs  
for (borough in borough_list){
    filename = sprintf("rollingsales_%s.xls",tolower(borough))
    result <- read.xls(filename, pattern="BOROUGH")
    result$borough_name = borough
    borough_data <- rbind(borough_data, result)
}

borough_data$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",borough_data$SALE.PRICE))
names(borough_data) <- tolower(names(borough_data))

## clean/format the data with regular expressions
borough_data$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                 borough_data$gross.square.feet))
borough_data$land.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                borough_data$land.square.feet))
borough_data$sale.date <- as.Date(borough_data$sale.date)
borough_data$year.built <- as.numeric(as.character(borough_data$year.built))

library(ggplot2)
## keep only the actual sales
borough_data.sale <- borough_data[borough_data$sale.price.n!=0,]
#Houses which sold for less than $10000 in any of the boroughs seem unreal. So we will ignore that data
borough_data.sale <- borough_data.sale[borough_data.sale$sale.price.n>10000,]
ggplot(borough_data.sale,aes(log(gross.sqft),log(sale.price.n))) + 
    geom_point() +
    facet_grid(.~borough_name) + xlab("Gross Sqft (Log)") + ylab("Sale Price (Log)")
  

## for now, let's look at 1-, 2-, and 3-family homes
borough_data.homes <- borough_data.sale[which(grepl("FAMILY",borough_data.sale$building.class.category)),]
borough_data.homes[which(borough_data.homes$sale.price.n<100000),][order(borough_data.homes[which(borough_data.homes$sale.price.n<100000),]$sale.price.n),]

## remove outliers that seem like they weren't actual sales
borough_data.homes$outliers <- (log(borough_data.homes$sale.price.n) <=5) + 0
borough_data.homes <- borough_data.homes[which(borough_data.homes$outliers==0),]

stats <- function(x){c(min(x),mean(x),max(x))}

library(doBy)

summaryBy(sale.price.n~borough_name+building.class.category, data=borough_data.homes, FUN=stats)

ggplot(borough_data.homes, aes(borough_name, log(sale.price.n), group = borough_name, color = borough_name)) + geom_boxplot() + 
          facet_grid(.~building.class.category) + xlab("Boroughs") + ylab("Sales Price (Log)")

ggplot(borough_data.sale, aes(building.class.category, log(sale.price.n))) + 
    geom_boxplot() + coord_flip() + facet_grid(.~borough_name) + ylab("Sale Price (Log)") + xlab("Building Category")

borough_data.sale <- borough_data.sale[borough_data.sale$gross.sqft>0,][borough_data.sale$land.sqft>0,]
bk.area <- summaryBy((sale.price.n/gross.sqft)~building.class.category+borough_name, data = borough_data.sale)
library(plyr)
bk.area <- rename(bk.area, c("building.class.category"="building.category","(sale.price.n/gross.sqft).mean"="price.sq.ft","borough_name"="borough_name"))
bk.area <- bk.area[-nrow(bk.area),]

#Average Price/sq.ft. of different building category in Brooklyn across all neighborhoods
ggplot(bk.area, aes(price.sq.ft, building.category)) + geom_point() + xlab("Average Price/Sq.ft") +
         facet_grid(.~borough_name)

ggplot(borough_data.homes, aes(x=borough_name, fill = borough_name)) + geom_bar() + facet_grid(.~building.class.category)

