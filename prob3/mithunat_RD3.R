require(gdata)

#Set "this.dir" to folder where brooklyn data set is present
this.dir <- "/home/mithun/Desktop/UB_Spring/DIC/EDA1/prob3/Brooklyn"
setwd(this.dir)

bk <- read.xls("rollingsales_brooklyn.xls",pattern="BOROUGH")
head(bk)
summary(bk)
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
count(is.na(bk$SALE.PRICE.N))
names(bk) <- tolower(names(bk))

## clean/format the data with regular expressions
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                 bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date)
bk$year.built <- as.numeric(as.character(bk$year.built))

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices

attach(bk)
hist(sale.price.n)
hist(sale.price.n[sale.price.n>0])
hist(gross.sqft[sale.price.n==0])
detach(bk)

stats <- function(x){c(min(x),mean(x),max(x))}
## keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]

## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]

#Plot of of Area vs Sale Price for One, Two and Three Family Homes in Brooklyn (Log Scale)
bk.homes[which(bk.homes$sale.price.n<100000),][order(bk.homes[which(bk.homes$sale.price.n<100000),]$sale.price.n),]

## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]

library(ggplot2)
#Plot of log(gross.sqft) vs log(sale.price) 
ggplot(bk.homes,aes(log(gross.sqft),log(sale.price.n))) + geom_point() + 
				ylab("Sale Price (Log)") + xlab("Gross Area (Log)")

#Plot of distribution of One, Two and Three Family homes in Brooklyn
ggplot(bk.homes, aes(building.class.category,log(sale.price.n))) + 
			geom_boxplot() + 
			xlab("Building Category") + 
			ylab("Sale Price(Log)")

#Plot of distrubtion of family housing prices across neighbourhoods
ggplot(bk.homes, aes(neighborhood,log(sale.price.n))) + geom_boxplot() + 
				xlab("Building Category") + 
				ylab("Sale Price(Log)") + 
				coord_flip()

bk.sale <- bk.sale[bk.sale$gross.sqft>0,][bk.sale$land.sqft>0,]
library(doBy)
bk.area <- summaryBy((sale.price.n/gross.sqft)~building.class.category, data = bk.sale)
library(plyr)
bk.area <- rename(bk.area, c("building.class.category"="building.category","(sale.price.n/gross.sqft).mean"="price.sq.ft"))


#Average Price/sq.ft. of different building category in Brooklyn across all neighborhoods
ggplot(bk.area, aes(price.sq.ft, building.category)) + geom_point() + 
			xlab("Average Price/Sq.ft") + 
			ylab("Building Category")
       
