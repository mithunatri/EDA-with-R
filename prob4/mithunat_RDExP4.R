library(jsonlite)
library(ggplot2)

refactor_data <- function() {
  #Remove all URLs within text
  tweets_text <- gsub("http[^[:space:]]*", "", result$text)
  tweets.df <- data.frame(matrix(unlist(tweets_text), byrow=T),stringsAsFactors=FALSE)
  
  tweets.df<-rename(tweets.df,c("matrix.unlist.tweets_text...byrow...T."="Tweets"))
  #Remove tweets with same textual content
  tweets.df <- subset(tweets.df, !duplicated(tweets.df$Tweets))
  
  attach(tweets.df)
  #Create a new column which identifies whether user is prospective tenant or Landlord.
  tweets.df$type <- ifelse(grepl("want|need|look", Tweets), "T", ifelse(grep("renting|for rent|lease", Tweets), "L","NA"))
  
  #Create a new column which identifies the locality.
  tweets.df$Locality[grepl("brooklyn",Tweets, ignore.case = TRUE)] <- "Brooklyn"
  tweets.df$Locality[grepl("manhat|#rentals",Tweets,ignore.case = TRUE)] <- "Manhattan"
  tweets.df$Locality[grepl("queen",Tweets, ignore.case = TRUE)] <- "Queens"
  tweets.df$Locality[grepl("bronx",Tweets, ignore.case = TRUE)] <- "Bronx"
  tweets.df$Locality <- ifelse(!grepl("Manhattan|Brooklyn|Queens|Bronx", tweets.df$Locality), "New York", tweets.df$Locality)
  
  #Create column for Price (if listed)
  tweets.df$Price <- regmatches(Tweets,gregexpr("\\$[0-9,.]+", Tweets))
  tweets.df$Price<-ifelse(grepl("character", tweets.df$Price), "NA", tweets.df$Price)
  for (i in 1:nrow(tweets.df)){
    tweets.df$Price[i] <- unlist(tweets.df$Price[i])[1]
  }
  tweets.df$Price.n <- as.numeric(gsub("[^[:digit:]]","",tweets.df$Price))
  
  
  detach(tweets.df)
  return (tweets.df)
}

#Get list of files ending with .json in current
#working directory
files <- list.files(pattern = "*.json")

json <- fromJSON(files[1])[,1]

for (i in 2:length(files)){
  json <- c(json,fromJSON(files[i])[,1])
}

library(plyr)
result <- data.frame(matrix(unlist(json), byrow = T), stringsAsFactors = FALSE)
result <-rename(result,c("matrix.unlist.json...byrow...T."="text"))

#Clean the data
data<-refactor_data()

#Plot of Rent Distribution in New York City across three Boroughs
ggplot(subset(data, data$Price.n>500 & data$Price.n != "NA" & data$Price.n < 10000), aes(Locality, Price.n, color=Locality)) + 
        geom_boxplot() + xlab("Locality") + ylab("Price ($)") + 
        ggtitle("Distribution of Rental Costs across Localities")

#---A Map plot to show the distribution of Landlord and Tenants area wise----------------------------#
library(doBy)
total <- summaryBy(Tweets~Locality+type, data=data, FUN=function(x){length(x)})
total <- rename(total,c("Tweets.function(x) {     length(x) }"="Total"))
total <- subset(total, total$Locality != "New York")

total$lon <- geocode(total$Locality)$lon
total$lat <- geocode(total$Locality)$lat
total$color <- ifelse(total$type == "L","darkgreen","blue")

library(ggmap)
nyc <- get_map(location = c(lon = -73.9, lat=40.7), zoom=11, maptype = "roadmap")
ggmap(nyc, legend = "left") + 
  geom_point(data = total, aes(total$lon, total$lat), alpha=0.7,color=total$color,size=total$Total/2.5)
#-----------------------------------------------------------------------------------------------------#      

library(doBy)
library(plyr)

total2 <- summaryBy(Tweets~Locality, data=data, FUN=function(x){length(x)})
total2<- rename(total2, c("Tweets.function(x) {     length(x) }"="Total"))
total2$mean <- summaryBy(Price.n~Locality,data=subset(data, data$Price.n > 0 & data$Price.n < 100000 & data$Price.n != 'NA'))[,2]
#Potential revenue per month. Current revevnue * 4 (4 weeks per month)
total2$revenue <- total2$Total*total2$mean * 4
write.table(total2, file="rental_total_locality.csv")

library(ggplot2)
ggplot(total2, aes(Locality, Total,fill=Locality)) + geom_bar(stat="identity")
  
ggplot(total2, aes(Locality, revenue, fill=Locality)) + geom_bar(width = 1, stat="identity") + 
  coord_polar(theta ="y") + 
  scale_y_continuous(labels=function(n){format(n, scientific=FALSE)}) + 
  ylab("Potential Revenue/Month ($)") + xlab("Locality") + 
  ggtitle("Potential Revenue through rentals/month")
 
