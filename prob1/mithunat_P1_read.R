library(jsonlite)

#Get list of files ending with .json in current
#working directory
this.dir <- "/home/mithun/Desktop/UB_Spring/DIC/EDA1/prob1"
setwd(this.dir)

files <- list.files(pattern = "*.json")

result <- NULL
for (i in 1:length(files)){
  json <- fromJSON(files[i])
  result <- rbind(result, json)
  remove(json)
}

refactor_data <- function() {
  #Remove all URLs within text
  tweets_text <- gsub("http[^[:space:]]*", "", tweets$text)
  tweets.df <- data.frame(matrix(unlist(tweets_text), byrow=T),stringsAsFactors=FALSE)
  library(plyr)
  tweets.df<-rename(tweets.df,c("matrix.unlist.tweets_text...byrow...T."="Tweets"))
  #Remove tweets with same textual content
  tweets.df <- subset(tweets.df, !duplicated(tweets.df$Tweets))
  
  attach(tweets.df)
  #Create a new column which identifies whether user is prospective tenant or Landlord.
  tweets.df$type <- ifelse(grepl("want|need|look", Tweets), "T", ifelse(grep("renting|for rent|lease", Tweets), "L","NA"))
  
  #Create a new column which identifies the locality.
  tweets.df$Locality[grepl("brooklyn",Tweets, ignore.case = TRUE)] <- "Brooklyn"
  tweets.df$Locality[grepl("manhat",Tweets,ignore.case = TRUE)] <- "Manhattan"
  tweets.df$Locality[grepl("queen",Tweets, ignore.case = TRUE)] <- "Queens"
  
  #Create column for Price (if listed)
  tweets.df$Price <- regmatches(Tweets,gregexpr("\\$[0-9]*", Tweets))
  tweets.df$Price<-ifelse(grepl("character", tweets.df$Price), "NA", tweets.df$Price)

  detach(tweets)
}

data <- refactor_data()


