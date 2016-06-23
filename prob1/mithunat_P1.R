library(ROAuth)
library(RJSONIO)
library(RCurl)
library(twitteR)
library(stringr)
library(jsonlite)


# Declare Twitter API Credentials------------------------------------------------------
api_key <- ""
api_secret <- ""
token <- "" # From dev.twitter.com
token_secret <- ""
#--------------------------------------------------------------------------------------

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

#Search twitter
twitter_result<-searchTwitter("house rent queens OR brooklyn OR manhattan OR bronx",n=300,lang="en")

#Convert list obtained from searchTwitter to a Data Frame
tweets.df<-twListToDF(twitter_result)

#Convert data frame to JSON data for storage
tweets <- toJSON(tweets.df)

#Write to file with Date as suffix in the current working directory.
path <- paste(c(getwd(),"rent_2016-02-28"),collapse="/")
#filename <- paste(c(path, as.character(Sys.Date()), "json"),collapse=".")
filename <- paste(c(path,Sys.Date(), "json"),collapse=".")
write(tweets, file=filename)


#---------------------------------------------------------------------------------------------------
#Extract tweets on specific users timeline. In this case, real estate agents.
#This will be a one-time extraction.
user <- userTimeline('bklynrentals', n=300)
user <- twListToDF(user)
user <- toJSON(user)
path2 <- paste(c(getwd(),"twitter_bklynrentals"),collapse="/")
filename2 <- paste(c(path2, as.character(Sys.Date()), "json"),collapse=".")
write(user, file=filename2)
#---------------------------------------------------------------------------------------------------



