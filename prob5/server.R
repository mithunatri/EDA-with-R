library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observe({
    invalidateLater(300000, session)
    refactor_data <- function(x){
      json_data <- json_data[,c("text","retweet_count","created_at")]
      json_data$text <- ifelse(grepl("http",json_data$text), gsub("http[^[:space:]]*", "", json_data$text),json_data$text)
      json_data <- json_data[!duplicated(json_data$text),]
      print("start")
      json_data$created <- ""
      for (i in 1:nrow(json_data)){
        temp_str <- unlist(strsplit(gsub(" ",",",json_data$created_at[i]),",")[1])
        month <- temp_str[2]
        day <- temp_str[3]
        year <- temp_str[6]
        #print(as.Date(paste0(c(month,day,year), collapse="-"),format="%b-%d-%Y"))
        json_data$created[i] <- as.character(as.Date(paste0(c(month,day,year), collapse="-"),format="%b-%d-%Y"))
      }
      print("End")
      #json_data$created <- as.Date(json_data$created)
      
      json_data$mention <- "NA"
      json_data$mention[grepl("Clinton", json_data$text,ignore.case = T) & json_data$mention == "NA"] <- "Clinton"
      json_data$mention[grepl("Trump", json_data$text,ignore.case = T) & json_data$mention == "NA"] <- "Trump"
      json_data$mention[grepl("Sanders", json_data$text,ignore.case = T) & json_data$mention == "NA"] <- "Sanders"
      json_data$mention[grepl("Rubio", json_data$text,ignore.case = T) & json_data$mention == "NA"] <- "Rubio"
      json_data$mention[grepl("Cruz", json_data$text,ignore.case = T) & json_data$mention == "NA"] <- "Cruz"
      json_data$mention[grepl("Carson", json_data$text,ignore.case = T) & json_data$mention == "NA"] <- "Carson"
      
      json_data$Party <- ifelse(grepl("Clinton|Sanders",json_data$mention),"Democrats","Republicans")
      
      return (json_data)
    }
  
    file = "./Alternate/elections.json"   #Change location of stream file here.
    
    stream_tweets <- function(x){
      library(streamR)
      library(ROAuth)
      
      requestURL <- "https://api.twitter.com/oauth/request_token"
      accessURL <- "https://api.twitter.com/oauth/access_token"
      authURL <- "https://api.twitter.com/oauth/authorize"
      
      #Authenticators
      consumerKey <- ""
      consumerSecret <- ""
      
      if(!file.exists("my_oauth.Rdata")){
        my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret,requestURL = requestURL, accessURL = accessURL, authURL = authURL)
        my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
        save(my_oauth, file = "my_oauth.Rdata")
      }
      
      load("my_oauth.Rdata")
      filterStream(file, track = c("clinton", "trump","rubio","cruz","carson","sanders","polls"), timeout = 30,oauth = my_oauth)
      
    }
    
    library(streamR)
    #Fetch twitter data and append to existing file
    stream_tweets()
    #Parse tweets
    input_data <- parseTweets("./Alternate/elections.json")
    json_data <- input_data
    json_data <- refactor_data(json_data)
    #save(json_data, file="election_data.RData")
    
    #Most Active Plot
    output$most.active <- renderPlot({
      
      library(doBy)
      candidate.files <- list.files(path="./Candidates/", pattern="*.json")
      candidate_data <- NULL
      for (i in 1:length(candidate.files)){
        temp <- jsonlite::fromJSON(paste0("./Candidates/",candidate.files[i]))[,c("text","created","retweetCount")]
        temp$candidate <- regmatches(candidate.files[i],regexpr("^([^.])+(?=\\.)",candidate.files[i],perl = TRUE))
        candidate_data <- rbind(candidate_data,temp)
      }
      candidate_data$created <- as.Date(candidate_data$created)
      most.active <<- summaryBy(text~candidate+created, 
                                data=with(candidate_data,candidate_data[(created >= "2016-02-01"),]), 
                                FUN=function(x){length(x)}, keep.names = T)
      
      candidate <- switch(as.numeric(input$select),
                          "*",
                          "clinton","sanders","trump","rubio","cruz","carson")
      
      library(ggplot2)
      ggplot(most.active[(grepl(candidate,most.active$candidate)),], aes(created,text, color = candidate)) + geom_point() + geom_line() +
        xlab("") + ylab("Number of tweets")
    })
    
    #Popularity Plot
    output$popularity_plot <- renderPlot({
      
      library(doBy)
      mentions <- summaryBy(text~mention+created, data=subset(json_data,json_data$mention != "NA"), FUN=function(x){length(x)})
      library(plyr)
      mentions <- rename(mentions,c("mention"="Candidate","text.function(x) {     length(x) }"="count"))
      
      library(ggplot2)
      
      candidate <- switch(as.numeric(input$radio_button),
                          "*",
                          "Clinton","Sanders","Trump","Rubio","Cruz","Carson")
      
      ggplot(subset(mentions, grepl(candidate,Candidate)) ,aes(created,count, color=Candidate, group=Candidate)) + geom_point() + geom_line() + 
        xlab("") + ylab("Number of mentions")
    })
    
    #Wordcloud
    output$word_cloud <- renderPlot({
      
      #Reference: Wordcloud http://www.r-bloggers.com/building-wordclouds-in-r/
      library(tm)
      library(wordcloud)
      
      data <- json_data[grepl(input$search,json_data$text),]
      data <- gsub("[[:digit:]]","",data)
      corpus <- Corpus(VectorSource(iconv(data, "latin1","ASCII",sub="")))
      corpus <- tm_map(corpus,PlainTextDocument)
      corpus <- tm_map(corpus, removePunctuation)
      #corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removeWords, c("dont","know","can","now","like",
                                              "this","just","the","get","hes",stopwords('english')))
      col=brewer.pal(6,"Dark2")
      wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
                random.color=T, max.word=45, random.order=F,colors=col)
      
      
    })
    
    #Hashtag
    output$hashtag_table <- renderTable({
      #load("election_data.RData")
      sub <- subset(json_data, json_data$created == as.character(input$date))
      hashtags <- unlist(regmatches(sub$text,gregexpr("#(\\d|\\w)+",sub$text)))  
      top <- aggregate(data.frame(count = hashtags), list(value = hashtags), length)
      top <- aggregate(data.frame(count = hashtags), list(value = hashtags), length)
      library(plyr)
      top<-as.data.frame(top[order(-top$count),][1:5,1])
      top <- rename(top,c("top[order(-top$count), ][1:5, 1]"="Hashtag"))
      top
    })
    
  })  
})

