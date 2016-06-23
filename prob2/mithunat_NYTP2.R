data1<-read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

#categorize
head(data1)
data1$Agecat<-cut(data1$Age,c(-Inf,18,24,34,44,54,64,Inf),
                  labels=c("<18","18-24","25-34","35-44","45-54","55-64",">65"))
#view
summary(data1)

install.packages("doBy")
library(doBy)
siterange <- function(x){c(length(x),min(x),mean(x),max(x))}
total <- function(x){sum(x)}

summaryBy(Age~Agecat, data=data1, FUN=siterange)

summary1<-summaryBy(Impressions+Clicks~Agecat, data=data1, FUN=total, keep.names = TRUE)

#Calculate CRT for each Age Category
summary1$CRT = summary1$Clicks/summary1$Impressions

#only signed in users have ages and genders
summaryBy(Signed_In+Impressions+Clicks~Agecat+Gender,
          data =data1)

#plot
install.packages("ggplot2")
library(ggplot2)
ggplot(data1, aes(x=Impressions, fill=Agecat))+geom_histogram(binwidth=1)
ggplot(data1, aes(x=Agecat, y=Impressions, fill=Agecat))+geom_boxplot()

#plot of Age Category vs. Impressions
ggplot(summary1, aes(x=Agecat, y=Impressions, fill=Agecat)) + 
  geom_bar(stat = "identity", width = 0.5) + xlab("Age Category") + 
  labs(title = "Age Category vs. Number of Impressions") + 
  scale_y_continuous(labels=function(n){format(n, scientific=FALSE)})
  
# plot of Age Category vs. CRT
ggplot(summary1, aes(x=Agecat, y=CRT, fill=Agecat)) + geom_bar(stat = "identity", width = 0.5) + 
  xlab("Age Category") + labs(title = "Age Category vs. CRT")

#New segments to categorize based on click behaviour
#Drop all rows where Impresions = 0 by creating new data frame
#as clicks will only be present when Impressions is not 0.
data2 <- data1[!(data1$Impressions == 0),]

# create categories
data2$scode[data2$Clicks==0] <- "False"
data2$scode[data2$Clicks>0] <- "True"

summary3 <- summaryBy(Impressions+Clicks~Agecat+Gender, data=data2, FUN=total, keep.names = TRUE)
summary3$CRT <- summary3$Clicks/summary3$Impressions

#Plot of Agecat vs Clicks based on Gender
ggplot(summary3, aes(x=Agecat, y=Clicks, shape = factor(Gender))) + 
  geom_point(aes(color=Gender, size=Clicks*2)) + guides(color = FALSE, size=FALSE) + 
  scale_shape_discrete(breaks=c(0,1), labels=c("Male","Female"))
  
#Plot of CRT vs. Impressions based on Gender
ggplot(summary3, aes(x=CRT, y=Impressions, shape = factor(Gender))) + geom_line(aes(color=Gender)) + 
  scale_color_continuous(breaks=c(0,1), labels=c("Male","Female"))

        
