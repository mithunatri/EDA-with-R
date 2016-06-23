this.dir <- getwd()
setwd(this.dir)

data<-read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))
data$day <- as.numeric("1")

for (i in 2:31){
  result <- read.csv(url(sprintf("http://stat.columbia.edu/~rachel/datasets/nyt%d.csv",i)))
  result$day<- as.numeric(i)
  data <- rbind(data, result)
}

#Categorize based on Age
data$Agecat<-cut(data$Age,c(-Inf,18,24,34,44,54,64,Inf),
                  labels=c("<18","18-24","25-34","35-44","45-54","55-64",">65"))
data$day <- as.numeric(gsub("[^[:digit:]]","",data$day))

data <- data[data$Impressions > 0,]
data$CTR <- data$Clicks/data$Impressions
data$Gender <- ifelse((data$Gender == 0),"Female","Male")

library(ggplot2)
ggplot(subset(data,Signed_In > 0 & Clicks > 0), aes(Agecat, CTR, fill=Gender)) + geom_boxplot()

library(doBy)
siterange <- function(x){c(min(x),mean(x),max(x))}
total <- function(x){sum(x)}

#Summary of Impressions and Clicks grouped by Age category
summary1<-summaryBy(Impressions+Clicks~Agecat, data=data, FUN=total, keep.names = TRUE)

#Calculate CTR for each Age Category
summary1$CTR = summary1$Clicks/summary1$Impressions

total_users <- data.frame(Sign_In_Status=c("Signed In","Not Signed In"),Total=c(nrow(data[data$Signed_In == 1,]),nrow(data[data$Signed_In == 0,])))

# Plot of Signed In vs not Signed In
ggplot(total_users, aes(Sign_In_Status,Total,fill = Sign_In_Status)) + geom_bar(stat = "identity") +
        xlab("Sign In Status") + ylab("Total Count")

#only signed in users have ages and genders. Additionally, we won't be able to
#make any inference from the clicks/impressions from people who have not signed in.
stats<-summaryBy(Impressions+Clicks+CTR~Agecat+Gender,
          data =subset(data, Signed_In>0 & Clicks>0), FUN=siterange)
write.table(stats, file="stats.csv")

temp <- summaryBy((Clicks/Impressions)~Agecat+order(day)+Gender, data = subset(data,Signed_In>0 & Clicks>0))
temp <- rename(temp, c("(Clicks/Impressions).mean"="Mean_CTR", "Agecat"="Age_Category","day" = "day"))

#Plot of CRT on each day faceted by Age Category
ggplot(temp, aes(day, Mean_CTR, color = Age_Category)) + 
        geom_point() + geom_line() + 
        facet_grid(Gender~Age_Category) +
        xlab("Day of the month") + ylab("Mean CTR")

data2 <- data[data$Signed_In > 0,]

data2$Female <- ifelse(grepl("Female",data2$Gender), 1, 0)
data2$Male <- ifelse(grepl("Male",data2$Gender), 1, 0)

MaleCount <- aggregate(data2$Male,by=list(Agecat=data2$Agecat),FUN=sum)
FemaleCount <- aggregate(data2$Female,by=list(Agecat=data2$Agecat),FUN=sum)

gender_count <- data.frame(Age_Category=MaleCount$Agecat, Gender=c("Male"), Count = MaleCount$x)
gender_count<-rbind(gender_count, data.frame(Age_Category=MaleCount$Agecat, Gender=c("Female"), Count = FemaleCount$x))
gender_count$Count<-ifelse(grepl("Male", gender_count$Gender),-1 * gender_count$Count, 1 * gender_count$Count)

#Pyramid of Number of Users vs Age Category
ggplot(gender_count, aes(Age_Category, Count, fill=Gender)) + 
  geom_bar(data=gender_count[gender_count$Gender == "Female",], stat = "identity") + 
  geom_bar(data=gender_count[gender_count$Gender == "Male",], stat = "identity") + coord_flip() + 
  scale_fill_brewer(palette = "Set1") + theme_bw() + 
  scale_y_continuous(breaks = seq(-1200000, 1200000, 200000),labels = c("1.2m","1m","800K","600k","400k","200K","0","200k","400k","600k","800k","1m","1.2m")) + 
  ylab("Number of Users") + xlab("Age Category")


