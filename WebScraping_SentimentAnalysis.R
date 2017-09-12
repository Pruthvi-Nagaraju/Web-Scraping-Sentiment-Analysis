
### Code to extract reviews from Trip advisor

install.packages("rvest")
library(rvest)
url <- 'https://www.tripadvisor.com/Airline_Review-d8729060-Reviews-Cheap-Flights-Delta-Air-Lines'
webpage <- read_html(url)
text <- html_nodes(x = webpage, css = ".entry") %>% html_text()
title <- html_nodes(x = webpage, css = ".quote") %>% html_text()
date <- html_nodes(x = webpage, css = ".ratingDate.relativeDate") %>% html_attr("title")
rating <-  html_nodes(x = webpage, css = ".rating.reviewItemInline") %>% html_children() %>% html_attr("class")
rating <- rating[grepl("ui",rating)]
rating <- as.numeric(substr(rating,nchar("ui_bubble_rating bubble_10")-1,nchar("ui_bubble_rating bubble_10")))

data_frame <- data.frame(title,text,date,rating)

sequence_webpage <- seq(10,15290,10)
for(i in 828:length(sequence_webpage)){
  x <- sequence_webpage[i]
  url2 <- paste("https://www.tripadvisor.com/Airline_Review-d8729060-Reviews-Cheap-Flights-or",x,"-Delta-Air-Lines#REVIEWS")
  url2 <- gsub(" ", "", url2) 
  webpage2 <- read_html(url2)
  text <- html_nodes(x = webpage2, css = ".entry") %>% html_text()
  title <- html_nodes(x = webpage2, css = ".quote") %>% html_text()
  date <- ifelse(i <200,html_nodes(x = webpage2, css = ".ratingDate.relativeDate") %>% html_attr("title"),
                 html_nodes(x = webpage2, css = ".ratingDate") %>% html_text())
  #date <- html_nodes(x = webpage2, css = ".ratingDate") %>% html_text()
  rating <-  html_nodes(x = webpage2, css = ".rating.reviewItemInline") %>% html_children() %>% html_attr("class")
  rating <- rating[grepl("ui",rating)]
  rating <- as.numeric(substr(rating,nchar("ui_bubble_rating bubble_10")-1,nchar("ui_bubble_rating bubble_10")))
  df<- data.frame(title,text,date,rating)
  data_frame <- rbind(data_frame,df)
  
}

head(data_frame)
summary(data_frame)
write.csv(data_frame,"detla_reviews_Kokila.csv")

data_frame


### Code to find the Polarity 

library(tm)
library(dplyr)
library(plyr)
library(stringr)
library(readxl)
reviews_text <- read_excel("C:/SummerProjects/detla_reviews_Kokila.xlsx")
data=read_excel("C:/SummerProjects/detla_reviews_Kokila.xlsx")

setwd("C:/SummerProjects/")

reviews_text <- unlist(lapply(reviews_text$text,function(x) gsub("^\\s+|\\s+$", "",x)))
reviews_text <- unlist(lapply(reviews_text,function(x) gsub("[^0-9A-Za-z@///' ]", "",x)))
reviews_text <- unlist(lapply(reviews_text,function(x) gsub("[[:digit:]]", "",x)))
reviews_text <- unlist(lapply(reviews_text,function(x) gsub("[[:punct:]]", "",x)))
head(review_text_rm)
class(reviews_text)

#removing stop words
rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
review_text_rm =unlist(rm_words(reviews_text, tm::stopwords("en")))
class(review_text_rm)

polarity_score = function(tweets, positive, negative)
{
  score =laply(tweets, function(tweet, positive, negative)
  {
    words = str_split(tweet,"\\s+")
    words = unlist(words)
    positive_overlap = match(words, positive) 
    negative_overlap= match(words, negative) 
    positive_overlap =!is.na(positive_overlap) 
    negative_overlap= !is.na(negative_overlap)
    score =sum(positive_overlap) - sum(negative_overlap)
    return(score)
  }, positive, negative)
  scores =data.frame(score=score, text=tweets)
  return(scores)
}

positive <- scan('C:/SummerProjects/Positive-Word.txt', what='character', comment.char=';')
negative <- scan('C:/SummerProjects/negative-words.txt', what='character', comment.char=';') 

#reviews_text_df=data.frame(text = sapply(reviews_text, as.character), stringsAsFactors = FALSE)
Sentiment_scores <- polarity_score(review_text_rm, positive, negative)
head(Sentiment_scores)
Sentiment_scores$score  #polarity score


data$PolarityScore=Sentiment_scores$score

library(xlsx)

write.xlsx(x = data, file = "withPolarity.xlsx")

### Code to find the cluster / Topic modelling

install.packages("readxl")
install.packages("RSiteCatalyst")
install.packages("RTextTools")

library("readxl")
library("RSiteCatalyst")
library("RTextTools")
library("tm")
ad = read_excel("R:/Study Material/RA - Project/withPolarity.xlsx",sheet = "Raw")
View(ad)
(ad$text)


#### 2. Process keywords into format suitable for text mining

#Create document-term matrix, passing data cleaning options
#Stem the words to avoid multiples of similar words
#Need to set wordLength to minimum of 1 because "r" a likely term
dtm <- create_matrix(ad$text, 
                     stemWords=TRUE, 
                     removeStopwords=FALSE, 
                     minWordLength=1,
                     removePunctuation= TRUE)

findFreqTerms(dtm, lowfreq=20)


mydata <- dtm
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


kmeans5<- kmeans(dtm, 5)

View(as.matrix(dtm))

nrow((as.data.frame(kmeans5$cluster)))
View((as.data.frame(kmeans5$cluster)))

#Merge cluster assignment back to keywords
kw_with_cluster <- as.data.frame(cbind(ad$text, kmeans5$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans5")

View(kw_with_cluster)
#Make df for each cluster result, quickly "eyeball" results
cluster1 <- subset(kw_with_cluster, subset=kmeans5 == 1)
cluster2 <- subset(kw_with_cluster, subset=kmeans5 == 2)
cluster3 <- subset(kw_with_cluster, subset=kmeans5 == 3)
cluster4 <- subset(kw_with_cluster, subset=kmeans5 == 4)
cluster5 <- subset(kw_with_cluster, subset=kmeans5 == 5)

stat= function(a = cluster1,b = cluster2,c=cluster3,d=cluster4,e=cluster5)
{
  a=nrow(cluster1)
  b=nrow(cluster2)
  c=nrow(cluster3)
  d=nrow(cluster4)
  e=nrow(cluster5)
  t = a+b+c+d+e
  a1=a*100/t
  a2=b*100/t
  a3=c*100/t
  a4=d*100/t
  a5=e*100/t
  s = c(a1,a2,a3,a4,a5)
  return(s)
}

stat()

ad$cluster = kw_with_cluster$kmeans5

write.csv(ad,file = "R:/Study Material/RA - Project/predvalue.csv")