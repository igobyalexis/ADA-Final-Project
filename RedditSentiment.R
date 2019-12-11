library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(readr)
library(tm)
library(lubridate)
library(Hmisc)
library(ggplot2)
library(SentimentAnalysis)
library(tidytext)
library(tm)
library(spacyr)
library(quanteda)
library(newsmap)
library(ggpubr)


#Research Questions to consider:
#1) Do subreddits differ on overall sentiment
#2) Does a particular sentiment drive overall "score"
#3) Anything else?

#connect to your directory and file #1 'askreddit'

data_dir <- "C:\\Users\\Alexi\\Documents\\UofM\\Fall2019\\ADA\\applied-data-analytics\\web-scraping\\Text_Based_Project_Final\\RawRedditData\\"

#my dataset
data_file <- "posts_reddit_topsubreddits.csv" 
redditdata <- read_csv(paste0(data_dir,data_file))

#First add columns for title scores in my 'reddidata' data frame
redditdata <- redditdata %>% 
  mutate(negative = 0.0)
redditdata <- redditdata %>% 
  mutate(positive = 0.0)

#my list of words I don't care about
stop_words <- c('trump', 'i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you', "you're", "you've", "you'll", "you'd", 'your', 'yours', 'yourself', 'yourselves', 'he', 'him', 'his', 'himself', 'she', "she's", 'her', 'hers', 'herself', 'it', "it's", 'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which', 'who', 'whom', 'this', 'that', "that'll", 'these', 'those', 'am', 'is', 'are', 'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about', 'against', 'between', 'into', 'through', 'during', 'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under', 'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more', 'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will', 'just', 'don', "don't", 'should', "should've", 'now', 'd', 'll', 'm', 'o', 're', 've', 'y', 'ain', 'aren', "aren't", 'couldn', "couldn't", 'didn', "didn't", 'doesn', "doesn't", 'hadn', "hadn't", 'hasn', "hasn't", 'haven', "haven't", 'isn', "isn't", 'ma', 'mightn', "mightn't", 'mustn', "mustn't", 'needn', "needn't", 'shan', "shan't", 'shouldn', "shouldn't", 'wasn', "wasn't", 'weren', "weren't", 'won', "won't", 'wouldn', "wouldn't") 

words <- data.frame(text = redditdata$title) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest() %>% 
  count(tokens) %>% 
  filter(!tokens %in% stop_words) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n))

words <- data.frame(word = words$tokens,
                  N = words$n,
                  freq = words$freq)


##### Sentiment Analysis on words! ##### 
nrc_sentiments <- get_sentiments("nrc") #my lexicon of choice

sentimentAll <- words %>%
  left_join(nrc_sentiments) %>%
  mutate(sentiment)

sentimentAll <- na.omit(sentimentAll, cols="sentiments") #remove words that don't have assigned values in the nrc lexicon

#if I want to-->  
write.csv(sentimentAll, "C:\\Users\\Alexi\\Documents\\UofM\\Fall2019\\ADA\\applied-data-analytics\\web-scraping\\Text_Based_Project_Final\\RawRedditData\\sentimentAll.csv", row.names = FALSE)

#Score each title, negative or positive scores

# tokenize and pull negative and positive words from SentimentAll
neg.score <- 0
neg.words <- sentimentAll %>% 
  filter(sentiment == "negative") %>% 
  pull(word) %>% 
  unique

pos.score <- 0
pos.words <- sentimentAll %>% 
  filter(sentiment == "positive") %>% 
  pull(word) %>% 
  unique

#loop to clean titles, and score, then add score to positive and negative columns.
for (i in 1:nrow(redditdata)){
    title.words <- redditdata$title[i]
    title.words <- unlist(strsplit(title.words,split=" "))
    title.words <- str_remove_all(title.words, '[[:punct:]]')
    title.words <- tolower(title.words)
  
  for (ij in title.words) {
    if (ij %in% neg.words) {
      neg.score <- neg.score + 1 }
    if (ij %in% pos.words) {
      pos.score <- pos.score + 1 }
  }
    
  redditdata$negative[i] <- neg.score
  neg.score <- 0
  redditdata$positive[i] <- pos.score
  pos.score <- 0
}



#Some General Linear Model Regression!
#redditdata <- redditdata %>%  #if i want to scale the data
#mutate(score_scale = (score - mean(score))/sd(score))

lm1 <- lm(score ~ posmin * subreddit - 1, data = redditdata)
summarylm1 <- summary(lm1)
describe(redditdata$score)


#Once I've completed my analysis, connect to instance of Tableau with following
library(Rserve)
Rserve(port = 6311)


##### Testing Ground - Hard Hats Required!!! #####

# testing ground - 1 title at a time
i <- 9
the.title <- redditdata$title[i]
title.words <- unlist(strsplit(the.title,split=" "))
title.words <- str_remove_all(title.words, '[[:punct:]]')
title.words <- tolower(title.words)


#Score each word in the title as negative or positive
for (word in title.words) {
  if (word %in% neg.words) {
    neg.score <- neg.score + 1
    redditdata$negative[i] <- neg.score #add title score to column 'negative' in redditdata datafram
    if (word %in% pos.words) {
      pos.score <- pos.score + 1
      redditdata$positive[i] <- pos.score #add title score to column 'positive' in redditdata dataframe
      
    }
  }
}
## Loop version to clean titles, remove punctuation, lowercase, etc...
the.title <- redditdata$title[i]
for (i in 1:nrow(redditdata)){
  the.title <- redditdata$title[i]
  title.words <- unlist(strsplit(the.title,split=" "))
  title.words <- str_remove_all(title.words, '[[:punct:]]')
  title.words <- tolower(title.words)
}
#for negative words
for (i in 1:nrow(redditdata)){
  if (i %in% neg.words) {
    neg.score <- neg.score + 1
    redditdata$negative[i] <- neg.score
  }
}


#Other lexicons
#AFINN
afinn_sentiments <- get_sentiments("afinn")

afinn_sentiments$word <- as.String(afinn_sentiments$word)

sentimentAfinn <- words %>%
  left_join(afinn_sentiments) %>% 
  mutate(sentiment)


SentimentAfinn <- merge(x = words, y = afinn_sentiments, by = "word", all.y = TRUE, na.omit(words, cols="sentiments"))

sentimentAll <- na.omit(sentimentAll, cols="sentiments") #remove words that don't have assigned values in the nrc lexicon

#loughran
loughran_sentiments <- get_sentiments("loughran")

sentimentAll <- words %>%
  left_join(loughran_sentiments) %>%
  mutate(sentiment) 

sentimentAll <- na.omit(sentimentAll, cols="sentiments") #remove words that don't have assigned values in the nrc lexicon


redditdata <- redditdata %>% 
  mutate(posminusneg = (positive-negative)) %>% 
  mutate(positiveoverall = posminusneg >=0)


results <- group_by(redditdata, posminusneg) %>%
  summarise(
    count = n(),
    mean = mean(score, na.rm = TRUE),
    sd = sd(score, na.rm = TRUE)
  )



#redditdata <- redditdata %>%  #if i want to scale the data
  #mutate(score_scale = (score - mean(score))/sd(score))

lm1 <- lm(score ~ posmin * subreddit - 1, data = redditdata)
summarylm1 <- summary(lm1)
describe(redditdata$score)

ggplot(results2, aes(x=posminusneg, y = mean), fill=posminusneg) + 
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Language Sentiment Analysis on Reddit")+
  xlab("")+
  guides(fill=FALSE)
