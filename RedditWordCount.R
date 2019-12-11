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


#connect to your directory and file #1 askreddit_ELI5_funny

data_dir <- "C:\\Users\\Alexi\\Documents\\UofM\\Fall2019\\ADA\\applied-data-analytics\\web-scraping\\Text_Based_Project_Final\\RawRedditData\\"

data_file <- "posts_askreddit_ELI5_funny.csv" #broke the big file into 3, will do each in silo and then combine results
redditdata <- read_csv(paste0(data_dir,data_file))

#list of words I don't care about
stop_words <- c('a', 'and', 'for', 'the', 'with', 'of', 'or', 'but',
                'so', 'that', 'is', 'was', 'to', 'it', 'in', 'you', 'what',
                'why', 'do', 'how', 'are', 'as', 'from', 'have', 'will', 'by', 'from',
                'be', 'I', 'on', 'when', 'this', 'does', 'if', 'not', 'be',
                'whats', 'like', 'at', 'we', 'an', 'us', 'has', 'their', 'your',
                'my', 'i', 'its', 'youve', 'than', 'more', 'had', 'been', 'into',
                'eli5how', 'get', 'they', 'can', 'who', 'would', 'all', 'did',
                'them', 'being', 'were', 'most', 'out', 'up', 'which', 'me', 'dont', 'while',
                'cant', 'go', 'got', 'such', 'really', 'then', 'than', 'some', 'about',
                'could', 'should', 'used', 'uses', 'use', 'using', 'eli5', 'eli5why', 'isnt',
                'went', 'didnt', 'very', 'ie', 'ive', 'put', 'yet', 'let', 'also', 'arent',
                'our', 'just', 'now', 'ever', 'there', 'after', 'where') 

#create data frame
wordcount <- data_frame(text = redditdata) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest() %>% 
  count(tokens) %>% 
  filter(!tokens %in% stop_words) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n))

#File #2 science_politics_news

data_file <- "posts_science_politics_news.csv" 
redditdata2 <- read_csv(paste0(data_dir,data_file))

wordcount2 <- data_frame(text = redditdata2) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest() %>% 
  count(tokens) %>% 
  filter(!tokens %in% stop_words) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n))

#File #3 worldnews_TIL_Showerthoughts

data_file <- "posts_worldnews_TIL_Showerthoughts.csv" 
redditdata3 <- read_csv(paste0(data_dir,data_file))

wordcount3 <- data_frame(text = redditdata3) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest() %>% 
  count(tokens) %>% 
  filter(!tokens %in% stop_words) %>% 
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n))


#### Recombine all three files into one dataframe with the following

df1 <- data.frame(word = wordcount3$tokens,
                  N = wordcount3$n)
df2 <- data.frame(word = wordcount2$tokens,
                  N= wordcount2$n)
df3 <- data.frame(word = wordcount$tokens,
                  N= wordcount$n)

unique.words <- unique(df1$word,df2$word,df3$word)
new.df <- data.frame(word=unique.words,
                     N=0,
                     stringsAsFactors = F)
for(i in 1:nrow(new.df)){
  this.word <- as.character(new.df$word[i])
  if(this.word %in% df1$word){
    new.df$N[i] <- df1$N[df1$word==this.word]
  }
  if(this.word %in% df2$word){
    new.df$N[i] <- new.df$N[i] + df2$N[df2$word==this.word]
  }
  if(this.word %in% df3$word){
    new.df$N[i] <- new.df$N[i] + df3$N[df3$word==this.word]
  } 
}
new.df$freq <- new.df$N/sum(new.df$N)
tidy_words<- new.df

#Sentiment Analysis
nrc_sentiments <- get_sentiments("nrc") #my lexicon of choice

sentimentAll <- tidy_words %>%
  inner_join(nrc_sentiments) %>%
  mutate(sentiment) %>% 
  group_by(sentiment)


Grouped_Sentiment <- sentimentAll %>% 
  group_by(sentiment)


#Now I will attempt to combine this into some nice visuals and descriptives

#convert from list to a dataframe
top_100 <- TotalWordsdf[1:100,]
top_sentiment <- sentimentAll[1:20,]

ggplot(top_sentiment, aes(x=sentiment, y=reorder(word, -freq), fill=sentiment)) + 
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Language Sentiment Analysis on Reddit")+
  xlab("")+
  guides(fill=FALSE)













