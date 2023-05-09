library(shiny)
if(!require("dplyr")){
  install.packages("dplyr")
}
library(dplyr)                                      #for cleaning the data
if(!require("tidyr"))
{install.packages("tidyr")
  library(tidyr) 
}
if(!require("data.table")){
  install.packages("data.table")
  library("data.table")
}

if(!require("tm")){
  install.packages("tm")
  library(tm)  
}
if(!require("R.utils")){
  install.packages("R.utils")
  library("R.utils")
}
if(!require("stringr")){
  install.packages("stringr")
  library("stringr")
}

if(!require("stringi")){
  install.packages("stringi")
  library("stringi")
}
if(!require("lexicon")){
  install.packages("lexicon")
  library("lexicon")
}
if(!require("furrr")){
  install.packages("furrr")
  library("furrr")
}

if(!require("tidytext")){
  install.packages("tidytext")
  library("tidytext")
}



current_path <-setwd("C:/Users/21214/Desktop/FoodMart/Exploratory data analysis/Data-science-capstone-project/Word_predictor_WebApp")
folderpath <- "/data/en_US/"
# url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# if (!(file.exists("./Coursera-SwiftKey.zip")))
# { download.file(url = url,
#                 destfile = paste(current_path, "Coursera-SwiftKey.zip", sep = "/"))}
# if (!file.exists("./Coursera-SwiftKey"))
# {unzip(zipfile  = "Coursera-SwiftKey.zip")}

## read text data from dataset
getwd()
# blogs
blogsFileName <- paste(current_path,folderpath,"en_US.blogs.txt",sep="")
con <- file(blogsFileName, open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

# news
newsFileName <- paste(current_path,folderpath,"en_US.news.txt",sep = "")
con <- file(newsFileName, open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

# twitter
twitterFileName <- paste(current_path , folderpath ,"en_US.twitter.txt",sep="")
con <- file(twitterFileName, open = "r")
twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

twitter_sample <- slice_sample(data.frame(twitter),prop = 0.05)
blog_sample <- slice_sample(data.frame(blogs),prop = 0.05)
news_sample <- slice_sample(data.frame(news),prop = 0.3)

### convert the charater string to a dataframe


twitter_df<-data.frame(twitter)
blogs_df<-data.frame(blogs)
news_df<- data.frame(news)

twitter_df<-twitter_df%>%mutate(line_len=str_length(twitter_df$twitter))
blogs_df<-blogs_df%>%mutate(line_len=str_length(blogs_df$blogs))
news_df<-news_df%>%mutate(line_len=str_length(news_df$news))

twitter_df<-twitter_df%>%mutate(word_count=str_count(twitter_df$twitter,"\\S+"))
blogs_df<-blogs_df%>%mutate(word_count=str_count(blogs_df$blogs,"\\S+"))
news_df<-news_df%>%mutate(word_count=str_count(news_df$news,"\\S+"))




# Custom content transformer to remove URLs, Twitter handles, and email patterns
myTransformer <- content_transformer(function(x) {
  x <- gsub("(f|ht)(tp)(s?)(://)[0-9a-zA-Z\\.]*/?", "", x) # remove URLs
  x <- gsub("@\\w+", "", x) # remove Twitter handles
  x <- gsub("[\\w.-]+@[\\w.-]+\\.[\\w]+", "", x) # remove email patterns
  return(x)
})
# Define a custom vector to remove profanity
profane_arr_bad <- lexicon::profanity_arr_bad


# Custom function to build the corpus
Corpus_builder <- function(text) {
  # Create a VCorpus object from the text
  corpus <- VCorpus(VectorSource(text))
  
  # Transform the corpus
  corpus <- tm_map(corpus, myTransformer) # remove URLs, Twitter handles, and email patterns
  corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lowercase
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # remove stop words
  corpus <- tm_map(corpus, removePunctuation) # remove punctuation marks
  corpus <- tm_map(corpus, removeNumbers) # remove numbers
  corpus <- tm_map(corpus, stripWhitespace) # trim whitespace
  corpus <- tm_map(corpus, removeWords, profane_arr_bad) # remove profanity
  corpus <- tm_map(corpus, PlainTextDocument) # convert to plain text documents
  
  return(corpus)
}



## sampling 1% of the data for cleaning and analysis
set.seed(1234) # Don't forget the reproducibility!



# Apply the function to the text column using furrr::future_map_chr
# plan(multiprocess)

## build corpus from the sample data
sampleData <- data.frame(sentence =  c(twitter_sample$twitter, blog_sample$blogs, news_sample$news))

# twitter_sample_coupus<-Corpus_builder(twitter_sample$twitter)
# blog_sample_corpus <- Corpus_builder(blog_sample$blogs)
# news_sample_corpus <- Corpus_builder(news_sample$news)

# build the corpus and write to disk (RDS)
corpus <- Corpus_builder(sampleData)
saveRDS(corpus, file = "./data/en_US.corpus.rds")

# convert corpus to a dataframe and write lines/words to disk (text)
corpusText <- data.frame(text = unlist(sapply(corpus, '[', "content")), stringsAsFactors = FALSE)
con <- file("./data/en_US.corpus.txt", open = "w")
writeLines(corpusText$text, con)
close(con)

split_ngram <- function(df) {
  new_df <- df %>%
    mutate(prediction = sapply(strsplit(ngram, " "), function(x) tail(x, 1)), 
           ngrams = sapply(strsplit(ngram, " "), function(x) paste(head(x, -1), collapse = " "))) %>%
    select(ngrams, prediction, n) %>%
    arrange(desc(n))
  
  return(new_df)
}

#generating unigrams
ngrams_1_sampleData <- sampleData %>%
  unnest_tokens(ngram, sentence, token = "ngrams", n = 1)

# Count the frequency of each n-gram
freq_1_sampleData <- ngrams_1_sampleData %>%
  count(ngram, sort = TRUE)
saveRDS(freq_1_sampleData, file = "./data/unigram__freq_.rds") 

# Tokenize sampleData sample the text into 2-grams
ngrams_2_sampleData <- sampleData%>%
  unnest_tokens(ngram, sentence, token = "ngrams", n = 2)

# Count the frequency of each n-gram
freq_2_sampleData <- ngrams_2_sampleData %>%
  count(ngram, sort = TRUE)%>%split_ngram()
saveRDS(freq_2_sampleData, file = "./data/bigram_freq__.rds")


# Tokenize sampleData sample the text into 3-grams
ngrams_3_sampleData <- sampleData%>%
  unnest_tokens(ngram, sentence, token = "ngrams", n = 3)

# Count the frequency of each n-gram
freq_3_sampleData <- ngrams_3_sampleData %>%
  count(ngram, sort = TRUE)
freq_3_sampleData <-freq_3_sampleData[complete.cases(freq_3_sampleData), ]%>% split_ngram() #subseting the dataframe to eliminate NA observations
saveRDS(freq_3_sampleData, file = "./data/trigram_freq__.rds")



# Tokenize sampleData sample the text into 4-grams
ngrams_4_sampleData <- sampleData%>%
  unnest_tokens(ngram, sentence, token = "ngrams", n = 4)


# Count the frequency of each n-gram
freq_4_sampleData <- ngrams_4_sampleData %>%
  count(ngram, sort = TRUE)
freq_4_sampleData <-freq_4_sampleData[complete.cases(freq_4_sampleData), ]%>% split_ngram() #subseting the dataframe to eliminate NA observations
saveRDS(freq_4_sampleData, file = "./data/quadgram_freq__.rds")

rm(list = ls()) #remove all variable from the RAM to free up space

