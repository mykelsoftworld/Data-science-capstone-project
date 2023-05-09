#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tm)
library(lexicon)
library(stringr)



#setwd("C:/Users/21214/Desktop/FoodMart/Exploratory data analysis/Data-science-capstone-project/Word_predictor_WebApp")
frequnigram <-readRDS("./data/unigram_freq.rds")
freq2ngram <- readRDS("./data/bigram_freq.rds")
freq3ngram <- readRDS("./data/trigram_freq.rds")
freq4ngram <- readRDS("./data/quadgram_freq.rds")





clean_input <- function(text) {
  # Create a VCorpus object from the text
  corpus <- VCorpus(VectorSource(text))
  
  # Custom content transformer to remove URLs, Twitter handles, and email patterns
  myTransformer <- content_transformer(function(x) {
    x <- gsub("(f|ht)(tp)(s?)(://)[0-9a-zA-Z\\.]*/?", "", x) # remove URLs
    x <- gsub("@\\w+", "", x) # remove Twitter handles
    x <- gsub("[\\w.-]+@[\\w.-]+\\.[\\w]+", "", x) # remove email patterns
    return(x)
  })
  
  profane_arr_bad <- lexicon::profanity_arr_bad
  
  # Transform the corpus
  corpus <- tm_map(corpus, myTransformer) # remove URLs, Twitter handles, and email patterns
  corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lowercase
  corpus <- tm_map(corpus, removePunctuation) # remove punctuation marks
  corpus <- tm_map(corpus, removeNumbers) # remove numbers
  corpus <- tm_map(corpus, stripWhitespace) # trim whitespace
  corpus <- tm_map(corpus, removeWords, profane_arr_bad) # remove profanity
  corpus <- tm_map(corpus, PlainTextDocument) # convert to plain text documents
  corpus <- data.frame(text = unlist(sapply(corpus, '[', "content")), stringsAsFactors = FALSE)$text
  return(unlist(str_split(corpus, pattern = "\\s+")))
}

predictionModel <- function(userInput, ngrams) {
  
  # quadgram (and higher)
  if (ngrams > 3) {
     userInput3 <- paste(userInput[length(userInput) - 2],
                         userInput[length(userInput) - 1],
                         userInput[length(userInput)])
     dataTokens <- freq4ngram %>% filter(ngrams == userInput3)
     if (nrow(dataTokens) > 2) {
       # for (i in 1:5){  #input$numpredictions replace with ds 5 used only for test purpose
       #   words <- strsplit(dataTokens$ngram, "\\s+")
       #   last_word <- sapply(words, tail, 1)
       #   
       prediction <-dataTokens$prediction
       return(prediction)
       }
       
     
     # backoff to trigram
     return(predictionModel(userInput, ngrams - 1))
   }
   
  # trigram
  if (ngrams == 3) {
    userInput2 <- paste(userInput[length(userInput)-1], userInput[length(userInput)])
    #dataTokens <- freq3ngram %>% filter(grepl(paste0("^",userInput2),ngram))
    dataTokens <- freq3ngram %>% filter(ngrams == userInput2)
    if (nrow(dataTokens) > 2) {

      prediction <-dataTokens$prediction
      
      return(prediction)
    }
    # backoff to bigram
    return(predictionModel(userInput, ngrams - 1))
  }
  
  # bigram (and lower)
  if (ngrams < 3) {
    userInput1 <- paste(userInput[length(userInput)])
    dataTokens <- freq2ngram %>% filter(ngrams == userInput1 ) 
    #dataTokens <- freq2ngram %>% filter(grepl(paste0("^",userInput1),ngram))
    if (nrow(dataTokens) > 1) {

      prediction <-dataTokens$prediction
      
      return(prediction)
    }
    # backoff (1-gram not implemented for enhanced performance)
    # return(match_predict(userInput, ngrams - 1))
  }
  
  # unigram: not implemented to enhance performance
  return(NA)
}

predictNextWord <- function(input) {
  
  input <- clean_input(input)
  
  if (length(input) == 0) {
    output <- frequnigram[1:3,1]
      } else if (length(input) ==1) {
    output <- predictionModel(input, ngrams = 2)
    } else if (length(input) ==2) {
    output <- predictionModel(input, ngrams = 3)
      }
   else if (length(input) > 2) {
     output <- predictionModel(input, ngrams = 4)
      }
  return(output)
}

shinyServer(function(input, output, session) {

  output$userSentence <- renderText({input$userInput});
  
  # reactive controls
  observe({
    numPredictions <- input$numPredictions
    pred <- reactive((predictNextWord(input$userInput)))
    prediction <- pred()
    prediction <- ifelse(is.na(prediction), "", prediction)  # replace NA with ""
    if (numPredictions == 1) {
      output$prediction1 <- renderText(prediction[1])
      output$prediction2 <- NULL
      output$prediction3 <- NULL
    } else if (numPredictions == 2) {
      output$prediction1 <- renderText(prediction[1])
      output$prediction2 <-  renderText(prediction[2])
      output$prediction3 <- NULL
    } else if (numPredictions == 3) {
      output$prediction1 <- renderText(prediction[1])
      output$prediction2 <- renderText(prediction[2])
      output$prediction3 <- renderText(prediction[3])
    }
  })
  
})
