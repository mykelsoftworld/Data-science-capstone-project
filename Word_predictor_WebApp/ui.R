#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tm)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Word predictor App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        textInput("userInput",
                  "Enter a word or phrase:",
                  value = "",
                  placeholder = "Enter text here"),
        br(),
        sliderInput("numPredictions", "Number of Predictions:",
                    value = 1.0, min = 1.0, max = 3.0, step = 1.0)
      ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type="tabs",
            tabPanel("Home", br(),
                     h4("Input text"),
                     textOutput("userSentence"),
                     br(),
                     h4("Predicted words"),
                     br(),
                     br(),
                     verbatimTextOutput("prediction1"),
                     verbatimTextOutput("prediction2"),
                     verbatimTextOutput("prediction3")
                    
        ),

      tabPanel("About",
             h3("About Word Predict App"),
             br(),
             div("Next Word Predictor is a Shiny app that uses a text
                            prediction algorithm to predict the next word(s)
                            based on text entered by a user.",
                 br(),
                 br(),
                 
                 p("the prediction is based on the the principle of predicting the most 
                   frequently used words combination  stored in a preprocessed dataset
                   loaded in the program"),
                 br(),
                 br(),
                 p("The predicted next word will be shown when the app
                            detects that you have finished typing one or more
                            words. When entering text, please allow a few
                            seconds for the output to appear."),
                 br(),
                 br(),
                 "Use the slider tool to select up to three next
                            word predictions. The top prediction will be
                            shown first followed by the second and third likely
                            next words.",
                 br(),
                 br(),
                 "kindly note !!  ==> it is recommended to wait for about 10 seconds so the application 
                 can load relavant library,dataset and model required to run the App "
                
            

             )
    
    )
)
))))

