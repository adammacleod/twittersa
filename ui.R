library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Twitter Sentiment Analyser (TSA)"),
  
  sidebarLayout(
    sidebarPanel(textInput("hashtag", "Hashtag", "#icbm2016"), width=2),

    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("cloudPlot")),
        tabPanel("Emotions", plotOutput("emoPlot")),
        tabPanel("Polarity", plotOutput("polarityPlot")),
        tabPanel("Tweets", DT::dataTableOutput("table"))
      )
    )
  )
))
