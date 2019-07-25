#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
        "Next Word Prediction: Stupid Backoff",
        tabPanel(
            "Prediction",
            fluidPage(
                fluidRow(column(12,
                    p("Start typing text in the input area below. The prediction of the next word will appear and update as you type.")
                )),
                fluidRow(column(12,
                    p("If the checkbox 'Predict partially entered words' is selected, the application will attempt to predict continuation of a word you are currently typing. In this mode, ", tags$strong("the next word is predicted only after you type the space character"), " to indicate that you have finished entering the previous word.")
                )),
                fluidRow(column(12,
                    p("When predicting partially entered words, if you start entering a word unknown to the application, it assumes the word to be misspelled and suggests a correction. Try to enter 'mashine' instead of 'machine'.")
                )),
                fluidRow(column(12,
                    p("Use the slider below the input area to choose the number of predicted words. Click the button 'Random Sample' to fill the input area with one of over 1000 prepared sample texts.")
                )),
                fluidRow(column(12,
                    p("Choose the tab 'About' in the headline to learn more about this application.")
                )),
                fluidRow(column(12,
                    textAreaInput("textPrefix", label = NULL, width = "800px")
                )),
                fluidRow(column(4,
                    sliderInput("n.predictions", "Number of predictions:",
                                min = 1, max = 10, value = 5)
                ),
                column(2, offset = 2,
                    actionButton("randomSample", label = "Random Sample")
                )),
                fluidRow(column(12,
                    checkboxInput("predictPartial",
                                  label = "Predict partially entered words",
                                  value = TRUE)
                )),
                fluidRow(column(12, plotlyOutput("predictionPlot")))
            )
        ),
        tabPanel("About",
                 column(12, includeMarkdown("about.md"))
    )
))
