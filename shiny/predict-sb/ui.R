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
                                p("Text area: enter the text here, and predicted
    words will appear on the chart below. The chart shows multiple candidates
    ordered by score, providing a visual clue on how probable each candidate is.
    When “Predict partially entered words” is activated, ",
    tags$strong("you have to type the space character to predict the next word"),
    "otherwise an ending of the current word is predicted.")
                )),
                fluidRow(column(12,
                    p("Number of predictions: choose from 1 to 10 candidates to predict.")
                )),
                fluidRow(column(12,
                    p("Predict partially entered words: select the checkbox to
    activate the extension which predicts partially entered words.")            
                )),
                fluidRow(column(12,
                    p("When predicting partially entered words, if you start
    entering a word unknown to the application, it assumes the word to be
    misspelled and suggests a correction. Try to enter 'mashine' instead of
    'machine'.")
                )),
                fluidRow(column(12,
                    p("Random sample: populate the text area with a random
    sample from a selection of over 1000 prepared sample texts.")
                )),
                fluidRow(column(12,
                    p("Choose the tab 'About' in the headline to learn more
    about this application.")
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
