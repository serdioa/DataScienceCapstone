#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("predict.sb.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    count <- 0
    
    observeEvent(input$randomSample, {
        textPrefix <- sample(test.sample, 1)
        updateTextInput(session, "textPrefix", value = textPrefix)
    })
    
    getReactiveTextPrediction <- reactive({
        textPrefix <- input$textPrefix
        predict.partial <- input$predictPartial
        n <- input$n.predictions

        if (nchar(trimws(textPrefix)) == 0) {
            NULL
        } else {
            sb.predict.text(textPrefix, predict.partial = predict.partial, n)
        }
    })

    output$predictionPlot <- renderPlotly({
        textPrediction <- getReactiveTextPrediction()
        if (is.null(textPrediction)) {
            return (NULL)
        }

        suffix <- paste(textPrediction$Suffix, "&nbsp;&nbsp;")
        score <- textPrediction$Score
        
        score.offset <- max(score) * 0.05

        p <- plot_ly(x = ~score, y = ~reorder(suffix, score), name = "Predicted Next Word",
                     type = "bar", orientation = "h",
                     marker = list(color = 'rgba(16, 52, 166, 0.6)',
                                   line = list(color = 'rgba(16, 52, 166, 1.0)', width = 1))) %>%
            layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, title = NA),
                   xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE, title = "Score")) %>%
            add_annotations(xref = 'x1', yref = 'suffix',
                            x = score + score.offset,  y = suffix,
                            text = round(score, 4),
                            showarrow = FALSE)
    })
    
    test.sample <- list(
        "The guy in front of me just bought a pound of bacon, a bouquet, and a case of ",
        "You're the reason why I smile everyday. Can you follow me please? It would mean the ",
        "Hey sunshine, can you follow me and make me the ",
        "Very early observations on the Bills game: Offense still struggling but the ",
        "Go on a romantic date at the ",
        "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my ",
        "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some ",
        "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little ",
        "Be grateful for the good times and keep the faith during the ",
        "If this isn't the cutest thing you've ever seen, then you must be ",
        "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd ",
        "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his ",
        "I'd give anything to see arctic monkeys this ",
        "Talking to your mom has the same effect as a hug and helps reduce your ",
        "When you were in Holland you were like 1 inch away from me but you hadn't time to take a ",
        "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the ",
        "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each ",
        "Every inch of you is perfect from the bottom to the ",
        "I’m thankful my childhood was filled with imagination and bruises from playing ",
        "I like how the same people are in almost all of Adam Sandler's "
    )
})
