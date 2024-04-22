## Shiny app

## Load packages
library(shiny)
library(tidyverse)
library(targets)
library(data.table)
library(here)


## Load models
# 2-6gram models
# ngram_model <- tar_read(ngram_model_merged, store = here("_targets"))
ngram_model <- readRDS("ngram_model.RDS")
# unigram model
# unigram_model <- tar_read(unigram_model_all_reduced, store = here("_targets"))
unigram_model <- readRDS("unigram_model.RDS")

## Minor cleaning to unigram model so that it can be rbinded to the ngram model results
unigram_model <- unigram_model[, history_text:=character()]
unigram_model <- setcolorder(unigram_model,c("history_text","text", "text_prob", "sb_score"))
unigram_model <- setNames(unigram_model, names(ngram_model))

## Force unigram scores to be lower than ngram score, whilst maintaining order
unigram_model <- unigram_model[, sb_score:=sb_score/10000]


## Cleaning text function
clean_text_function <- function(text){
    
    ## Define regex
    replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&lt;|&gt;|(RT|via)((?:\\b\\W*@\\w+)+)|[[:punct:]]|[[:digit:]]|[[:symbol:]]|@\\w+|http\\w+|[ \t]{2,}|^\\s+|\\s+$"
    
    ## to lower
    text <- str_to_lower(text)
    
    ## replace ampersand with and
    text <- str_replace_all(text, "&amp;", "and")
    
    ## Main replace
    text <- str_replace_all(text, replace_reg, "")
    
    
    
}




## Include prediction function
## To be referenced by server part below.
predict_words_function <- function(input_text){
    
    ## clean input text to remove punctuation.
    input_text <- clean_text_function(input_text)
    
    
    ## Split based on space
    input_text_split <- as.character(str_split(tolower(input_text), pattern = " ", simplify = TRUE))
    
    ## Reduce size to only look at last x words
    input_text_split <- tail(input_text_split, 5)
    
    ## To be used in control flow
    input_text_length <- length(input_text_split)
    

    ## *******************************************************
    ## New loop to look for n-gram in each n-gram model
    
    ## Set up list for each set of predictions
    preds <- list()
    
    ## Trying with for loop
    ## For loop
    for (i in 1:5){
        
        ## get last n words from string and combine into single character, separated by spaces.
        input_text_ngram <- paste(tail(input_text_split,i), collapse = ' ')
        
        ## Filter training data on input_text_ngram
        preds[[i]] <- ngram_model[history_text == input_text_ngram,]
        
    }
    
    ## Combine lists to one DT
    preds <- rbindlist(preds)
    
    ## Arrange preds by score, descending
    preds <- preds[order(-sb_score)]
    
    ## Combine preds with unigram model
    preds <- rbind(preds, unigram_model)
    
    ## Arrange preds by score, descending
    preds <- preds[order(-sb_score)]
    
    ## group by predicted word and combine scores
    preds <- preds[, .(sb_score_total=exp(sum(log(sb_score)))), by=next_word]
    
    ## Arrange preds by score, descending
    preds <- preds[order(-sb_score_total)]
    
    ## Return
    preds
}




# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Next word prediction - Data Science Capstone"),
    
    ## sidebar layout
    sidebarLayout(
    
        # Basic instructions
        sidebarPanel(
            helpText(
                "This application uses an ngram model to predict the next word in a sentance."
            ),
            helpText(
                "Please enter a sentance in the box to the right and predicted words will appear."
            ),
            helpText(
                "You can add to the initial sentance by clicking on the predicted word buttons, or just continuing to type."
            ),
            helpText(
                "If the sentance is unfamiliar or blank, the application will suggest common high frequency words such as 'the'."
            ),
            helpText(
                "A report detailing exploratory analysis of the data used for this application cab be found in
                the 'Exploratory analysis report' tab."
            ),
            helpText(
                "For more information, please see the 'About' tab."
            )
        ),
    
            
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                ## First tab
                tabPanel(
                    "Predictor",
                    ## Line breaks
                    br(),
                    br(),
                    
                    ## Input text
                    textAreaInput(
                        inputId = "textinput",
                        label = "Please write your sentance here",
                        value = "",
                        rows = 5
                    ) %>% shiny::tagAppendAttributes(style = 'width: 100%;')
                    ,
                    ## Outputted predicted words
                    splitLayout(
                        # cellWidths = 100,
                        cellArgs = list(style = "padding: 6px"),
                        actionButton("suggestion1", label = ""),
                        actionButton("suggestion2", label = ""),
                        actionButton("suggestion3", label = ""),
                        actionButton("suggestion4",label = ""),
                        actionButton("suggestion5",label = "")
                    )
                ),
                
                ## EDA report
                tabPanel(
                    "Exploratory analysis report",
                    includeHTML("milestone-report.html")  
                ),
                
                
                ## About page
                tabPanel(
                    "About",
                    helpText(
                        "This R Shiny application was developed as part of the capstone project
                        for the John Hopkins University Data Science Specialisation Coursera course.
                        "
                    ),
                    helpText(
                        "The aim of the capstone project was to build a predictive text model that could
                        predict the next word in a sentence, and then to build a product to showcase this model.
                        This R Shiny application represents that product."
                    ),
                    helpText(
                        "The model behind the application is a combination of six ngram models, which show the probability of words
                        given the previous n words in a sentance. Probabilities are adjusted so that when n is larger, the probability 
                        is higher, as per the Katz back-off approach."
                    ),
                    HTML('<a href="https://en.wikipedia.org/wiki/Katz%27s_back-off_model"> Katz back-off model</a>'),
                    helpText(
                        "The full model is condensed to show the highest probability words for each sentance and is stored as 
                        a data.table object. It is therefore very small and fast, presenting predictions instantly."
                    ),
                    helpText(
                        "For full details on the model and this app, please see the following repository:"
                    ),
                    HTML('<a href="https://github.com/alexjhall/DataScienceCapstone"> https://github.com/alexjhall/DataScienceCapstone </a>')
                    
                )
            )
        )
    )
)




# Define server logic 
server <- function(input, output, session) {
    
    # initial button labels
    prediction <- reactiveVal(c("the","quick","brown","fox","jumps")) 
    
    # to watch input for change
    observeEvent(input$textinput,{
        
        # predict words
        preds <- predict_words_function(input$textinput)
        
        # get top5 words
        top5words <- head(preds$next_word, 5)
        
        ## Update action buttons with output from prediction
        updateActionButton(session,inputId = "suggestion1",label =top5words[1])
        updateActionButton(session,inputId = "suggestion2",label =top5words[2])
        updateActionButton(session,inputId = "suggestion3",label =top5words[3])
        updateActionButton(session,inputId = "suggestion4",label =top5words[4])
        updateActionButton(session,inputId = "suggestion5",label =top5words[5])
        
        ## Update prediction reactive vals
        prediction(top5words)
        
        
    })
    
    ## Allow action buttons to modify input
    observeEvent(input$suggestion1,{
        updateTextAreaInput(session,inputId = "textinput",value = paste(input$textinput,prediction()[1]))
    })
    
    observeEvent(input$suggestion2,{
        updateTextAreaInput(session,inputId = "textinput",value = paste(input$textinput,prediction()[2]))
    })
    
    observeEvent(input$suggestion3,{
        updateTextAreaInput(session,inputId = "textinput",value = paste(input$textinput,prediction()[3]))
    })
    
    observeEvent(input$suggestion4,{
        updateTextAreaInput(session,inputId = "textinput",value = paste(input$textinput,prediction()[4]))
    })
    
    observeEvent(input$suggestion5,{
        updateTextAreaInput(session,inputId = "textinput",value = paste(input$textinput,prediction()[5]))
    })
}

## Run app
shinyApp(ui, server)