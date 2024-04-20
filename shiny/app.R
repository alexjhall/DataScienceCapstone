## Shiny app

## Load packages
library(shiny)
library(tidyverse)
library(targets)
library(data.table)
library(here)


## Load models
# 2-6gram models
ngram_model <- tar_read(ngram_model_merged, store = here("_targets"))
# unigram model
unigram_model <- tar_read(unigram_model_all_reduced, store = here("_targets"))



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
    
    ## Arrange preds by score?
    preds
    
    
    ## Return
    
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
                "This is where there will be basic instructions on entering text"
            )
            
        ),
            
        # Show a plot of the generated distribution
        mainPanel(
            ## Input text
            textareaInput(
                inputId = "textinput",
                label = "Please write your sentance here",
                value = ""
            ),
            ## Outputted predicted words
            splitLayout(
                cellWidths = 100,
                cellArgs = list(style = "padding: 6px"),
                actionButton("suggestion1", label = ""),
                actionButton("suggestion2", label = ""),
                actionButton("suggestion3", label = ""),
                actionButton("suggestion4",label = ""),
                actionButton("suggestion5",label = "")
            ),
            
            ## testing output
            # textOutput("test1")
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