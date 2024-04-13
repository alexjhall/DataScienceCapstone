## Accuracy testing

## Quiz 3.

## load model
ngram_model <- tar_read(ngram_model_merged)
# check size
print(object.size(ngram_model), units="Mb")

## load unigram model
unigram_model <- tar_read(unigram_model_all_reduced)
# check size
print(object.size(unigram_model), units="Mb")


## text to test
input_text <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd" # guessed die
input_text <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his" #guessed marital
input_text <- "I'd give anything to see arctic monkeys this" #guessed weekend
input_text <- "Talking to your mom has the same effect as a hug and helps reduce your" # guessed stress
input_text <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a" # algo look
    # wrong
    # picture?

input_text <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the" #algo case
    # wrong
    # matter?

input_text <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each" #algo hand, but low down
input_text <- "Every inch of you is perfect from the bottom to the" # algo: top
input_text <- "I’m thankful my childhood was filled with imagination and bruises from playing" #guessed outside
input_text <- "I like how the same people are in almost all of Adam Sandler's" # guessed movies

## Got 80%!

# input_text <- "What on earth are you"


## clean input text to remove punctuation.
input_text <- clean_text_function(input_text)


## Split based on space
input_text_split <- as.character(str_split(tolower(input_text), pattern = " ", simplify = TRUE))

## Reduce size to only look at last x words
input_text_split <- tail(input_text_split, 5)

## To be used in control flow
input_text_length <- length(input_text_split)

# input_text_ngram <- paste(tail(input_text_split,5), collapse = ' ')


## *******************************************************
## New loop to look for n-gram in each n-gram model

## Set up list for each set of predictions
preds <- list()

## Trying with for loop
## For loop
for (i in 1:5){
    
    ## get last n words from string and combine into single character, separated by spaces.
    input_text_ngram <- paste(tail(input_text_split,i), collapse = ' ')
    
    # ## if statement to check if input text exists in ngram model
    # if(input_text_ngram %in% ngram_model$history_text){
    #     
    #     ## Filter training data on input_text_ngram
    #     preds <- ngram_model[history_text == input_text_ngram, next_word]
    # }
    
    
    ## Filter training data on input_text_ngram
    preds[[i]] <- ngram_model[history_text == input_text_ngram,]
    
    
}

## Combine lists to one DT
preds <- rbindlist(preds)
preds












## *******************************************************
## Backoff version
## *******************************************************


## model list
# model_list <- tar_read(ngram_model_backoff_list)

## Load all models
# unigram <- tar_read(unigram_model_all)
# bigram <- tar_read(bigram_model_all)
# trigram <- tar_read(trigram_model_all)
# quadgram <- tar_read(quadgram_model_all)
# fivegram <- tar_read(fivegram_model_all)
# sixgram <- tar_read(sixgram_model_all)

ngram_model <- tar_read(ngram_model_merged)







# input_text <- "so then I thought"

## input text questions
input_text <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of" # beer?
input_text <- "You're the reason why I smile everyday. Can you follow me please? It would mean the" #world
input_text <- "Hey sunshine, can you follow me and make me the" #happiest
    input_text <- "Very early observations on the Bills game: Offense still struggling but the" # doesn't know... wrong words
    # guessed defence
    input_text <- "Go on a romantic date at the" # doesn't know... wrong words
    # guessed beach
input_text <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my" #way
input_text <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some" #time
    input_text <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little" #wrong words
    # Guessed fingers
    input_text <- "Be grateful for the good times and keep the faith during the" #lots of words, down to two word history
    ## Guessed bad
    input_text <- "If this isn't the cutest thing you've ever seen, then you must be"
    ## Guessed callous
    # only this one wrong, try again later.
    # 13/04/24 guessed insane. correct.


# ngram_model[history_text == input_text_ngram, next_word.x]
# ngram_model[history_text == input_text_ngram]



## Timer - end time
# start.time <- Sys.time()


## Split based on space
input_text_split <- as.character(str_split(tolower(input_text), pattern = " ", simplify = TRUE))

## Reduce size to only look at last x words
input_text_split <- tail(input_text_split, 5)

## To be used in control flow
input_text_length <- length(input_text_split)

input_text_ngram <- paste(tail(input_text_split,3), collapse = ' ')


## *******************************************************
## New loop to look for n-gram in each n-gram model

## Set up list for each set of predictions
preds <- list()

## Trying with for loop
## For loop
for (i in 1:3){
    
    ## get last n words from string and combine into single character, separated by spaces.
    input_text_ngram <- paste(tail(input_text_split,i), collapse = ' ')
    
    # ## if statement to check if input text exists in ngram model
    # if(input_text_ngram %in% ngram_model$history_text){
    #     
    #     ## Filter training data on input_text_ngram
    #     preds <- ngram_model[history_text == input_text_ngram, next_word]
    # }
    

    ## Filter training data on input_text_ngram
    preds[[i]] <- ngram_model[history_text == input_text_ngram,]

    
}

## Combine lists to one DT
preds <- rbindlist(preds)
preds










## Set vars
## While loop boolean
ngram_found <- FALSE

## Counter for model
ngram_model_counter = 1
input_text_length_counter = 3

# set up list of dataframes
# pred_word_list = list()


## While loop
while(ngram_found == FALSE & input_text_length_counter >0){
    
    ## get data
    # ngram_model_data <- model_list[[ngram_model_counter]]
    
    ## get last n words from string and combine into single character, separated by spaces.
    input_text_ngram <- paste(tail(input_text_split,input_text_length_counter), collapse = ' ')
    
    ## if statement to check if input text exists in ngram model
    if(input_text_ngram %in% ngram_model$history_text){
        
        ## Filter training data on input_text_ngram
        preds <- ngram_model[history_text == input_text_ngram, next_word]
            
            
            
            # ngram_model_data %>%
            # filter(history_text == input_text_ngram) %>%
            # mutate(ngram = paste0(ngram_model_counter+1, "-gram")) %>%
            # select(-history_text)
        
        ## Add dataframe to list
        # pred_word_list[[input_text_length_counter]] <- preds
        
        ## change while condition - end loop
        ngram_found <- TRUE
    } else {
        
        ## Increment counters
        # ngram_model_counter <- ngram_model_counter + 1
        input_text_length_counter <- input_text_length_counter -1
        
    }
    
    
    
}
