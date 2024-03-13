## Functions to create ngram models


## Unigram
create_unigram_function <- function(data){
    
    ## Convert to data.table in place
    setDT(data)
    
    ## Subset
    data <- data[, 3, with=F]
    
    ## change names
    setnames(data, colnames(data), "text")
    
    ## total_word_count
    data[, total_word_count := .N]
    ## text count and prob
    data[, text_count := .N, by=text][, text_prob := log(text_count / total_word_count)]
    ## Remove columns not needed
    data[, c("text_count","total_word_count"):=NULL]
    # unique(data)
    data <- unique(data)
    ## Set keys
    setkey(data, text)
    
    
    ## return
    return(data)
    
    
}



## >1 ngrams

## Create function
create_ngram_function <- function(data){
    
    
    ## Convert to data.table in place
    setDT(data)
    
    ## Subset
    data <- data[, 3, with=F]
    
    ## change names
    setnames(data, colnames(data), "text")
    
    
    ## Create two new columns and split text
    data[, c("history_text","next_word") := tstrsplit(text, ' (?=[^ ]*$)',perl=TRUE)]
    ## Remove original column
    data[, text:=NULL]
    
    ## history_text_count
    data[, history_text_count := .N, by=history_text]
    ## next_word_count and prob
    data[, next_word_count := .N, by = .(history_text, next_word)][, next_word_prob := log(next_word_count / history_text_count)]
    ## Remove columns not needed
    data[, c("history_text_count","next_word_count"):=NULL]
    # unique(data)
    data <- unique(data)
    ## Set keys
    setkey(data, history_text)
    
    
    ## return
    return(data)
    
}



## Create validation testing dataset
## This uses the validation ngram models and then splits them into history and next word
## This can be used for validating testing
## The history will be fed into the prediction algorithm, then it will try to predict what the next word will be
## This can be compared against what the actual next word was.

split_ngram_function <- function(data){
    
    
    
    ## subset data to column with text
    data <- data[, 3]
    
    ## Pull out ngram type as char
    ngram_type <- names(data)
    
    ## Add column with value of text column header
    data <- 
        data %>%
        mutate(ngram = rep(ngram_type, nrow(data))) %>%
        relocate(last_col()) 
    
    # rename column to be agnostic of ngram type
    names(data) <- "text"
    names(data) <- c(
        "ngram",
        "text"
    )
    
    ## Create dataframe, splitting into histry and next word
    data <-
        data  %>%
        separate_wider_regex(text, c(history_text = ".*", " ", next_word = ".*?"))
        
 
    ## Return
    return(data)
       
}


## Model list
## Does include unigram as 6th item in list.
ngrams_into_list_function <- function(
        unigram,
        bigram,
        trigram,
        quadgram,
        fivegram,
        sixgram) {
    
    ## manipulate unigram model slightly
    
    ## Append unigram words
    ## unigram df
    ## add identifier
    unigram_df <- 
        unigram %>%
        mutate(ngram = "1-gram")
    
    ## rename
    names(unigram_df) <- c("next_word", "next_word_prob", "ngram")
    
    
    ## Create list of models, with unigram at the end
    model_list <- list(
        bigram,
        trigram,
        quadgram,
        fivegram,
        sixgram,
        unigram_df
    )
    
    
    ## Return list
    return(model_list)
    
    
    
    
}

## Model list for backoff method
## Basically a different order
ngrams_into_backoff_list_function <- function(
        unigram,
        bigram,
        trigram,
        quadgram,
        fivegram,
        sixgram) {
    
    ## manipulate unigram model slightly
    
    ## Append unigram words
    ## unigram df
    ## add identifier
    unigram_df <- 
        unigram %>%
        mutate(ngram = "1-gram")
    
    ## rename
    names(unigram_df) <- c("next_word", "next_word_prob", "ngram")
    
    
    ## Create list of models, with unigram at the end
    model_list <- list(
        sixgram,
        fivegram,
        quadgram,
        trigram,
        bigram,
        unigram_df
    )
    
    
    ## Return list
    return(model_list)
    
    
    
    
}








## Get model size
## returns list
## 1) dataframe with size per model
## 2) size as single number, in MB
get_model_size <- function() {
    
    ## Load models
    unigram <- tar_read(unigram_model)
    bigram <- tar_read(bigram_model)
    trigram <- tar_read(trigram_model)
    quadgram <- tar_read(quadgram_model)
    fivegram <- tar_read(fivegram_model)
    sixgram <- tar_read(sixgram_model)
    
    ## Create list of models
    model_list <- list(
        unigram,
        bigram,
        trigram,
        quadgram,
        fivegram,
        sixgram
    )
    
    ## Name items in list
    names(model_list) <- c(
        "unigram_model",
        "bigram_model",
        "trigram_model",
        "quadgram_model",
        "fivegram_model",
        "sixgram_model"
    )
    
    ## Get size of each model
    model_sizes <- sapply(model_list,function(x){as.numeric(object.size(x))})
    
    ## put into MB
    model_sizes <- round(model_sizes / 1000000, 2)
    
    ## total
    model_size_total <- round(sum(model_sizes), 2)
    
    ## create dataframe
    model_size_df <- tibble(
        model = names(model_list),
        size_mb = model_sizes
    )
    
    ## output list
    output_list <- list(model_size_df, model_size_total)
    
    ## return
    return(output_list)
    
}




