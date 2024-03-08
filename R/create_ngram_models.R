## Functions to create ngram models


## Unigram
create_unigram_function <- function(data){
    
    ## subset data to column with text
    data_text <- data[, 3]
    
    # rename column to be agnostic of ngram type
    names(data_text) <- "text"
    
    ## create dataframe
    ngram_probs <-
        data_text  %>%
        mutate(total_word_count = n()) %>%
        group_by(text) %>%
        mutate(
            text_count = n(),
            text_prob = text_count /total_word_count
        ) %>%
        ungroup() %>%
        select(
            text,
            text_prob
        ) %>%
        distinct() %>%
        arrange(desc(text_prob))
    
    
    ## return
    return(ngram_probs)
    
    
    
}



## >1 ngrams

## Create function
create_ngram_function <- function(data){
    
    ## subset data to column with text
    data_text <- data[, 3]
    
    # rename column to be agnostic of ngram type
    names(data_text) <- "text"

    
    ## create dataframe
    ngram_probs <-
        data_text  %>%
        separate_wider_regex(text, c(history_text = ".*", " ", next_word = ".*?")) %>%
        group_by(history_text) %>%
        mutate(history_text_count = n()) %>%
        ungroup() %>%
        group_by(history_text, next_word) %>%
        mutate(
            next_word_count = n(),
            next_word_prob = next_word_count / history_text_count
        ) %>%
        ungroup() %>%
        select(
            history_text, 
            next_word,
            next_word_prob
        ) %>%
        distinct() %>%
        arrange(desc(next_word_prob))
    
    
    ## return
    return(ngram_probs)
    
}



## Create validation testing dataset
## This uses the validation ngram models and then splits them into history and next word
## This can be used for validating testing
## The history will be fed into the prediction algorithm, then it will try to predict what the next word will be
## This can be compared against what the actual next word was.

split_ngram_function <- function(data){
    
    
    
    ## subset data to column with text
    data_text <- data[, 3]
    
    ## Pull out ngram type as char
    ngram_type <- names(data_text)
    
    ## Add column with value of text column header
    data_text <- 
        data_text %>%
        mutate(ngram = rep(ngram_type, nrow(data_text))) %>%
        relocate(last_col()) 
    
    # rename column to be agnostic of ngram type
    names(data_text) <- "text"
    names(data_text) <- c(
        "ngram",
        "text"
    )
    
    ## Create dataframe, splitting into histry and next word
    split_ngram <-
        data_text  %>%
        separate_wider_regex(text, c(history_text = ".*", " ", next_word = ".*?"))
        
 
    ## Return
    return(split_ngram)
       
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




