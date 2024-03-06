## Functions to create ngram models
## Aiming to make it agnostic of type of ngram
## Should be able to split with argument
## Actually, don't need to define at all. Always just pops off last word

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
        distinct()
    
    
    ## return
    return(ngram_probs)
    
}