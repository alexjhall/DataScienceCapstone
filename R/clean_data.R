## Script to contain numerous functions to clean data


## Function to remove items from a text vector that contain profanity.
## Define function
remove_profanity_function <- function(data){
    
    ## sentence boundary disambiguation
    element_profan_count <- 
        sentimentr::get_sentences(data) %>%
        profanity() %>%
        group_by(element_id) %>%
        summarise(profan_count = sum(profanity_count)) %>%
        filter(profan_count > 0)
    
    ## profan data index
    ## vector to be used for subsetting
    profan_index <- element_profan_count$element_id
    
    
    ## Filter raw data to remove lines with profan
    noprofan_data <- data[-profan_index]
    # profan_data <- data[profan_index]
    
    
    ## Return
    return(noprofan_data)
}




## Function to combine sources
combine_source_vectors_function <- function(vec_list){
    
    
    ## Create tibbles from each vector
    twitter_noprofan <- tibble(text = vec_list[['Twitter']]) %>% mutate(linenumber = row_number())
    news_noprofan <- tibble(text = vec_list[['News']]) %>% mutate(linenumber = row_number())
    blogs_noprofan <- tibble(text = vec_list[['Blogs']]) %>% mutate(linenumber = row_number())
    
    
    ## Create list from above
    dflist = list(
        "Twitter" = twitter_noprofan,
        "Blogs" = blogs_noprofan,
        "News" = news_noprofan
    )
    
    
    ## Combine
    comb_df <- bind_rows(dflist, .id = "column_label")
    
    
    ## Return single dataframe
    return(comb_df)
    
    
}





## Define function to preprocess, including:
## remove stop words, digits, punctuation
## as well as tokenise

preprocess_tokenise_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = word
        ) %>%
        dplyr::filter(!grepl('[[:digit:]]', word)) %>%
        anti_join(stop_words)
    
    
    ## Specify what to return
    return(data_unnest)
    
}




