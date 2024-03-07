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


## *************************************************
## First set of functions informs EDA milestone report, removes stopwords


## Define function to preprocess, including:
## remove stop words, digits, punctuation
## as well as tokenise into words

preprocess_tokenise_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = word
        ) %>%
        dplyr::filter(!grepl('[[:digit:]]', word)) %>%
        anti_join(stop_words) %>%
        rename(text_source = column_label)
    
    
    ## Specify what to return
    return(data_unnest)
    
}


## Define function to preprocess, including:
## remove stop words, digits, punctuation
## as well as tokenise into bi-grams

preprocess_bigram_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = bigram,
            token = "ngrams",
            n = 2
        ) %>%
        rename(text_source = column_label) %>%
        dplyr::filter(!grepl('[[:digit:]]', bigram)) %>%
        filter(!is.na(bigram)) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        unite(bigram, word1, word2, sep = " ")
    
    
    ## Specify what to return
    return(data_unnest)
    
}



## Define function to preprocess, including:
## remove stop words, digits, punctuation
## as well as tokenise into tri-grams

preprocess_trigram_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = trigram,
            token = "ngrams",
            n = 3
        ) %>%
        rename(text_source = column_label) %>%
        dplyr::filter(!grepl('[[:digit:]]', trigram)) %>%
        filter(!is.na(trigram)) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word3 %in% stop_words$word) %>%
        unite(trigram, word1, word2, word3, sep = " ")
    
    
    ## Specify what to return
    return(data_unnest)
    
}


## Define function to preprocess, including:
## remove stop words, digits, punctuation
## as well as tokenise into quad-grams

preprocess_quadgram_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = quadgram,
            token = "ngrams",
            n = 4
        ) %>%
        rename(text_source = column_label) %>%
        dplyr::filter(!grepl('[[:digit:]]', quadgram)) %>%
        filter(!is.na(quadgram)) %>%
        separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        filter(!word3 %in% stop_words$word) %>%
        filter(!word4 %in% stop_words$word) %>%
        unite(quadgram, word1, word2, word3, word4, sep = " ")
    
    
    ## Specify what to return
    return(data_unnest)
    
}





## *************************************************
## Second set of functions is for later modelling
## labelled as 'premodel'
## first set can be assumed to be 'EDA'

## Also goes up to sixgrams

# Does not remove stopwords
# Does other steps to incorporate <UNK> etc etc


## Unigram
preprocess_tokenise_premodel_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = word
        ) %>%
        dplyr::filter(!grepl('[[:digit:]]', word)) %>%
        rename(text_source = column_label)
    
    
    ## Specify what to return
    return(data_unnest)
    
}


## Bigram
preprocess_bigram_premodel_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = bigram,
            token = "ngrams",
            n = 2
        ) %>%
        rename(text_source = column_label) %>%
        dplyr::filter(!grepl('[[:digit:]]', bigram)) %>%
        filter(!is.na(bigram))
    
    
    ## Specify what to return
    return(data_unnest)
    
}



## Trigram
preprocess_trigram_premodel_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = trigram,
            token = "ngrams",
            n = 3
        ) %>%
        rename(text_source = column_label) %>%
        dplyr::filter(!grepl('[[:digit:]]', trigram)) %>%
        filter(!is.na(trigram))
    
    ## Specify what to return
    return(data_unnest)
    
}


## Quadgram
preprocess_quadgram_premodel_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = quadgram,
            token = "ngrams",
            n = 4
        ) %>%
        rename(text_source = column_label) %>%
        dplyr::filter(!grepl('[[:digit:]]', quadgram)) %>%
        filter(!is.na(quadgram))
    
    
    ## Specify what to return
    return(data_unnest)
    
}


## Fivegram
preprocess_fivegram_premodel_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = fivegram,
            token = "ngrams",
            n = 5
        ) %>%
        rename(text_source = column_label) %>%
        dplyr::filter(!grepl('[[:digit:]]', fivegram)) %>%
        filter(!is.na(fivegram))
    
    
    ## Specify what to return
    return(data_unnest)
    
}



## Sixgram
preprocess_sixgram_premodel_function <- function (data){
    
    data_unnest <- 
        data %>% 
        tidytext::unnest_tokens(
            input = text,
            output = sixgram,
            token = "ngrams",
            n = 6
        ) %>%
        rename(text_source = column_label) %>%
        dplyr::filter(!grepl('[[:digit:]]', sixgram)) %>%
        filter(!is.na(sixgram))
    
    
    ## Specify what to return
    return(data_unnest)
    
}





