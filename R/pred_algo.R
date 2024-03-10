## Script for prediction algorithm functions


## Load ngram models



## Generate predicted words from input
predict_words_function <- function(input_text, model_list){
    
    ## Timer - end time
    start.time <- Sys.time()
    
    
    ## Split based on space
    input_text_split <- as.character(str_split(input_text, pattern = " ", simplify = TRUE))
    
    ## Reduce size to only look at last x words
    input_text_split <- tail(input_text_split, 5)
    
    ## To be used in control flow
    input_text_length <- length(input_text_split)
    
    
    
    ## put into while loop
    
    # set up counter
    input_text_length_counter = 1
    
    # set up list of dataframes?
    pred_word_list = list()
    
    # While loop
    while(input_text_length_counter < input_text_length){ 
        
        ## get data
        ngram_model_data <- model_list[[input_text_length_counter]]
        
        ## get last n words from string and combine into single character, separated by spaces.
        input_text_ngram <- paste(tail(input_text_split,input_text_length_counter), collapse = ' ')
        
        ## Filter training data on input_text_ngram
        preds <- 
            ngram_model_data %>%
            filter(history_text == input_text_ngram) %>%
            mutate(ngram = paste0(input_text_length_counter+1, "-gram")) %>%
            select(-history_text)
        
        ## Add dataframe to list
        pred_word_list[[input_text_length_counter]] <- preds
        
        ## Increment counter
        input_text_length_counter <- input_text_length_counter + 1
    }
    
    
    
    ## Append unigram words to list
    pred_word_list <- append(pred_word_list, list(model_list[[6]]))
    
    
    ## Collapse list into single dataframe
    pred_word_df <- 
        bind_rows(pred_word_list) %>%
        arrange(ngram, next_word)
    
    
    ## Pivot wider
    pred_word_df <-
        pred_word_df %>%
        pivot_wider(
            names_from = "ngram",
            # names_prefix = "prob_",
            values_from = "next_word_prob",
            values_fill = NA
        )
    
    
    ## Look for missing columns
    ## Vector of ngram names, to be used for intersection
    ngram_names <- 
        c(
            "1-gram",
            "2-gram",
            "3-gram",
            "4-gram",
            "5-gram",
            "6-gram"
        )
    ## Pull missing columns
    missing_ngrams <- subset(ngram_names, !(ngram_names %in% names(pred_word_df)))
    
    ## Append missing columns
    pred_word_df[, missing_ngrams] <- as.numeric(NA)
    
    ## Create combined column
    pred_word_df <- 
        pred_word_df %>%
        mutate(comb_prob = 
                   select(., `1-gram`:`6-gram`) %>%
                   rowSums(na.rm = TRUE)
        ) %>%
        arrange(desc(comb_prob))
    
    ## Get predictions
    pred_top1 <- pred_word_df$next_word[1]
    pred_top3 <- pred_word_df$next_word[1:3]
    pred_top5 <- pred_word_df$next_word[1:5]
    
    
    ## Timer - end time
    end.time <- Sys.time()
    
    ## Get time
    time.taken <- as.numeric(end.time - start.time)
    
    
    ## Make list
    return_list <- list(pred_top1, pred_top3, pred_top5, time.taken)
    
    ## Return
    return(return_list)
    
    
    
}





