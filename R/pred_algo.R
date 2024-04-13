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
                   exp(rowSums(na.rm = TRUE))
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



## Generate predicted words from input
## second version using DT ngram model
predict_words_function_v2 <- function(input_text, ngram_model, unigram_model){
    
    ## Timer - end time
    start.time <- Sys.time()
    
    
    
    ## *************************************************
    ## clean and parse input test
    
    ## clean input text to remove punctuation.
    input_text <- clean_text_function(input_text)
    
    ## Split based on space
    input_text_split <- as.character(str_split(tolower(input_text), pattern = " ", simplify = TRUE))
    
    ## Reduce size to only look at last x words
    input_text_split <- tail(input_text_split, 5)
    
    ## To be used in control flow
    input_text_length <- length(input_text_split)
    
    
    
    ## Set up list for each set of predictions
    preds <- list()
    
    
    ## ***********************************
    ## Get predictions from ngram model
    ## For loop
    for (i in 1:input_text_length){
        
        ## get last n words from string and combine into single character, separated by spaces.
        input_text_ngram <- paste(tail(input_text_split,i), collapse = ' ')
        
        ## Filter training data on input_text_ngram
        preds[[i]] <- ngram_model[history_text == input_text_ngram,]
        
        
    }
    
    ## Combine lists to one DT
    preds <- rbindlist(preds)
    
    ## arrange descending by sb_score
    data.table::setorder(preds, -sb_score)
    
    ## make vector with preds results plus unigram model
    ## if pred results has none, or less than 5, unigram model will fill in the blanks
    pred_vec <- c(preds$next_word, unigram_model$text)
    
    
    ## ********************
    ## Pull out top predictions
    
    pred_top1 <- pred_vec[1]
    pred_top3 <- pred_vec[1:3]
    pred_top5 <- pred_vec[1:5]
    
    
    
    ## Timer - end time
    end.time <- Sys.time()
    
    ## Get time
    time.taken <- as.numeric(end.time - start.time)
    
    ## Make list
    return_list <- list(pred_top1, pred_top3, pred_top5, time.taken)
    
    ## Return
    return(return_list)
    
    
    
}


## Function to get test summary
test_prediction_function <- function(test_df, ngram_model_input, unigram_model_input){
    
    ## Add empty columns to dataframe to put results into
    test_df[, c("pred1_true", "pred3_true", "pred5_true", "time_taken")] <- NA
    
    ## change type of time_taken
    test_df$time_taken <- as.numeric(NA)
    
    
    ## For loop first to check.
    
    testing_start_time <- Sys.time()
    
    for(i in seq(nrow(test_df))){
        
        ## Produce list of predictions, including timer output
        output_list <- predict_words_function_v2(
            input_text = test_df$history_text[i],
            ngram_model = ngram_model_input,
            unigram_model = unigram_model_input)
        
        ## Put into variables
        test_df$pred1_true[i] <- test_df$next_word[i] == output_list[[1]]
        test_df$pred3_true[i] <- test_df$next_word[i] %in% output_list[[2]]
        test_df$pred5_true[i] <- test_df$next_word[i] %in% output_list[[3]]
        
        test_df$time_taken[i] <- output_list[[4]]
        
    }
    
    ## Timer - end time
    testing_end_time <- Sys.time()
    
    ## Get time
    time.taken <- testing_end_time - testing_start_time
    
    ## make dataframe
    accuracy_summary_df <- tibble(
        `Next word accuracy` = scales::percent(sum(test_df$pred1_true) / nrow(test_df)),
        `Top 3 accuracy` = scales::percent(sum(test_df$pred3_true) / nrow(test_df)),
        `Top 5 accuracy` =  scales::percent(sum(test_df$pred5_true) / nrow(test_df)),
        `Time taken` = time.taken
    )
    
    
    ## func_output_list
    func_output_list <- list(test_df, accuracy_summary_df)
    
    
    
    # ## *******************************************************
    # ## Summary of accuracy
    # print(paste("Next word accuracy: ", scales::percent(sum(val_test_data$pred1_true) / nrow(val_test_data))))
    # print(paste("Top 3 accuracy: ", scales::percent(sum(val_test_data$pred3_true) / nrow(val_test_data))))
    # print(paste("Top 5 accuracy: ", scales::percent(sum(val_test_data$pred5_true) / nrow(val_test_data))))
    # print(time.taken)
    
    
    
    ## return
    return(func_output_list)
    
    
    
    
    
}

