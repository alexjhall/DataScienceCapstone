## Scratchpad for working.
## Ignored by command sourcing functions from _targets.R







# ## Trying to make create_ngram_function more efficient
# 
# ## subset data to column with text
# data <- data[, 3]
# 
# ## Remove first object
# rm(data)
# 
# # rename column to be agnostic of ngram type
# names(data) <- "text"
# 
# 
# ## Timer - start time
# testing_start_time <- Sys.time()
# 
# ## split text
# data <-
#     data  %>%
#     separate_wider_regex(text, c(history_text = ".*", " ", next_word = ".*?"))
# 
# ## Timer - end time
# testing_end_time <- Sys.time()
# 
# ## Get time
# time.taken <- testing_end_time - testing_start_time
# print(time.taken)


## ************************************
## data.table version
library(data.table)


## Try to do all in one with chains



## Read in data as above
# data <- tar_read(preprocess_tokenise_bigram_premodel_all)
data <- tar_read(preprocess_tokenise_trigram_premodel_all)


## Timer - start time
testing_start_time <- Sys.time()


## Instead, converts in place
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


## Timer - end time
testing_end_time <- Sys.time()

## Get time
time.taken <- testing_end_time - testing_start_time
print(time.taken)

## size
print(object.size(data), units="Mb")














## ************************************
## data.table version
library(data.table)


## Read in data as above
data <- tar_read(preprocess_tokenise_bigram_premodel_all)


## Convert to data table
# DT <- as.data.table(data)

## Remove first object
# rm(data)

## Instead, converts in place
setDT(data)


## size
print(object.size(data), units="Mb")

## Subset
data <- data[, 3, with=F]

## change names
setnames(data, colnames(data), "text")



## split text

## Timer - start time
testing_start_time <- Sys.time()


## Delete original column too.
# cols = c("text", "history_text","next_word")
# data[, (cols) := c(list(NULL), tstrsplit(text, ' (?=[^ ]*$)', perl=TRUE))]

## Create two new columns and split text
data[, c("history_text","next_word") := tstrsplit(text, ' (?=[^ ]*$)',perl=TRUE)]
## Remove original column
data[, text:=NULL]

## Timer - end time
testing_end_time <- Sys.time()

## Get time
time.taken <- testing_end_time - testing_start_time
print(time.taken)

## size
print(object.size(data), units="Mb")



## Now do group_bys

## History_text_count
## Timer - start time
testing_start_time <- Sys.time()


## History count
data[, history_text_count := .N, by=history_text]

## Timer - end time
testing_end_time <- Sys.time()

## Get time
time.taken <- testing_end_time - testing_start_time
print(time.taken)

## size
print(object.size(data), units="Mb")


## next_word_count
## Timer - start time
testing_start_time <- Sys.time()


## next_word_count
data[, next_word_count := .N, by = .(history_text, next_word)]

## probs
data[, next_word_prob := log(next_word_count / history_text_count), by = .(history_text, next_word)]

## Timer - end time
testing_end_time <- Sys.time()

## Get time
time.taken <- testing_end_time - testing_start_time
print(time.taken)

## size
print(object.size(data), units="Mb")













# %>%
#     group_by(history_text) %>%
#     mutate(history_text_count = n()) %>%
#     ungroup() %>%
#     group_by(history_text, next_word) %>%
#     mutate(
#         next_word_count = n(),
#         next_word_prob = log(next_word_count / history_text_count)
#         # next_word_prob = next_word_count / history_text_count
#     ) %>%
#     ungroup() %>%
#     select(
#         history_text, 
#         next_word,
#         next_word_prob
#     ) %>%
#     distinct() %>%
#     arrange(desc(next_word_prob))










## Testing better cleaning

## Read data
text_vec <- tar_read(twitter_noprofan)

data <- tibble(text = text_vec) %>% mutate(linenumber = row_number())

## regex patterns
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|(RT|via)((?:\\b\\W*@\\w+)+)|[[:punct:]]|[[:digit:]]|[[:symbol:]]|@\\w+|http\\w+|[ \t]{2,}|^\\s+|\\s+$"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"


## Actual tokenisation
data_unnest <- 
    data %>% 
    # mutate(text = str_replace_all(text, replace_reg, "")) %>%
    mutate(text = clean_text_function(text)) %>%
    tidytext::unnest_tokens(
        input = text,
        output = bigram,
        token = "ngrams",
        # pattern = "unnest_reg"
        n = 2
    ) %>%
    # rename(text_source = column_label) %>%
    # dplyr::filter(!grepl('[[:digit:]]', bigram)) %>%
    filter(!is.na(bigram))


# comp_data <-
#     tibble(
#         old = data$text,
#         new = data_unnest
#     )












## Original
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




























## Testing the testing algorithm.

## Load model list
model_list <- tar_read(ngram_model_list)

## Load validation testing data
val_test_data <- tar_read(hist_text_val_split_trigram)

## subset for testing
val_test_data <- val_test_data[sample(nrow(val_test_data), 100), ]




## Add empty columns to dataframe to put results into
val_test_data[, c("pred1_true", "pred3_true", "pred5_true", "time_taken")] <- NA

## change type of time_taken
val_test_data$time_taken <- as.numeric(NA)


## For loop first to check.

testing_start_time <- Sys.time()

for(i in seq(nrow(val_test_data))){
    
    ## Produce list of predictions, including timer output
    output_list <- predict_words_function(
        input_text = val_test_data$history_text[i],
        model_list)
    
    ## Put into variables
    val_test_data$pred1_true[i] <- val_test_data$next_word[i] == output_list[[1]]
    val_test_data$pred3_true[i] <- val_test_data$next_word[i] %in% output_list[[2]]
    val_test_data$pred5_true[i] <- val_test_data$next_word[i] %in% output_list[[3]]
    
    val_test_data$time_taken[i] <- output_list[[4]]
    
}

## Timer - end time
testing_end_time <- Sys.time()

## Get time
time.taken <- testing_end_time - testing_start_time








## *******************************************************
## Summary of accuracy
print(paste("Next word accuracy: ", scales::percent(sum(val_test_data$pred1_true) / nrow(val_test_data))))
print(paste("Top 3 accuracy: ", scales::percent(sum(val_test_data$pred3_true) / nrow(val_test_data))))
print(paste("Top 5 accuracy: ", scales::percent(sum(val_test_data$pred5_true) / nrow(val_test_data))))
print(time.taken)







## *******************************************************
## Backoff version
## *******************************************************


## model list
model_list <- tar_read(ngram_model_backoff_list)

# input_text <- "so then I thought"

## input text questions
# input_text <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
input_text <- "You're the reason why I smile everyday. Can you follow me please? It would mean the" #world
input_text <- "Hey sunshine, can you follow me and make me the" #happiest
input_text <- "Very early observations on the Bills game: Offense still struggling but the" # doesn't know... wrong words
input_text <- "Go on a romantic date at the" # doesn't know... wrong words
input_text <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my" #"own", wrong word
input_text <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some" #time
input_text <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little" #wrong words
input_text <- "Be grateful for the good times and keep the faith during the" #lots of words, down to two word history
input_text <- "If this isn't the cutest thing you've ever seen, then you must be"






## Timer - end time
# start.time <- Sys.time()


## Split based on space
input_text_split <- as.character(str_split(tolower(input_text), pattern = " ", simplify = TRUE))

## Reduce size to only look at last x words
input_text_split <- tail(input_text_split, 5)

## To be used in control flow
input_text_length <- length(input_text_split)


## *******************************************************
## New loop to look for n-gram in each n-gram model


## Set vars
## While loop boolean
ngram_found <- FALSE

## Counter for model
ngram_model_counter = 1
input_text_length_counter = 5

# set up list of dataframes
# pred_word_list = list()


## While loop
while(ngram_found == FALSE & input_text_length_counter >0){
    
    ## get data
    ngram_model_data <- model_list[[ngram_model_counter]]
    
    ## get last n words from string and combine into single character, separated by spaces.
    input_text_ngram <- paste(tail(input_text_split,input_text_length_counter), collapse = ' ')
    
    ## if statement to check if input text exists in ngram model
    if(input_text_ngram %in% ngram_model_data$history_text){
        
        ## Filter training data on input_text_ngram
        preds <- 
            ngram_model_data %>%
            filter(history_text == input_text_ngram) %>%
            mutate(ngram = paste0(ngram_model_counter+1, "-gram")) %>%
            select(-history_text)
        
        ## Add dataframe to list
        # pred_word_list[[input_text_length_counter]] <- preds
        
        ## change while condition - end loop
        ngram_found <- TRUE
    } else {
        
        ## Increment counters
        ngram_model_counter <- ngram_model_counter + 1
        input_text_length_counter <- input_text_length_counter -1
        
    }
    
    
    
}










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





























## testing

## Timer - end time
start.time <- Sys.time()

## Get outputs
output_list <- predict_words_function(
    input_text = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
    model_list)

## Timer - end time
end.time <- Sys.time()

## Get time
time.taken <- end.time - start.time
print(time.taken)

output_list[[3]]

## Next


data <- tar_read(hist_text_val_split_all)

train_data <- tar_read(bigram_model)


## Load models
unigram <- tar_read(unigram_model)
bigram <- tar_read(bigram_model)
trigram <- tar_read(trigram_model)
quadgram <- tar_read(quadgram_model)
fivegram <- tar_read(fivegram_model)
sixgram <- tar_read(sixgram_model)

## Create list of models
model_list <- list(
    # unigram,
    bigram,
    trigram,
    quadgram,
    fivegram,
    sixgram
)

## Vector of ngram names, to be used for intersection later
ngram_names <- 
    c(
        "1-gram",
        "2-gram",
        "3-gram",
        "4-gram",
        "5-gram",
        "6-gram"
    )






## probably remove individual models after putting into list (not unigram)



## Build prediction model

## Try bigrams first

## Phrase to predict
input_text <- "do you want to know how much"
# pred_text <- "what"

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



## Append unigram words
## unigram df
## add identifier
unigram_df <- 
    unigram %>%
    mutate(ngram = "1-gram")

## rename
names(unigram_df) <- c("next_word", "next_word_prob", "ngram")

## append to list
pred_word_list <- append(pred_word_list, list(unigram_df))


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


## Find missing columns
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
    






## Subset long text to search in each ngram

 











## dataframe

pred_df <- 
    tibble(
        ngram = "ngram",
        word = "word",
        prob = "prob"
    )










## tidyverse manipulate data
preds <- 
    train_data %>%
    filter(history_text == pred_text) %>%
    slice_max(n = 5, order_by = next_word_prob)







arrange(desc(next_word_prob)) %>%















print(object.size(data), units = "Mb", standard = "auto", digits = 1L)















## Look at train/val/test split


data <- tar_read(train_val_test_split)

test_data <- rsample::training(data)










## Looking at output
data <- tar_read(sixgram_model)

data <- tar_read(trigram_model)


test <- data %>% filter(next_word_prob<0)


## Working to build function.

data <- tar_read(preprocess_tokenise_bigram_premodel)


## define sample size
sample_n <- 10000

## subset data
# n sample as above
# actual text bigrams only
data_small <- data[sample(nrow(data), sample_n), 3]

# rename column to be agnostic
names(data_small) <- "text"

## split into history and next_word


## create dataframe
bigram_probs <-
    data_small  %>%
    # data  %>%
    separate_wider_regex(text, c(history_text = ".*?", " ", next_word = ".*")) %>%
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

















## Create markov chain model - third approach
## using version with stop_words

## normal size
## Look at bigram model
data <- tar_read(preprocess_tokenise_bigram_premodel)

## define sample size
sample_n <- 10000

## subset data
# n sample as above
# actual text bigrams only
data_small <- data[sample(nrow(data), sample_n), 3]


## create dataframe
bigram_probs <-
    data_small  %>%
    # data  %>%
    separate(bigram, c("current_word", "next_word"), sep = " ") %>%
    group_by(current_word) %>%
    mutate(current_word_count = n()) %>%
    ungroup() %>%
    group_by(current_word, next_word) %>%
    mutate(
        next_word_count = n(),
        next_word_prob = next_word_count / current_word_count
    ) %>%
    ungroup() %>%
    select(
        current_word, 
        next_word,
        next_word_prob
    ) %>%
    distinct()


## check size
print(object.size(bigram_probs), units = "Mb", standard = "auto", digits = 1L)












## Create markov chain model - second approach


## normal size
## Look at bigram model
data <- tar_read(preprocess_tokenise_bigram)

## define sample size
sample_n <- 100000

## subset data
# n sample as above
# actual text bigrams only
data_small <- data[sample(nrow(data), sample_n), 3]


## create dataframe
bigram_probs <-
    # data_small  %>%
    data  %>%
    separate(bigram, c("current_word", "next_word"), sep = " ") %>%
    group_by(current_word) %>%
    mutate(current_word_count = n()) %>%
    ungroup() %>%
    group_by(current_word, next_word) %>%
    mutate(
        next_word_count = n(),
        next_word_prob = next_word_count / current_word_count
    ) %>%
    ungroup() %>%
    select(
        current_word, 
        next_word,
        next_word_prob
    ) %>%
    distinct()


## check size
print(object.size(bigram_probs), units = "Mb", standard = "auto", digits = 1L)















## Create markov chain model - first approach


# ## Look at bigram model
# data <- tar_read(preprocess_tokenise_bigram)
# 
# ## reduce size
# data_small <- data[1:2000,]
# bigram_vec <- data_small$bigram
# 
# # count_bigram_df <- 
# #     data_small %>% count(bigram, sort = TRUE)
# 
# bigram_vec <- data_small$bigram




## normal size
## Look at bigram model
data <- tar_read(preprocess_tokenise_bigram)

## define sample size
sample_n <- 1000

# data_small <- data[1:10000,]
data_small <- data[sample(nrow(data), sample_n), ]
bigram_vec <- data_small$bigram


## create df
# bigram_df <- tibble(bigram = bigram_vec, count = rep(1, length(bigram_vec)))
bigram_df <- tibble(bigram = bigram_vec)


# calc transition probabilities
transition_probs <- 
    bigram_df  %>%
    mutate(current_bigram = bigram,
           next_bigram = lead(bigram)) %>%
    filter(!is.na(next_bigram)) %>%
    group_by(current_bigram) %>%
    mutate(current_bigram_count = n()) %>%
    ungroup() %>%
    group_by(current_bigram, next_bigram) %>%
    mutate(
        next_bigram_count = n(),
        next_bigram_prob = next_bigram_count / current_bigram_count
        ) %>%
    ungroup() %>%
    select(
        current_bigram, 
        next_bigram,
        next_bigram_prob
    ) %>%
    distinct()
    

## transition matrix
transition_matrix_tbl <-
    transition_probs %>%
    pivot_wider(
        names_from = next_bigram,
        values_from = next_bigram_prob,
        values_fill = 0
    ) 

# just names
# transition_matrix_tbl_names <- transition_matrix_tbl$current_bigram

# just probs
transition_matrix_tbl_probs <- 
    transition_matrix_tbl %>%
    select(-current_bigram)

## convert to matrix, excluding first col which is bigram names
transition_matrix <- 
    as.matrix(transition_matrix_tbl_probs)

# ## remove previous objects
# rm(list=setdiff(ls(), "transition_matrix"))
# 
# ## clear memory
# gc()

## Convert matrix to markov chain?
# markov_model <- new("markovchain",
#                     transitionMatrix = transition_matrix,
#                     name = "testmarkov")

## save object down
# saveRDS(markov_model, file = "test_markov_model.RDS") 



## testing markov_model object
# plot(markov_model)


markovchain::predict(markov_model)





predictive_text <- function(text, num_word){
    
    suggest <- markov_model$estimate[ tolower(text), ] %>%
        sort(decreasing = T) %>% 
        head(num_word) 
    
    suggest <- suggest[ suggest > 0] %>% 
        names() %>% 
        str_extract(pattern = "\\s(.*)") %>% 
        str_remove("[ ]")
    
    return(suggest)
}

predictive_text("eye health", 5) 



suggest <- markov_bigram$estimate[ tolower("bus stop"), ] %>%
    sort(decreasing = T) %>% 
    head(5)


## next


















## bigram working
df <- tar_read(combine_source_vectors)


## test
output <- 
    preprocess_bigram_function(df, "bigram", 2)





data_unnest <- 
    df %>% 
    tidytext::unnest_tokens(
        input = text,
        output = bigram,
        token = "ngrams",
        n = 2
    ) %>%
    rename(text_source = column_label) %>%
    dplyr::filter(!grepl('[[:digit:]]', bigram)) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    unite(bigram, word1, word2, sep = " ")



# %>%
#     anti_join(stop_words) %>%
#     










## working 10th Feb 2024
df <- tar_read(preprocess_tokenise)

## counts
df %>% count(word, sort = TRUE)


## grouping?
df %>%
    dplyr::group_by(column_label) %>%
    count(word, sort = TRUE)















## sample data
sample_data <- tar_read(sample_en_us_data_twitter)


## no profan data
noprofan <- tar_read(twitter_noprofan)


length(noprofan)/length(sample_data)







## read summary table
summary_table <- tar_read(meta_summary)













## Try summary stats
# text_data <- tar_read(sample_en_us_data_twitter)

text_data <- tar_read(read_en_us_data_twitter)

## Source
text_source <- "Twitter"


## size
# print(object.size(text_data), units = "auto")

text_size <- print(object.size(text_data), units = "auto")

print(x)


x <- as.numeric(object.size(text_data))

x

## length
length(text_data)

## total number of words
words_per_line <- str_count(text_data, pattern = "\\S+")

total_words <- sum(words_per_line)

## avg number of words
avg_words_per_line <- mean(words_per_line)


## build df
summary_df <- tibble(
    Source = 
)





for (thing in ls()) {
    print(
        object.size(
            get(thing)
        ),
        units='auto')
}




# print(object.size(get(text_data)), units='auto')

print(object.size(get(text_data), units='auto'))



print(object.size(text_data), units = "auto")


lapply(text_data, length)

length(text_data)



stri_count(text_data, regex="\\S+")


str_count(text_data, pattern = "\\S+")



text_data[1:10]




## Try preprocess and tokenise df.
## Try combined df.
## Works!
df <- tar_read(preprocess_tokenise)




## Try combined df.
## Works!
comb_df <- tar_read(combine_source_vectors)






## Review noprofan samples of news and blogs
## Looks pretty good....

## news
news_noprofan <- tar_read(news_noprofan)


## news
blogs_noprofan <- tar_read(blogs_noprofan)



## Now work on function to combine.
twitter_noprofan <- tar_read(twitter_noprofan)
news_noprofan <- tar_read(news_noprofan)
blogs_noprofan <- tar_read(blogs_noprofan)


## Create tibbles from each
twitter_noprofan <- tibble(text = twitter_noprofan) %>% mutate(linenumber = row_number())
news_noprofan <- tibble(text = news_noprofan) %>% mutate(linenumber = row_number())
blogs_noprofan <- tibble(text = blogs_noprofan) %>% mutate(linenumber = row_number())



## Create list from above
dflist = list(
    "Twitter" = twitter_noprofan,
    "Blogs" = blogs_noprofan,
    "News" = news_noprofan
)


## Combine
comb_df <- bind_rows(dflist, .id = "column_label")
















## Look at raw data
# data <- tar_read(read_en_us_data_blogs)
raw_data <- tar_read(sample_en_us_data_twitter)

# data <- readtext::readtext(data)
data <- tibble(text = raw_data) %>%
        mutate(linenumber = row_number())

# ## unnest with assignment
# ## Default splits on whitespace
# data_unnest <- 
#     data %>% 
#         tidytext::unnest_tokens(
#             input = text,
#             output = word
#     )
# 
# 
# ## adding step to remove stopwords
# data_unnest <- 
#     data %>% 
#     tidytext::unnest_tokens(
#         input = text,
#         output = word
#     ) %>%
#     anti_join(stop_words)
# 
# 
# ## Simple count
# freq_list <- data_unnest %>%
#     count(word, sort = TRUE)
# 
# ## With chart
# data_unnest %>%
#     count(word, sort = TRUE) %>%
#     filter(n > 1) %>%
#     mutate(word = reorder(word, n)) %>%
#     ggplot(aes(n, word)) +
#     geom_col() +
#     labs(y = NULL)





## More complete process w/ count
data_unnest_count <- 
    data %>% 
    tidytext::unnest_tokens(
        input = text,
        output = word
    ) %>%
    dplyr::filter(!grepl('[[:digit:]]', word)) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE)

## More complete process
data_unnest <- 
    data %>% 
    tidytext::unnest_tokens(
        input = text,
        output = word
    ) %>%
    dplyr::filter(!grepl('[[:digit:]]', word)) %>%
    anti_join(stop_words)


## profanity working
library(sentimentr)

## function
profanity_table <- sentimentr::profanity(data$text)







## Example
#Flag records that have profanity
# df %>% 
#     rowwise() %>% 
#     mutate(ProfanityCount = sum(profanity(ingredients, profanity_list = customCussWords)$profanity_count)) %>% 
#     filter(ProfanityCount > 0)



## Running on my data
data_unnest_profan <- 
    data %>% 
    tidytext::unnest_tokens(
        input = text,
        output = word
    ) %>%
    dplyr::filter(!grepl('[[:digit:]]', word)) %>%
    anti_join(stop_words) %>%
    rowwise() %>% 
    mutate(ProfanityCount = sum(profanity(word)$profanity_count))

## simpler?
words <- tibble(data_unnest$word)

# Add profan count
words_profan <-
words %>%
    mutate(ProfanityCount = profanity(`data_unnest$word`)$profanity_count)








## New version of profan with tibble
# df <- tibble(text = raw_data) %>%
#     mutate(linenumber = row_number()) %>%
    


## sentence boundary disambiguation
element_profan_count <- 
    sentimentr::get_sentences(raw_data) %>%
    profanity() %>%
    group_by(element_id) %>%
    summarise(profan_count = sum(profanity_count)) %>%
    filter(profan_count > 0)

## profan data index
## vector to be used for subsetting
profan_index <- element_profan_count$element_id


## Filter raw data to remove lines with profan
noprofan_data <- raw_data[-profan_index]
profan_data <- raw_data[profan_index]















## Custom function to tokenize, but included hypthenated words

tokenize_hyphenated_words <- function(x, lowercase = TRUE) {
    if (lowercase)
        x <- str_to_lower(x)
    
    str_split(x, "[:space:]") %>%
        map(~ str_remove_all(.x, "^[:punct:]+|[:punct:]+$"))
}

## Need to remove numbers
## Need to change to remove.... ignore. 


## Testing function
data$text %>% tokenize_hyphenated_words()


















## Load data with tidytext?
## No, sample with readlines loop

## Set params
file_path <- here::here("data-raw/en_US/en_US.twitter.txt")
sample_size <- 100  # Define the size of the random sample


# Open the .txt file and read lines while sampling
# Set up vector
random_sample <- character()
# Set up connection
con <- file(file_path, "r")

# While loop
while (length(random_sample) < sample_size) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) break  # End of file
    random_sample <- c(random_sample, line)
}

# Close connection
close(con)








## Or actually use tidytext.




