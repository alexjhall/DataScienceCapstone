## Scratchpad for working.
## Ignored by command sourcing functions from _targets.R



## Check size of models



sixgram <- tar_read(sixgram_model)
bigram <- tar_read(bigram_model)
## check size
print(object.size(bigram), units = "Mb", standard = "auto", digits = 1L)








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




