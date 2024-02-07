## Scratchpad for working.
## Ignored by command sourcing functions from _targets.R



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




