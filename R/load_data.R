## Data load functions

## Load full data 
## Define function
full_text_data <- function(file_path){
    
    # full_text <- readLines(file_path, n=1000)
    full_text <- readr::read_lines(file_path)
    
    # Return character vector
    return(full_text)

}


## Summary stats for meta detail on each source

## Build function
meta_summary_function <- function(text_data, source){
    
    ## Source
    ## Passed to function 
    
    ## Size
    text_size <- round(as.numeric(object.size(text_data))/1000000, 2)
        
    ## Length
    text_length <- format(length(text_data), nsmall=1, big.mark=",")
    
    ## words per line
    words_per_line <- str_count(text_data, pattern = "\\S+")
    
    ## total number of words
    total_words <- format(sum(words_per_line), nsmall=1, big.mark=",")
    
    ## avg number of words per line
    avg_words_per_line <- round(mean(words_per_line), 2)
    
    ## stdv
    sd_words_per_line <- round(sd(words_per_line), 2)
    
    
    ## build df
    summary_df <- tibble(
        Source = source,
        `Size (MB)` = text_size,
        `Number of lines` = text_length,
        `Number of words` = total_words,
        `Mean words per line` = avg_words_per_line,
        `SD words per line` = sd_words_per_line
    )


    ## Return
    return(summary_df)


}





## Load sample of data and return a vector
## Define function
sample_text_data <- function(full_text, sample_size){
    
    ## Take random sample
    random_sample <- base::sample(full_text, size = sample_size)
    
    
    ## Return character vector
    return(random_sample)
    

    
}

