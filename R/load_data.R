## Load sample of data and return a vector



## Define function
sample_text_data <- function(file_path, sample_size){
    
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
    
    
    
    ## Return character vector
    return(random_sample)
    

    
}

