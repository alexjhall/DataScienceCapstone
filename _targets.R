## Load targets packages
library(targets)
library(tarchetypes)

## Source all functions (except for _scratchpad.R which you can use for random
## development)
lapply(
  grep(
    list.files('./R', full.names = TRUE),
    # pattern = '_scratchpad\\.R', invert = TRUE, value = TRUE
    pattern = 'scratchpad', invert = TRUE, value = TRUE
  ),
  source
)

## Packages loaded as part of above code.



## This is where you specify your targets and dependencies. See
## https://books.ropensci.org/targets/ for more details.
list(
  
  
  ## *************************************************
  ## Source paths
  ## *************************************************
  
    ## Source en_US data - twitter
    tar_target(
        en_us_data_twitter_path,
        paste0(here::here(), "/data-raw/en_US/en_US.twitter.txt"),
        format = "file"
    ),
    
    ## Source en_US data - blogs
  tar_target(
    en_us_data_blogs_path,
    paste0(here::here(), "/data-raw/en_US/en_US.blogs.txt"),
    format = "file"
  ),
  
  ## Source en_US data - news
  tar_target(
      en_us_data_news_path,
      paste0(here::here(), "/data-raw/en_US/en_US.news.txt"),
      format = "file"
  ),
  
  
  
  
  
  
  
  
  ## *************************************************
  ## Read sources
  ## *************************************************
  
  ## Read in whole en_US data - probably not required
  ## Blogs
  # tar_target(
  #   read_en_us_data_blogs,
  #   readtext::readtext(file = en_us_data_blogs_path)
  # )
  
  
  ## Read sample of twitter data and returns vector
  tar_target(
      sample_en_us_data_twitter,
      sample_text_data(file_path = en_us_data_twitter_path,
                       sample_size = 1000)
  ),
  
  ## Read sample of blogs data and returns vector
  tar_target(
      sample_en_us_data_blogs,
      sample_text_data(file_path = en_us_data_blogs_path,
                       sample_size = 1000)
  ),
  
  ## Read sample of news data and returns vector
  tar_target(
      sample_en_us_data_news,
      sample_text_data(file_path = en_us_data_news_path,
                       sample_size = 1000)
  ),
  
  
  
  ## *************************************************
  ## Cleaning
  ## *************************************************
  
  ## Remove profanity - twitter
  tar_target(
      twitter_noprofan,
      remove_profanity_function(sample_en_us_data_twitter)
  ),
  
  ## Remove profanity - blogs
  tar_target(
      blogs_noprofan,
      remove_profanity_function(sample_en_us_data_blogs)
  ),
  
  
  ## Remove profanity - news
  tar_target(
      news_noprofan,
      remove_profanity_function(sample_en_us_data_news)
  ),
  
  
  
  
  ## Combine above vectors into single dataframe (tibble)
  tar_target(
      combine_source_vectors,
      combine_source_vectors_function(vec_list = list(
          "Twitter" = twitter_noprofan,
          "Blogs" = blogs_noprofan,
          "News" = news_noprofan
      ))
      
  ),
  
  
  ## Pre-processing and tokenisation
  tar_target(
      preprocess_tokenise,
      preprocess_tokenise_function(combine_source_vectors)

  )
 
  
  
  ## *************************************************
  ## Document Term Matrix
  ## *************************************************
  
  
  
  
  
  
  ## *************************************************
  ## Analysis steps
  ## *************************************************
  
  
  
  ## *************************************************
  ## Report
  ## *************************************************
  
  # # ## Quarto main report
  # tarchetypes::tar_quarto(
  #   main_report,
  #   here("Report/campaign-eval-design-report.qmd")
  # ),
  # 

  ## *************************************************
  ## Slides
  ## *************************************************
  
  ## Colours set as global variables at the top of this script.
  
  
  # 
  # ## Quarto main report slides
  # tarchetypes::tar_quarto(
  #   main_report_slides,
  #   here("Slides/campaign-eval-design-slides.qmd")
  # )

  
)
