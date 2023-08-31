## Load targets packages
library(targets)
library(tarchetypes)

## Source all functions (except for _scratchpad.R which you can use for random
## development)
lapply(
  grep(
    list.files('./R', full.names = TRUE),
    pattern = '_scratchpad\\.R', invert = TRUE, value = TRUE
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
  
  ## Source en_US data - twitter
  tar_target(
      en_us_data_twitter_path,
      paste0(here::here(), "/data-raw/en_US/en_US.twitter.txt"),
      format = "file"
  ),
  
  
  
  
  
  
  ## *************************************************
  ## Read sources
  ## *************************************************
  
  ## Read in en_US data
  ## Blogs
  tar_target(
    read_en_us_data_blogs,
    readtext::readtext(file = en_us_data_blogs_path)
  )
  
  
  
  
  ## *************************************************
  ## Cleaning
  ## *************************************************
  
  
  
 
  
  
  ## *************************************************
  ## Subsets of data
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
