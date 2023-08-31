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

## Set colours using function sourced in above code.
set_colours_function()


## This is where you specify your targets and dependencies. See
## https://books.ropensci.org/targets/ for more details.
list(
  
  
  ## *************************************************
  ## Source paths
  ## *************************************************
  ## Source Enforcement campaign data
  tar_target(
    ec_data_path,
    paste0(here::here(), "/data-raw/CommsTrackerDataEnforcement.xlsx"),
    format = "file"
  )
  
  
  
  
  ## *************************************************
  ## Read sources
  ## *************************************************
  
  # ## Read in EC data
  # tar_target(
  #   read_ec_data,
  #   readxl::read_excel(path = ec_data_path, col_types="text")
  # ),
  
  
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
