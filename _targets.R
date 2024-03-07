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

## Set seed
set.seed(202024)

## Targets list
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
  
  ## Read in whole datasets
  ## Twitter
  tar_target(
      read_en_us_data_twitter,
      full_text_data(file_path = en_us_data_twitter_path)
      ),
  
  ## Blogs
  tar_target(
      read_en_us_data_blogs,
      full_text_data(file_path = en_us_data_blogs_path)
  ),


  ## News
  tar_target(
      read_en_us_data_news,
      full_text_data(file_path = en_us_data_news_path)
  ),
  
  
  ## Create summary tables
  
  ## Twitter
  tar_target(
      meta_twitter,
      meta_summary_function(text_data = read_en_us_data_twitter, source = "Twitter")
  ),
  
  ## Blogs
  tar_target(
      meta_blogs,
      meta_summary_function(text_data = read_en_us_data_blogs, source = "Blogs")
  ),
  
  
  ## News
  tar_target(
      meta_news,
      meta_summary_function(text_data = read_en_us_data_news, source = "News")
  ),
  
  
  ## Combine into one table
  tar_target(
      meta_summary,
      rbind(meta_twitter, meta_blogs, meta_news)
  ),
  
  
  ## Read sample of twitter data and returns vector
  tar_target(
      sample_en_us_data_twitter,
      sample_text_data(full_text = read_en_us_data_twitter,
                       sample_size = 10000)
  ),
  
  ## Read sample of blogs data and returns vector
  tar_target(
      sample_en_us_data_blogs,
      sample_text_data(full_text = read_en_us_data_blogs,
                       sample_size = 10000)
  ),
  
  ## Read sample of news data and returns vector
  tar_target(
      sample_en_us_data_news,
      sample_text_data(full_text = read_en_us_data_news,
                       sample_size = 10000)
  ),
  
  
  ## *************************************************
  ## After initial EDA. These steps will take a larger sample. 
  
  ## Read sample of twitter data and returns vector
  tar_target(
      large_sample_twitter,
      sample_text_data(full_text = read_en_us_data_twitter,
                       sample_size = 100000)
  ),
  
  ## Read sample of blogs data and returns vector
  tar_target(
      large_sample_blogs,
      sample_text_data(full_text = read_en_us_data_blogs,
                       sample_size = 100000)
  ),
  
  ## Read sample of news data and returns vector
  tar_target(
      large_sample_news,
      sample_text_data(full_text = read_en_us_data_news,
                       sample_size = 100000)
  ),
  
  
  
  
  ## *************************************************
  ## Cleaning
  ## *************************************************
  
  ## *************************************************
  ## Remove profanity
  
  ## *************************************************
  ## Version for EDA
  
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
  
  
  ## *************************************************
  ## Larger sample version
  
  ## Remove profanity - twitter
  tar_target(
      twitter_noprofan_lg,
      remove_profanity_function(large_sample_twitter)
  ),
  
  ## Remove profanity - blogs
  tar_target(
      blogs_noprofan_lg,
      remove_profanity_function(large_sample_blogs)
  ),
  
  
  ## Remove profanity - news
  tar_target(
      news_noprofan_lg,
      remove_profanity_function(large_sample_news)
  ),
  
  
  
  
  ## Combine above vectors into single dataframe (tibble)
  tar_target(
      combine_source_vectors_lg,
      combine_source_vectors_function(vec_list = list(
          "Twitter" = twitter_noprofan_lg,
          "Blogs" = blogs_noprofan_lg,
          "News" = news_noprofan_lg
      ))
      
  ),
  
  
  ## *************************************************
  ## Train/val/test split
  ## *************************************************
  
  ## Split combined dataset into train/val/test
  ## Returns list which needs to be subset
  
  tar_target(
      train_val_test_split,
      initial_validation_split(combine_source_vectors_lg, prop = c(0.6, 0.2))
  ),
  
  
  
  ## *************************************************
  ## Tokenisation
  ## *************************************************
  
  ## *************************************************
  ## Tokenisation, without stopwords
  
  
  ## Pre-processing and tokenisation into single words
  tar_target(
      preprocess_tokenise,
      preprocess_tokenise_function(combine_source_vectors)

  ),
  
  
  ## Pre-processing and tokenisation into bigrams
  tar_target(
      preprocess_tokenise_bigram,
      preprocess_bigram_function(combine_source_vectors)
      
  ),
  
  ## Pre-processing and tokenisation into trigrams
  tar_target(
      preprocess_tokenise_trigram,
      preprocess_trigram_function(combine_source_vectors)
      
  ),
  
  ## Pre-processing and tokenisation into quadgrams
  tar_target(
      preprocess_tokenise_quadgram,
      preprocess_quadgram_function(combine_source_vectors)
      
  ),
  
  
  ## *************************************************
  ## Tokenisation with stop words included
  
  
  ## Pre-processing and tokenisation into single words
  tar_target(
      preprocess_tokenise_premodel,
      preprocess_tokenise_premodel_function(rsample::training(train_val_test_split))

  ),


  ## Pre-processing and tokenisation into bigrams
  tar_target(
      preprocess_tokenise_bigram_premodel,
      preprocess_bigram_premodel_function(rsample::training(train_val_test_split))

  ),

  ## Pre-processing and tokenisation into trigrams
  tar_target(
      preprocess_tokenise_trigram_premodel,
      preprocess_trigram_premodel_function(rsample::training(train_val_test_split))

  ),

  ## Pre-processing and tokenisation into quadgrams
  tar_target(
      preprocess_tokenise_quadgram_premodel,
      preprocess_quadgram_premodel_function(rsample::training(train_val_test_split))

  ),

  ## Pre-processing and tokenisation into fivegrams
  tar_target(
      preprocess_tokenise_fivegram_premodel,
      preprocess_fivegram_premodel_function(rsample::training(train_val_test_split))

  ),

  ## Pre-processing and tokenisation into sixgrams
  tar_target(
      preprocess_tokenise_sixgram_premodel,
      preprocess_sixgram_premodel_function(rsample::training(train_val_test_split))

  ),


  ## *************************************************
  ## Create ngrams
  ## *************************************************

  ## Unigram - Need to work on this


  ## Bigram
  tar_target(
      bigram_model,
      create_ngram_function(preprocess_tokenise_bigram_premodel)
  ),

  ## Trigram
  tar_target(
      trigram_model,
      create_ngram_function(preprocess_tokenise_trigram_premodel)
  ),

  ## Quadgram
  tar_target(
      quadgram_model,
      create_ngram_function(preprocess_tokenise_quadgram_premodel)
  ),

  ## Fivegram
  tar_target(
      fivegram_model,
      create_ngram_function(preprocess_tokenise_fivegram_premodel)
  ),

  ## Sixgram
  tar_target(
      sixgram_model,
      create_ngram_function(preprocess_tokenise_sixgram_premodel)
  ),
  
  
  
  
  ## *************************************************
  ## Report
  ## *************************************************
  
  ## Quarto milestone report
  tarchetypes::tar_quarto(
    milestone_report,
    here("reports/milestone-report.qmd")
  )


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
