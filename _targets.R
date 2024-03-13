## Load targets packages
library(targets)
library(tarchetypes)

## Source all functions (except for _scratchpad.R which you can use for random
## development)
## This also loads packages.
lapply(
  grep(
    list.files('./R', full.names = TRUE),
    # pattern = '_scratchpad\\.R', invert = TRUE, value = TRUE
    pattern = 'scratchpad', invert = TRUE, value = TRUE
  ),
  source
)



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
  ## Create training ngrams
  ## *************************************************

  ## Unigram - Need to work on this
  tar_target(
      unigram_model,
      create_unigram_function(preprocess_tokenise_premodel)
  ),

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
  
  ## Model list (exluding unigram)
  tar_target(
      ngram_model_list,
      ngrams_into_list_function(
          unigram = unigram_model,
          bigram = bigram_model,
          trigram = trigram_model,
          quadgram = quadgram_model,
          fivegram = fivegram_model,
          sixgram = sixgram_model
          )
  ),
  
  ## Model list (backoff)
  tar_target(
      ngram_model_backoff_list,
      ngrams_into_backoff_list_function(
          unigram = unigram_model,
          bigram = bigram_model,
          trigram = trigram_model,
          quadgram = quadgram_model,
          fivegram = fivegram_model,
          sixgram = sixgram_model
      )
  ),
  
  
  ## *************************************************
  ## Create validation testing ngrams
  ## *************************************************
  
  
  ## *************************************************
  ## Validation testing version 
  ## Tokenisation with stop words included
  
  ## unigrams
  ## not included yet.
  
  ## Pre-processing and tokenisation into bigrams
  tar_target(
      preprocess_tokenise_bigram_premodel_val,
      preprocess_bigram_premodel_function(rsample::validation(train_val_test_split))
  ),
  
  ## Pre-processing and tokenisation into trigrams
  tar_target(
      preprocess_tokenise_trigram_premodel_val,
      preprocess_trigram_premodel_function(rsample::validation(train_val_test_split))
  ),
  
  ## Pre-processing and tokenisation into quadgrams
  tar_target(
      preprocess_tokenise_quadgram_premodel_val,
      preprocess_quadgram_premodel_function(rsample::validation(train_val_test_split))
  ),
  
  ## Pre-processing and tokenisation into fivegrams
  tar_target(
      preprocess_tokenise_fivegram_premodel_val,
      preprocess_fivegram_premodel_function(rsample::validation(train_val_test_split))
  ),
  
  ## Pre-processing and tokenisation into sixgrams
  tar_target(
      preprocess_tokenise_sixgram_premodel_val,
      preprocess_sixgram_premodel_function(rsample::validation(train_val_test_split))
  ),
  
  
  ## *************************************************
  ## Create validation testing history/text datasets
  ## *************************************************
  
  ## Essentially split each into 'history' and 'next_word'
  ## Do this for each ngram, then combine into one.
  
  ## Bigram model
  tar_target(
      hist_text_val_split_bigram,
      split_ngram_function(preprocess_tokenise_bigram_premodel_val)
  ),
  
  ## Trigram model
  tar_target(
      hist_text_val_split_trigram,
      split_ngram_function(preprocess_tokenise_trigram_premodel_val)
  ),
  
  ## Quadgram model
  tar_target(
      hist_text_val_split_quadgram,
      split_ngram_function(preprocess_tokenise_quadgram_premodel_val)
  ),
  
  ## Fivegram model
  tar_target(
      hist_text_val_split_fivegram,
      split_ngram_function(preprocess_tokenise_fivegram_premodel_val)
  ),
  
  ## Sixgram model
  tar_target(
      hist_text_val_split_sixgram,
      split_ngram_function(preprocess_tokenise_sixgram_premodel_val)
  ),
  
  ## Combine all
  tar_target(
      hist_text_val_split,
      rbind(
          hist_text_val_split_bigram,
          hist_text_val_split_trigram,
          hist_text_val_split_quadgram,
          hist_text_val_split_fivegram,
          hist_text_val_split_sixgram
      )
  ),
  
  
  ## **************************
  ## ***************************************
  ## *************************************************
  ## All data version
  ## *************************************************
  ## ***************************************
  ## **************************

  ## *************************************************
  ## Cleaning
  ## *************************************************
  
  ## *************************************************
  ## Remove profanity
  
  ## Remove profanity - twitter
  tar_target(
      twitter_noprofan_all,
      remove_profanity_function(read_en_us_data_twitter)
  ),
  
  ## Remove profanity - blogs
  tar_target(
      blogs_noprofan_all,
      remove_profanity_function(read_en_us_data_blogs)
  ),
  
  
  ## Remove profanity - news
  tar_target(
      news_noprofan_all,
      remove_profanity_function(read_en_us_data_news)
  ),
  
  ## Combine above vectors into single dataframe (tibble)
  tar_target(
      combine_source_vectors_all,
      combine_source_vectors_function(vec_list = list(
          "Twitter" = twitter_noprofan_all,
          "Blogs" = blogs_noprofan_all,
          "News" = news_noprofan_all
      ))
      
  ),
  
  
  ## *************************************************
  ## Train/val/test split
  ## *************************************************
  
  ## Split combined dataset into train/val/test
  ## Returns list which needs to be subset
  
  tar_target(
      all_train_val_test_split,
      initial_validation_split(combine_source_vectors_all, prop = c(0.6, 0.2))
  ),
  
  
  ## *************************************************
  ## Tokenisation with stop words included
  
  
  ## Pre-processing and tokenisation into single words
  tar_target(
      preprocess_tokenise_premodel_all,
      preprocess_tokenise_premodel_function(rsample::training(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into bigrams
  tar_target(
      preprocess_tokenise_bigram_premodel_all,
      preprocess_bigram_premodel_function(rsample::training(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into trigrams
  tar_target(
      preprocess_tokenise_trigram_premodel_all,
      preprocess_trigram_premodel_function(rsample::training(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into quadgrams
  tar_target(
      preprocess_tokenise_quadgram_premodel_all,
      preprocess_quadgram_premodel_function(rsample::training(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into fivegrams
  tar_target(
      preprocess_tokenise_fivegram_premodel_all,
      preprocess_fivegram_premodel_function(rsample::training(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into sixgrams
  tar_target(
      preprocess_tokenise_sixgram_premodel_all,
      preprocess_sixgram_premodel_function(rsample::training(all_train_val_test_split))
  ),


  ## *************************************************
  ## Create training ngrams
  ## *************************************************

  ## Unigram - Need to work on this
  tar_target(
      unigram_model_all,
      create_unigram_function(preprocess_tokenise_premodel_all)
  ),

  ## Bigram
  tar_target(
      bigram_model_all,
      create_ngram_function(preprocess_tokenise_bigram_premodel_all)
  ),

  ## Trigram
  tar_target(
      trigram_model_all,
      create_ngram_function(preprocess_tokenise_trigram_premodel_all)
  ),

  ## Quadgram
  tar_target(
      quadgram_model_all,
      create_ngram_function(preprocess_tokenise_quadgram_premodel_all)
  ),

  ## Fivegram
  tar_target(
      fivegram_model_all,
      create_ngram_function(preprocess_tokenise_fivegram_premodel_all)
  ),

  ## Sixgram
  tar_target(
      sixgram_model_all,
      create_ngram_function(preprocess_tokenise_sixgram_premodel_all)
  ),

  # ## Model list (exluding unigram)
  # tar_target(
  #     ngram_model_list_all,
  #     ngrams_into_list_function(
  #         unigram = unigram_model_all,
  #         bigram = bigram_model_all,
  #         trigram = trigram_model_all,
  #         quadgram = quadgram_model_all,
  #         fivegram = fivegram_model_all,
  #         sixgram = sixgram_model_all
  #     )
  # ),
  # 
  # ## Model list (backoff)
  # tar_target(
  #     ngram_model_backoff_list_all,
  #     ngrams_into_backoff_list_function(
  #         unigram = unigram_model_all,
  #         bigram = bigram_model_all,
  #         trigram = trigram_model_all,
  #         quadgram = quadgram_model_all,
  #         fivegram = fivegram_model_all,
  #         sixgram = sixgram_model_all
  #     )
  # ),


  ## *************************************************
  ## Create validation testing ngrams
  ## *************************************************


  ## *************************************************
  ## Validation testing version
  ## Tokenisation with stop words included

  ## unigrams
  ## not included yet.

  ## Pre-processing and tokenisation into bigrams
  tar_target(
      preprocess_tokenise_bigram_premodel_val_all,
      preprocess_bigram_premodel_function(rsample::validation(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into trigrams
  tar_target(
      preprocess_tokenise_trigram_premodel_val_all,
      preprocess_trigram_premodel_function(rsample::validation(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into quadgrams
  tar_target(
      preprocess_tokenise_quadgram_premodel_val_all,
      preprocess_quadgram_premodel_function(rsample::validation(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into fivegrams
  tar_target(
      preprocess_tokenise_fivegram_premodel_val_all,
      preprocess_fivegram_premodel_function(rsample::validation(all_train_val_test_split))
  ),

  ## Pre-processing and tokenisation into sixgrams
  tar_target(
      preprocess_tokenise_sixgram_premodel_val_all,
      preprocess_sixgram_premodel_function(rsample::validation(all_train_val_test_split))
  ),


  ## *************************************************
  ## Create validation testing history/text datasets
  ## *************************************************

  ## Essentially split each into 'history' and 'next_word'
  ## Do this for each ngram, then combine into one.

  ## Bigram model
  tar_target(
      hist_text_val_split_bigram_all,
      split_ngram_function(preprocess_tokenise_bigram_premodel_val_all)
  ),

  ## Trigram model
  tar_target(
      hist_text_val_split_trigram_all,
      split_ngram_function(preprocess_tokenise_trigram_premodel_val_all)
  ),

  ## Quadgram model
  tar_target(
      hist_text_val_split_quadgram_all,
      split_ngram_function(preprocess_tokenise_quadgram_premodel_val_all)
  ),

  ## Fivegram model
  tar_target(
      hist_text_val_split_fivegram_all,
      split_ngram_function(preprocess_tokenise_fivegram_premodel_val_all)
  ),

  ## Sixgram model
  tar_target(
      hist_text_val_split_sixgram_all,
      split_ngram_function(preprocess_tokenise_sixgram_premodel_val_all)
  ),

  ## Combine all
  tar_target(
      hist_text_val_split_all,
      rbind(
          hist_text_val_split_bigram_all,
          hist_text_val_split_trigram_all,
          hist_text_val_split_quadgram_all,
          hist_text_val_split_fivegram_all,
          hist_text_val_split_sixgram_all
      )
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
