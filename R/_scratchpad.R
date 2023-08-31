## Scratchpad for working.
## Ignored by command sourcing functions from _targets.R


## Look at raw data
data <- data.frame(tar_read(read_ec_data))



## Look at cleaned data
data <- data.frame(tar_read(clean_ec))

## Look at cleaned data
data <- data.frame(tar_read(data_audience_subset)[[1]])


## Look at cleaned control sheets date data
control_dates <- data.frame(tar_read(clean_control_dates))


## Look at merged data
data <- data.frame(tar_read(merge_derived))



## Look at raw data and location variable (HQ2)
testdata <- data.frame(tar_read(clean_ec))


## Look at raw data and location variable (HQ2) - look at demographics target
testdemo <- data.frame(tar_read(derived_demographics))


# ## 'DID' working: difference in difference
# library(foreign)
# mydata <- read.dta("http://dss.princeton.edu/training/Panel101.dta")




## *****************************************************************
## *****************************************************************
## Working on charts functions

## *****************************************************************
## Sample size chart.

## First create dataframe with variables for sample size and %diff possible.

## Set test parameters
SigLevel_alpha <- 0.05
ConfLevel <- 1 - SigLevel_alpha
PowerLevel <- 0.80
Direction <- "two.sided"

## Set proportion estimates
## Create reference proportion (p1)
RefProp <- 0.70

PctChange <- 
  seq(0.01, 0.2, 0.01)



## Create vector   
y <- c()

## Set proportion 1 (reference from input),
## proportion 2 and effect sizes
Prop1 <- rep(RefProp, length(PctChange))
Prop2 <- Prop1 + PctChange
EffSize <- ES.h(p1 = Prop1, p2 = Prop2)


## For loop
for (i in seq_along(PctChange)){
  
  
  ## Conduct test    
  x <-        
    pwr.2p.test(
      h = EffSize[i],
      #n = SampleSize,
      sig.level = SigLevel_alpha,
      power = PowerLevel,
      alternative = Direction
    )$n
  
  ## Accumulate results from loop
  y <- c(y, x)
}

y <- round(y, 0)


## Output table to make results easier to read.
OutputTable <- data.frame(
  "Reference_proportion" = Prop1,
  "Observed_proportion" = Prop2,
  "Difference" = PctChange,
  "Effect_size" = round(EffSize, 3),
  "Sample_required" = round(y, 0),
  "label" = c(paste("n=",round(y[1], 0)), rep("", 18), paste("n=",round(y[20], 0)))
)
    
      
## Then create chart

OutputTable %>%
  ggplot(aes(x = Difference, y = Sample_required, label = label)) + 
  geom_point() + 
  geom_text_repel(nudge_x = 0.01) +
  scale_y_log10() +
  # scale_x_discrete(limits=rev) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(from = 0, to = 0.2, by = 0.01)
                     )+ 
  # scale limited
  # scale_y_continuous(labels = scales::percent, 
  #                breaks = seq(from = 0.4, to = 0.8, by = 0.1),
  #                limits = c(0,1)) + 
  labs(
    caption = "Sample sizes required to detect statistically significant differences (p < 0.05) in proportion z-tests\nTwo sided z-test with power of 0.8, alpha of 0.05 and reference proportion of 0.7",
    x = "Percentage difference from example pre-launch survey result of 70%",
    y = "Sample size required in each pre and post-launch period",
    fill = NULL
  ) 








## *****************************************************************
## First chart for aq3_22 pre and post

data <- targets::tar_read(name = "data_aw_b3_subset", store = here("_targets"))[[2]]



## Raw data output
rawagg_aw_b3_prepost_AQ3_22 <- 
  data %>%
  group_by(prepost_period) %>%
  summarise(
    prop = sum(AQ3_22_positive_weighted) / sum(AQ3_22_answered_weighted),
    base = sum(AQ3_22_answered),
    SE = 1.96*sqrt(prop*(1-(prop))/base),
    upper = prop + SE,
    lower = prop - SE,
    Source = "Standard weighting",
    basetext = paste0("n = ",base)
  ) %>%
  dplyr::select(-SE, -base) %>%
  ungroup()



## Make prepost_period into factor, so that 'pre' appears first.
rawagg_aw_b3_prepost_AQ3_22$prepost_period <-
  factor(rawagg_aw_b3_prepost_AQ3_22$prepost_period,
         levels = c("Pre-launch", "Post-launch"))



## Point chart
# chart_prelaunch_only <-
  rawagg_aw_b3_prepost_AQ3_22 %>%
  ggplot(aes(x = prepost_period, y = prop)) +
  geom_point(size = 4, colour = main_gold) + 
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = .25,
                colour = main_gold) +
  
  # scale_colour_manual(values = c(main_gold, light_purple)) +
  
  geom_text(
    aes(
      # x = Gender,
      y = .5,
      label = basetext
    ),
    nudge_x = 0.25,
    # nudge_y = -.15,
    colour = "black",
    show.legend = FALSE
  ) +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(labels = scales::percent) + 
  # scale limited
  # scale_y_continuous(labels = scales::percent, 
  #                breaks = seq(from = 0.4, to = 0.8, by = 0.1),
  #                limits = c(0,1)) + 
  coord_flip() +
  labs(
    caption = "All workers, Manslaughter burst 2 (July 2020)\nAQ3_22: % agree (There is a high likelihood of unsafe workplaces being caught)",
    x = "",
    y = "",
    fill = NULL
  ) 















## *****************************************************************
## Looking at differences in SEs for raw vs models.

## Run main report code first, to generate tables etc.
## Look at this table: data_aw_agg_all_MRP 


## Generate summary table
testsummary <-
  data_aw_agg_all_MRP %>%
  dplyr::group_by(wave, Calculation) %>%
  dplyr::summarise(
    mean_lower = mean(Lower),
    mean_upper = mean(Upper)
  ) %>%
  dplyr::group_by(Calculation) %>%
  dplyr::summarise(
    mean_lower = mean(mean_lower),
    mean_upper = mean(mean_upper)
  ) %>%
  dplyr::mutate(
    diff = mean_upper - mean_lower
  )

## v2 calc diff at start
testsummaryv2 <-
  data_aw_agg_all_MRP %>%
  dplyr::mutate(diff = Upper - Lower) %>%
  dplyr::group_by(wave, Calculation) %>%
  dplyr::summarise(
    mean_lower = mean(Lower),
    mean_upper = mean(Upper),
    mean_diff = mean(diff)
  ) %>%
  dplyr::group_by(Calculation) %>%
  dplyr::summarise(
    mean_lower = mean(mean_lower),
    mean_upper = mean(mean_upper),
    mean_diff = mean(mean_diff)
  )







## wave by wave predictions


##stan_wavebywave_pred_function <- function(model_list, abs_data, group=NULL){
  
  ## Load data
  model_list <- tar_read(model_aw_wavebywave_AQ3_22)
  abs_data <- data.frame(tar_read(abs_clean))
  
  
 
  ## extract unique wave values from each list.
  ## to later append to predictions dataframe.
  list_names <-
    sapply(
      model_list,
      '[[',
      c(11, 4)
    ) %>%
    lapply(
      .,
      unique
    ) %>%
    purrr::reduce(c)
    
    
  
  ## Empty list
  pred_list <- list()
  
  
  
  ## Build models in loop
  pred_list <- 
    lapply(
      model_list,
      function(x)
        tidybayes::add_epred_draws(object = x, newdata = abs_data) %>%
        rename(predict_stan_bydraw = .epred) %>%
        dplyr::mutate(predict_stan_bydraw_wt = predict_stan_bydraw * Prop_pop_total) %>%
        group_by(.draw) %>%
        dplyr::summarise(
          predict_stan = sum(predict_stan_bydraw_wt)) %>%
        dplyr::summarise(
          estimate = mean(predict_stan),
          lower = quantile(predict_stan, 0.025),
          upper = quantile(predict_stan, 0.975)
        ) 
    )
        
  ## convert prediction list into dataframe
  pred_df <-
    do.call(rbind.data.frame, pred_list)
  
  
  
  ## Create output dataframe
  pred_output <-
    cbind(
      list_names, 
      pred_df
    )
  
  ## Rename list_names to "wave"
  pred_output <-
    pred_output %>%
    dplyr::rename(wave = list_names)
  
  
  
  ## Return dataframe
  return(pred_output)
  
  
  
  
  
  # ## First item
  # model1 <- model_list[[1]]
  # 
  # ## Test summarising with one model only.
  # model1pred <-
  #   model1 %>%
  #   tidybayes::add_epred_draws(newdata = abs_data) %>%
  #   rename(predict_stan_bydraw = .epred) %>%
  #   dplyr::mutate(predict_stan_bydraw_wt = predict_stan_bydraw * Prop_pop_total) %>%
  #   group_by(.draw) %>%
  #   dplyr::summarise(
  #     predict_stan = sum(predict_stan_bydraw_wt)) %>%
  #   dplyr::summarise(
  #     estimate = mean(predict_stan),
  #     lower = quantile(predict_stan, 0.025),
  #     upper = quantile(predict_stan, 0.975)
  #   )
  
  
  
  
  
  
  
  






















## Look at wave-by-wave stuff.

wavebywave <- list(tar_read(model_aw_wavebywave_AQ3_22))


wavebywave1 <- wavebywave[[1]]


## data, age...
data <- data.frame(tar_read(data_aw_indfull)) 

table(data$wave, data$Age, useNA = "always")

table(data$wave, data$AQ3_22_positive, useNA = "always")






## tar_read
data <- data.frame(tar_read(data_aw_indfull))

## response variable
response_variable <- "AQ3_22_positive"

## First filter data for testing
data <-
  data %>%
  dplyr::filter(wave < ymd("2020-01-01"))


## Create wave index, for loop
LoopIndex <- unique(data$wave)

## Split data up into list of dataframes. As many as in LoopIndex
DFlist <- 
  data %>%
  dplyr::group_split(wave)


## Create empty list
ModelList <- list()

## Build models in loop
ModelList <- 
  lapply(
    DFlist,
    function(x)
      rstanarm::stan_glmer(
        eval(parse(text = response_variable)) ~
          (1|Age) +
          (1|Gender) +
          (1|Location) +
          (1|Industry),
        family = binomial(),
        data = data)
  )












## testing pred function:

# set up
model <- targets::tar_read(name = "model_aw_allwave_AQ3_22",
                           store = here("_targets"))
abs_data <- targets::tar_read(name = "abs_aw_allwave_AQ3_22",
                              store = here("_targets"))
group <- as.character("wave")



  ## Create vectors of variable names which are passed to group_by
  # long_groupby <- c("Age", "Gender", "Location", "Industry", eval(substitute(group)))
  # short_groupby <- eval(substitute(group))

  long_groupby <- c("wave", ".draw")
  
  ## prediction
  stan_abs_pred <- 
    model %>%
    tidybayes::add_epred_draws(newdata = abs_data) %>%
    rename(predict_stan_bydraw = .epred)
  
  
  ## Group IS provided
  stan_abs_pred_est <-
    stan_abs_pred %>%
    dplyr::mutate(predict_stan_bydraw_wt = predict_stan_bydraw * Prop_pop_total) %>%
    dplyr::group_by(wave, .draw) %>%
    dplyr::summarise(
      predict_stan = sum(predict_stan_bydraw_wt)) %>%
    dplyr::group_by(wave) %>%
    dplyr::summarise(
      estimate = mean(predict_stan),
      lower = quantile(predict_stan, 0.025),
      upper = quantile(predict_stan, 0.975)
    )
  
  
  
  








































## Look at abs tables after edits




abs_test <- targets::tar_read(name = "abs_aw_allwave_AQ3_22",
                             store = here("_targets"))


x <- targets::tar_read(name = "abs_clean",
                              store = here("_targets"))

## male only
y <- x[x$Gender == "Male", ]

## Check sums
sum(y$Prop_pop_Gender)




## Gender
x <- 
  x %>%
  dplyr::group_by(Gender) %>%
  dplyr::mutate(Count_Gender = sum(Count)) %>%
  dplyr::group_by(Gender, Location, Industry, Age) %>%
  dplyr::mutate(Prop_pop_Gender = sum(Count) / Count_Gender)



## Gender
x <- 
  x %>%
  dplyr::group_by(Gender) %>%
  dplyr::mutate(Count_Gender = sum(Count)) %>%
  ungroup() %>%
  dplyr::mutate(Prop_pop_Gender = Count / Count_Gender)







## testing abs rep function - with prepost

absdata <- targets::tar_read(name = "abs_clean",
                             store = here("_targets"))
dataset <- targets::tar_read(name = "data_audience_subset",
                             store = here("_targets"))[[1]] 
responsevar <- "AQ3_22_positive"
repvar <- as.character("prepost_period")



## data with responsevar
data <-
  dataset %>%
  dplyr::filter(!is.na(eval(parse(text = responsevar))))

## Find unique values of repvar
repvar_unique <- 
  data %>%
  dplyr::select(all_of(repvar)) %>%
  unique() %>%
  dplyr::pull() %>%
  as.character()

## Filter out NA
repvar_unique <- repvar_unique[!is.na(repvar_unique)]

## Repeat repvar_unique as many times as there are abs combinations
repvar_unique_rep <-
  sort(
    rep(repvar_unique, nrow(absdata))
  )


## Repeat abs data
abs_data_repvar <- 
  do.call("rbind", replicate(length(repvar_unique), absdata, simplify = FALSE))
  
  
  
  
# Append new data
abs_data_repvar <- 
  cbind(
    abs_data_repvar,
    repvar_unique_rep
  )


# Rename new variable
names(abs_data_repvar) <- 
  c(
    names(absdata),
    as.character(parse(text = repvar))
  )


## Return
return(abs_data_repvar)


























## testing pred function:

# set up
model <- targets::tar_read(name = "model_aw_allwave_AQ3_22",
                           store = here("_targets"))
abs_data <- targets::tar_read(name = "abs_aw_allwave_AQ3_22",
                              store = here("_targets"))
group <- as.character("wave")


## If statement for if group is provided or not.
if(is.null(group)){
  
  ## Group NOT provided
  stan_abs_pred <-
    model %>%
    tidybayes::add_epred_draws(newdata = abs_data) %>%
    rename(predict_stan_bydraw = .epred) %>%
    dplyr::mutate(predict_stan_bydraw_wt = predict_stan_bydraw * Prop_pop_total) %>%
    group_by(Age, Gender, Location, Industry) %>%
    summarise(
      mean = mean(predict_stan_bydraw_wt),
      lower = quantile(predict_stan_bydraw_wt, 0.025),
      upper = quantile(predict_stan_bydraw_wt, 0.975)
    ) 
  
} else {
  
  ## Create vectors of variable names which are passed to group_by
  long_groupby <- c("Age", "Gender", "Location", "Industry", eval(substitute(group)))
  short_groupby <- eval(substitute(group))
  
  ## Group IS provided
  stan_abs_pred <-
    model %>%
    tidybayes::add_epred_draws(newdata = abs_data) %>%
    rename(predict_stan_bydraw = .epred) %>%
    dplyr::mutate(predict_stan_bydraw_wt = predict_stan_bydraw * Prop_pop_total) %>%
    group_by_at(long_groupby) %>%
    summarise(
      mean = mean(predict_stan_bydraw_wt),
      lower = quantile(predict_stan_bydraw_wt, 0.025),
      upper = quantile(predict_stan_bydraw_wt, 0.975)
    ) %>%
    group_by_at(short_groupby) %>%
    summarise(
      estimate = sum(mean),
      lower = sum(lower),
      upper = sum(upper)
    )
}

















## testing abs rep function

absdata <- targets::tar_read(name = "abs_clean",
                             store = here("_targets"))
dataset <- targets::tar_read(name = "data_audience_subset",
                             store = here("_targets"))[[1]] 
responsevar <- "AQ3_22_positive"
repvar <- as.character("wave")


## data with responsevar
data <-
  dataset %>%
  dplyr::filter(!is.na(eval(parse(text = responsevar))))

## Find unique values of repvar
repvar_unique <- 
  data %>%
  dplyr::select(all_of(repvar)) %>%
  unique() %>%
  dplyr::pull()
  
## Repeat repvar_unique as many times as there are abs combinations
repvar_unique_rep <-
  sort(
    rep(repvar_unique, nrow(absdata))
  )

## Repeat abs data
abs_data_repvar <- 
  absdata %>%
  slice(rep(1:n(), length(repvar_unique)))

# Append wave data
abs_data_repvar <- 
  cbind(
    abs_data_repvar,
    repvar_unique_rep
  )


# Rename variable
names(abs_data_repvar)[7] <- as.character(parse(text = repvar))



























## Testing model functions.

## Burst 2 and 3 only
## With no missing industry values too
data_employers_b2_b3_indfull <- 
  targets::tar_read(name = "data_emp_b2_b3_subset",
                    store = here("_targets"))[[2]]


## Create function
test_glmer_function <- function(data, response_variable){
  
  ## Build model 
  test_glmer_model <- 
    glmer(
        eval(parse(text = response_variable)) ~
        (1|Age) +
        (1|Gender) +
        (1|Location) +
        (1|Industry),
      family = binomial(),
      data = data)
  
  
  ## Return fitted model
  return(test_glmer_model)
  
}



## Test
model <-
  test_glmer_function(data_employers_b2_b3_indfull, "AQ3_21_positive")
  

















## Testing joining abs to data.


joined_data <-
  dplyr::left_join(data_employers_b2_b3_indfull,
                   abs_data,
                   by = c("Gender", "Location", "Industry", "Age"))












## Testing tar_make error: 16th March
clean_ec <- data.frame(tar_read(clean_ec))
derived_audience <- data.frame(tar_read(derived_audience))
derived_measure <- data.frame(tar_read(derived_measure))
derived_period <- data.frame(tar_read(derived_period))
derived_demographics <- data.frame(tar_read(derived_demographics))
derived_recognition <- data.frame(tar_read(derived_recognition))

## Find unique variables in each
# Set to easier names
x1 <- clean_ec
x2 <- derived_audience
x3 <- derived_measure
x4 <- derived_period
x5 <- derived_demographics
x6 <- derived_recognition


# Find unique variables
x2diff <- setdiff(x = names(x2), y = names(x1))
x3diff <- setdiff(x = names(x3), y = names(x1))
x4diff <- setdiff(x = names(x4), y = names(x1))
x5diff <- setdiff(x = names(x5), y = names(x1))
x6diff <- setdiff(x = names(x6), y = names(x1))


# Filter each set to Unique_ID plus the difference
x2select <- select(x2, c("Unique_ID", all_of(x2diff)))
x3select <- select(x3, c("Unique_ID", all_of(x3diff)))
x4select <- select(x4, c("Unique_ID", all_of(x4diff)))
x5select <- select(x5, c("Unique_ID", all_of(x5diff)))
x6select <- select(x6, c("Unique_ID", all_of(x6diff)))


## Series of left joins
data <-
  x1 %>%
  left_join(x2select, by = "Unique_ID") %>%
  left_join(x3select, by = "Unique_ID") %>%
  left_join(x4select, by = "Unique_ID") %>%
  left_join(x5select, by = "Unique_ID") %>%
  left_join(x6select, by = "Unique_ID")










