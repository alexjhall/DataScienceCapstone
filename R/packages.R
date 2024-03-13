## Central list of packages to be loaded. 
## Can be referred to in _targets and markdown files.


## Set target-specific options such as packages.
library(tidyverse)
library(tibble)
library(wsvthemes); theme_set(theme_wsv())
library(scales)
library(targets)
library(tarchetypes)
library(lubridate)
library(here)
library(readxl)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(glue)
library(dbplyr) 
# library(DBI) 
# library(odbc) 
# library(keyring)
library(data.table)
library(quarto)
library(scales)
library(RColorBrewer)
# library(ivs)
library(janitor)
library(ggrepel)
library(pwr)
library(fuzzyjoin)

# text mining
library(tm)
library(quanteda)
library(tidytext)
library(readtext)
library(stopwords)
library(sentimentr) ## For removing profanity
library(stringr)
library(cld2) ## For detecting language
library(hunspell) ## Also for detecting language, more of spell check?

# modelling
library(tidymodels)

# markov chain
library(markovchain)

# For reversing list structure
library(paleotree)
# For emojis
library(emo)

# ## MRP (multilevel regression and poststratification)
# library(brms) # ?
# library(lme4) #glmer function
# library(broom) # Helps make the regression results tidier
# library(skimr) # Helps summarise the data
# #library(tidybayes) # Used to help understand the modelling estimates
# library(modelsummary) ## crosstab function
# 
# ## Logistic regression
# library(pscl) ## McFadden r-squared
# library(MASS) ## stepwise AIC
# 
# library(devtools)
# library(pkgbuild)
# 
# ## stan
# library(StanHeaders)
# library(rstan)
# library(rstanarm)
# library(bayesplot)
# 
# ## model testing
# library(performance)
# library(see)
# library(DHARMa)

