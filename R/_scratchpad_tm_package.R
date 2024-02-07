## Scratchpad for working.
## Ignored by command sourcing functions from _targets.R


## Look at raw data
data <- tar_read(read_en_us_data_blogs)



## Just twitter.
## Bring in so that each line is document?
TC <- VCorpus(DirSource(directory = here::here("data-raw", "en_US"),
                       pattern = "*twitter.txt"))

length(TC)
class(TC)
summary(TC)



## sample only
## Bring in so that each line is document?
TCsmall <- VCorpus(DirSource(directory = here::here("data-raw", "en_US"),
                        pattern = "*sample.txt"))

## Look at small sample
TCsmall[[1]]$content



## DCM?
TC_TDM <- tm::TermDocumentMatrix(TCsmall[[1]]$content)


## view it?
TC_TDM
## Looks better, captures number of documents. and other summary info.

## inspect
inspect(TCsmall[1])
## this is also better. Allows to see matrix.
## Lots of stop words to take out... But do we need them, in order to predict next word...?
## Maybe actually need tri-grams or n-grams that allow prediction of sequence...
inspect(TC_TDM)


## Try importing as corpus
newCorpus <- Corpus(DirSource(directory = here::here("data-raw", "en_US"),
                              pattern = "*.txt"
                              ))
## class of newCorpus
class(newCorpus)
length(newCorpus)

## take twitter out of list?
twitterCorpus <- newCorpus[["en_US.twitter.txt"]]
length(twitterCorpus)

## Trying to get a feel for the data, without viewing and making it crash...
class(twitterCorpus)

TC <- Corpus(twitterCorpus)

summary(twitterCorpus)
dim(twitterCorpus)
meta(twitterCorpus)



