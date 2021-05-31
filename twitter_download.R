# libraries ----

library(httr) # parsing API information
library(purrr) # parallelization of loops
library(dplyr) # data wrangling & piping
library(stringr) # text operations
library(jsonlite) # JSON to data format transformation

# set-up environment and twitter credentials ------

Sys.setenv(BEARER_TOKEN = bearer_token)
source("twitter_credentials.R")

# define data ranges -----

data_range <- tibble(datetime = seq(as.POSIXct("2021-05-30 17:59:01"), # beggining
                                    as.POSIXct("2021-05-30 18:00:01"), # end
                                    # as.POSIXct("2021-05-23 18:00:01"), 
                                    # as.POSIXct("2021-05-30 17:59:01")
                                    by = "1 min") %>% # interval
                       as.character(),
                     tw_datetime = "") 

data_range <- data_range %>% mutate(tw_datetime = datetime %>% # create twitter specific datetime information
                                      str_replace(" ", "T") %>% 
                                      str_replace(":01$", ":01.000Z"))

# create download function

twitter_download <- function(x) {
  # sleep until executing agian
  Sys.sleep(5) # FIXME: to 60 secs
  
  params = list(
    `query` = '(vaccine OR BioNTech OR AstraZeneca OR Pfizer) -is.retweet lang:en', # TODO: search terms
    `max_results` = '30', # every 60 secs, 30 downloads 
    `tweet.fields` = 'created_at,lang,conversation_id,public_metrics', # metrics, such as retweets, likes etc.
    `start_time` = as.character(data_range[x,2]) # dynamic range depending on map
  )
  
  # show progress
  print(as.character(data_range[x,2]))
  
  # API-specific part 
  response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', 
                        httr::add_headers(.headers = headers), query = params)
  
  
  obj <- httr::content(response, as = "text")
  
  # create data frame from JSON export 
  fromJSON(obj, flatten = TRUE) %>% 
    as.data.frame() %>% 
    # select needed data fields / data columns
    select(data.text, data.lang, data.id,
           data.created_at, data.conversation_id, data.public_metrics.retweet_count,
           data.public_metrics.reply_count, data.public_metrics.like_count, data.public_metrics.quote_count, 
           meta.newest_id, meta.oldest_id, meta.result_count, meta.next_token) %>% 
    # transform into better data format (faster data wrangling, important for higher data sizes)
    as_tibble()
  
  
}

# execute download function -----

# intuition: map through all the rows in "data_range" (which contain the timedates)
# and execute the function above for all of them 

full_exp <- map(1:nrow(data_range), ~twitter_download(.x)) %>% 
  # reduce complexity of the results from individual tibbles into one big tibble
  reduce(bind_rows)


