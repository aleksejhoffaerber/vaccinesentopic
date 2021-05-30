# libraries

library(httr)
library(purrr)
library(dplyr)
library(stringr)
library(tsibble)
library(jsonlite)

# set-up and twitter credentials ------

Sys.setenv(BEARER_TOKEN = "AAAAAAAAAAAAAAAAAAAAANGsQAEAAAAAVqIajpHCsttpQXrLSUhzgoGBMxg%3D1rES3hXHKCfFy6HhGcZGGMiE3b8MFbkyVD00Q3XuKOj0B84DJQ")

bearer_token <- "AAAAAAAAAAAAAAAAAAAAANGsQAEAAAAAVqIajpHCsttpQXrLSUhzgoGBMxg%3D1rES3hXHKCfFy6HhGcZGGMiE3b8MFbkyVD00Q3XuKOj0B84DJQ"
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

api_key <- "Onr9QZ6CGZnE2CzGzzbCxyh61"
api_secret <- "FGOeYy9zgWdrFF1jRMGfcg28xtbljxY9M6VilpWum8B2aE1Way"

acc_token <- "1394959278528733190-J3JiyE7ZxRvrtzRO1tLLdoLgwUmlvn"
acc_secret <- "Pv9D5EXIUyxZQf5g03XIY5AwuFgT4cDM439x2nJxFJaw0"


# define data ranges -----

data_range <- tibble(datetime = seq(as.POSIXct("2021-05-30 17:59:01"), 
                                    as.POSIXct("2021-05-30 18:00:01"), 
                                    # as.POSIXct("2021-05-23 18:00:01"), 
                                    # as.POSIXct("2021-05-30 17:59:01")
                                    by = "1 min") %>% 
                       as.character(),
                     tw_datetime = "")

data_range <- data_range %>% mutate(tw_datetime = datetime %>% 
                                      str_replace(" ", "T") %>% 
                                      str_replace(":01$", ":01.000Z"))

# create download function

twitter_download <- function(x) {
  Sys.sleep(5) # FIXME: to 60 secs
  
  params = list(
    `query` = '(vaccine OR BioNTech OR AstraZeneca OR Pfizer) -is.retweet lang:en',
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
    as_tibble()
  
  
}

# execute download function 

full_exp <- map(1:nrow(data_range), ~twitter_download(.x)) %>% 
  reduce(bind_rows)


