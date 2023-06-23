# twitter data analysis

library(tidyverse)
library(rtweet)
library(httpuv)
library(bbplot)

# authenticate with your Twitter API account
app_name <- "twitter"
consumer_key <- "#########"
consumer_secret <- "#########"
access_token <- "#########"
access_secret <- "#########"

# create an authentication object
twitter_token <- create_token(
  app = app_name,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)

# search "sweater"
sweater.tweets <- search_tweets("sweater OR sweaters", n = 2000, include_rts = FALSE, lang = "en")

# search "jumper"
jumper.tweets <- search_tweets("jumper OR jumpers", n = 2000, include_rts = FALSE, lang = "en")

# search "pullover"
pullover.tweets <- search_tweets("pullover OR pullovers", n = 2000, include_rts = FALSE, lang = "en")

# combine tweets
combined.tweets <- rbind(sweater.tweets, jumper.tweets, pullover.tweets)

combined.tweets <- combined.tweets %>%
  mutate(text = tolower(text)) %>%
  mutate(type = case_when(
    str_detect(text, 'sweater') ~ 'sweater',
    str_detect(text, 'jumper') ~ 'jumper',
    str_detect(text, 'pullover') ~ 'pullover'
  ))

# UK geocoded tweets
combined.tweets <- lat_lng(combined.tweets)

combined.tweets %>%
  filter(is.na(lat) & is.na(lng)) %>%
  nrow()

combined.tweets.uk <- combined.tweets %>%
  filter(lat > 49.82 & lat < 59.47 & lng > -10.85 & lng < 2.02)

nrow(combined.tweets.uk)

# map plot
library(maps)
world <- map_data("world")

uk <- world %>%
  filter(region == "UK")

uk %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group)) +
  geom_point(data = combined.tweets.uk, aes(x = lng, y = lat, colour = type)) +
  theme_void()
