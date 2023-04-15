library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggwordcloud)
library(lubridate)
library(stringr)
library(tidytext)
library(stopwords)
library(RColorBrewer)
library(wordcloud2)
library(gganimate)
library(DT)
library(gridExtra)
library(hrbrthemes)
library(extrafont)
loadfonts()

# reading and cleaning
com_msg <- read.csv("full.csv")
com_msg_small <- com_msg[seq(1, nrow(com_msg), by=10), ]
msg_clean <- com_msg_small %>% select(-c("commit", "author"))
msg_clean <- msg_clean %>% filter(msg_clean$message != "")

# for time series
msg_time <- msg_clean %>% mutate(
  date_withzone = as_datetime(substr(date, 1, nchar(date)-6), format = "%a %b %d %H:%M:%S %Y"),
  date = substr(date_withzone, 1, length(date_withzone)-4),
  hour = hour(date),
  day_of_week = wday(date, label = TRUE),
  month = month(date, label = TRUE),
  year = year(date)
) %>% select(-c("date_withzone"))

# for nlp
msg_nlp <- msg_clean %>% mutate(
  message = gsub("[[:digit:]]", "", message)
) %>% select(-c("date"))

msg_tokens <- msg_nlp %>% unnest_tokens(token = "words", input = message, 
                                        output = tokens, drop=TRUE, 
                                        to_lower = TRUE) %>% 
  filter(!tokens %in% stopwords("english")) %>% 
  count(tokens, sort=TRUE)

# wordcloud 
msg_token_byrepo <- msg_nlp %>% unnest_tokens(token = "words", input = message, 
                                              output = tokens, drop=TRUE, 
                                              to_lower = TRUE) %>% 
  filter(!tokens %in% stopwords("english")) %>% 
  group_by(repo) %>% 
  count(tokens, sort=TRUE)

msg_token_byrepo_count <- msg_nlp %>% unnest_tokens(token = "words", input = message, 
                                                    output = tokens, drop=TRUE, 
                                                    to_lower = TRUE) %>% 
  filter(!tokens %in% stopwords("english")) %>% 
  group_by(repo) %>% 
  count(repo, sort=TRUE)

msg_token_byrepo_relative <- merge(
  x = msg_token_byrepo,
  y = msg_token_byrepo_count,
  by.x = "repo",
  by.y = "repo",
  all.x = FALSE,
  all.y = FALSE
)

msg_token_byrepo_percent <- msg_token_byrepo_relative %>% mutate(percentage = 
                                                                   n.x / n.y) %>% arrange(desc(percentage))