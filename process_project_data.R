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

# num_commits_plot
num_commits_plot <- msg_time %>% group_by(repo, year) %>% summarize(count = n()) %>% 
  ggplot(aes(x = year, y=count, color=repo)) + 
  geom_point() +
  geom_line() +
  theme(legend.spacing.x  = unit(0.01, "cm")) +
  theme_ipsum() +
  theme(legend.text = element_text(size = 6, face = "bold"),
        legend.position = "top", 
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.2, "cm")) +
  ggtitle("Number of commits of most popular repos on Github over years")

# dow plot
dow_plot <- msg_time %>% group_by(repo, day_of_week) %>% 
  ggplot(aes(x = day_of_week, fill=repo)) + 
  geom_bar() +
  theme(legend.spacing.x  = unit(0.01, "cm")) +
  theme_ipsum() +
  theme(legend.text = element_text(size = 6, face = "bold"),
        legend.position = "top", 
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.2, "cm")) +
  ggtitle("Number of commits for each Day of Week")

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

msg_token_byrepo_percent <- msg_token_byrepo_relative %>% 
  mutate(percentage =  n.x / n.y) %>% arrange(desc(percentage))

# wordcloud plots
word_cloud1 <- msg_token_byrepo %>% top_n(n=10) %>% 
  ggplot(aes(label = tokens, size = n, color = repo)) +
  geom_text_wordcloud(area_corr = 0.3) + 
  theme_minimal() +
  ggtitle("Wordcloud of top occurring tokens across all repositories")

# webscraps
# PART 2: Getting data of own/selected repository
library(httr)
library(jsonlite)

# Set Github API endpoint URL
endpoint <- "https://api.github.com/users/WilliamQD/repos"

# Set Github personal access token
token <- "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"

# Make GET request to API endpoint
response <- GET(endpoint, add_headers("Authorization" = paste0("token ", token)))

# Parse JSON response
repos <- fromJSON(rawToChar(response$content))

# Initialize empty data frame to store commit messages
commit_df <- data.frame(repo = character(),
                        username = character(),
                        commit_author = character(),
                        message = character(),
                        stringsAsFactors = FALSE)

# Loop through each repo and retrieve commit messages
for (i in 1:length(repos)) {
  repo_name <- repos[i, ]$name 
  username <- repos[i, ]$owner$login
  commits_endpoint <- paste0("https://api.github.com/repos/", username, "/", repo_name, "/commits") # send another request for the commits
  commits_response <- GET(commits_endpoint, add_headers("Authorization" = paste0("token ", token)))
  commits <- fromJSON(rawToChar(commits_response$content))
  if (is.null(commits$message)){
    for (j in 1:length(commits)) {
      # Get message, author, and save them to the datafrmae
      commit_message <- commits[j, ]$commit$message
      author <- commits[j, ]$commit$author$name
      commit_df <- rbind(commit_df, data.frame(repo = repo_name, username = username, commit_author =
                                                 author, message = commit_message, 
                                               stringsAsFactors = FALSE))
    }
  }
}


commit_df_clean <- commit_df %>% 
  mutate(message = str_replace_all(message, "[^\x01-\x7F]+", "")) %>% 
  filter(!is.na(message)) %>% 
  filter(!grepl("^[[:space:]]*$", message))


commit_nlp <- commit_df_clean %>% mutate(
  message = gsub("[[:digit:]]", "", message)
)



commit_tokens <- commit_nlp %>% unnest_tokens(token = "words", input = message, 
                                              output = tokens, drop=TRUE, 
                                              to_lower = TRUE) %>% 
  filter(!tokens %in% stopwords("english")) %>% count(tokens, sort=TRUE)



word_cloud_mine <- commit_tokens %>% 
  ggplot(aes(label = tokens, size = n, color = tokens)) +
  geom_text_wordcloud(area_corr = 0.3) + 
  theme_minimal() +
  ggtitle("Wordcloud of my repos!")



# Generating wordcloud for Steven

# Set Github API endpoint URL
endpoint <- "https://api.github.com/users/Yuanxyyds/repos"

# Set Github personal access token
token <- "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"

# Make GET request to API endpoint
response <- GET(endpoint, add_headers("Authorization" = paste0("token ", token)))

# Parse JSON response
repos <- fromJSON(rawToChar(response$content))

# Initialize empty data frame to store commit messages
commit_df_stev <- data.frame(repo = character(),
                             username = character(),
                             commit_author = character(),
                             message = character(),
                             stringsAsFactors = FALSE)

# Loop through each repo and retrieve commit messages
for (i in 1:length(repos)) {
  repo_name <- repos[i, ]$name 
  username <- repos[i, ]$owner$login
  commits_endpoint <- paste0("https://api.github.com/repos/", username, "/", repo_name, "/commits") # send another request for the commits
  commits_response <- GET(commits_endpoint, add_headers("Authorization" = paste0("token ", token)))
  commits <- fromJSON(rawToChar(commits_response$content))
  if (is.null(commits$message)){
    for (j in 1:length(commits)) {
      # Get message, author, and save them to the datafrmae
      commit_message <- commits[j, ]$commit$message
      author <- commits[j, ]$commit$author$name
      commit_df_stev <- rbind(commit_df_stev, data.frame(repo = repo_name, 
                                                         username = username,
                                                         commit_author = author, 
                                                         message = commit_message,
                                                         stringsAsFactors = FALSE))
    }
  }
}


commit_stev_clean <- commit_df_stev %>% 
  mutate(message = str_replace_all(message, "[^\x01-\x7F]+", "")) %>% 
  filter(!is.na(message)) %>% 
  filter(!grepl("^[[:space:]]*$", message))

commit_stev_nlp <- commit_stev_clean %>% mutate(
  message = gsub("[[:digit:]]", "", message)
)

commit_stev_tokens <- commit_stev_nlp %>% unnest_tokens(token = "words", input = message, 
                                                        output = tokens, drop=TRUE, 
                                                        to_lower = TRUE) %>% 
  filter(!tokens %in% stopwords("english")) %>% count(tokens, sort=TRUE)

wordcloud_stev <- commit_stev_tokens %>% 
  ggplot(aes(label = tokens, size = n, color = tokens)) +
  geom_text_wordcloud(area_corr = 0.3) + 
  theme_minimal() +
  ggtitle("Wordcloud of Steven's repos!")


# Generating wordcloud for JSC370

# Set Github API endpoint URL
endpoint <- "https://api.github.com/users/JSC370/repos"

# Set Github personal access token
token <- "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"

# Make GET request to API endpoint
response <- GET(endpoint, add_headers("Authorization" = paste0("token ", token)))

# Parse JSON response
repos <- fromJSON(rawToChar(response$content))

# Initialize empty data frame to store commit messages
commit_df_370 <- data.frame(repo = character(),
                            username = character(),
                            commit_author = character(),
                            message = character(),
                            stringsAsFactors = FALSE)

# Loop through each repo and retrieve commit messages
for (i in 1:length(repos)) {
  repo_name <- repos[i, ]$name 
  username <- repos[i, ]$owner$login
  commits_endpoint <- paste0("https://api.github.com/repos/", username, "/", repo_name, "/commits") # send another request for the commits
  commits_response <- GET(commits_endpoint, add_headers("Authorization" = paste0("token ", token)))
  commits <- fromJSON(rawToChar(commits_response$content))
  if (is.null(commits$message)){
    for (j in 1:length(commits)) {
      # Get message, author, and save them to the datafrmae
      commit_message <- commits[j, ]$commit$message
      author <- commits[j, ]$commit$author$name
      commit_df_370 <- rbind(commit_df_370, data.frame(repo = repo_name, 
                                                       username = username,
                                                       commit_author = author, 
                                                       message = commit_message,
                                                       stringsAsFactors = FALSE))
    }
  }
}


commit_370_clean <- commit_df_370 %>% 
  mutate(message = str_replace_all(message, "[^\x01-\x7F]+", "")) %>% 
  filter(!is.na(message)) %>% 
  filter(!grepl("^[[:space:]]*$", message))

commit_370_nlp <- commit_370_clean %>% mutate(
  message = gsub("[[:digit:]]", "", message)
)

commit_370_tokens <- commit_370_nlp %>% unnest_tokens(token = "words", input = message, 
                                                      output = tokens, drop=TRUE, 
                                                      to_lower = TRUE) %>% 
  filter(!tokens %in% stopwords("english")) %>% count(tokens, sort=TRUE)

wordcloud_370 <- commit_370_tokens %>% 
  ggplot(aes(label = tokens, size = n, color = tokens)) +
  geom_text_wordcloud(area_corr = 0.3) + 
  theme_minimal() +
  ggtitle("Wordcloud of JSC370's repos!")
