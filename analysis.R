library(tidyverse)
library(ggplot2)
library(stringr)
library(stringi)
library(tidytext)
library(plotly)

alltweets <- read.csv("alltweets_senate_2018.csv", header=TRUE, stringsAsFactors=FALSE) 


# Merge in party affiliation, states and 2018 race outcomes

party_and_votes <- read.csv("party_and_votes.csv", 
                            header=TRUE, stringsAsFactors=FALSE) 

merged_w_votes <- merge(alltweets,party_and_votes,by="screen_name")


remove_ind <- merged_w_votes %>% filter(party != "I") 
remove_lib_and_ind <- remove_ind %>% filter(party != "L")

table(remove_lib_and_ind$party)


# Create histgram for D's and R's in lead-up to 2018 midterms 

ggplot(remove_lib_and_ind, aes(date, fill=party)) + 
  geom_histogram(stat = "count") +
  ylim(0, 500) +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  scale_fill_manual(values=c("#404f7c", "#c63b3b")) #,"#"999999


# Perform sentiment analysis

tweets_to_use <- alltweets %>%
  select(user_id, created_at, status_id, date, screen_name, text, source, hashtags, favorite_count, retweet_count, status_url, name, location, description, followers_count, friends_count)

tweets_to_use$posttext <- tweets_to_use$text # duplicate tweet text column

tokenized_tweets <- tweets_to_use %>%
  select(user_id,  created_at, screen_name, date, text, source, hashtags, status_id,favorite_count, retweet_count, status_url, name, location, description, followers_count, friends_count, posttext) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  group_by(word, user_id,  created_at, screen_name, date, source, hashtags, status_id, favorite_count, retweet_count, status_url, name, location, description, followers_count, friends_count, posttext) %>%
  tally() %>%
  arrange(desc(n))

glimpse(tokenized_tweets)

sentiments <- read.csv("https://raw.githubusercontent.com/aleszu/textanalysis-shiny/master/labMT2english.csv", sep="\t")
labMT <- sentiments %>%
  select(word, happs)

all_sentiment <- tokenized_tweets %>%  
  inner_join(labMT, by = "word") %>%
  group_by(posttext, user_id, created_at, screen_name, source, hashtags, status_id, favorite_count, retweet_count, status_url, name, location, description, followers_count, friends_count) %>% 
  summarize(sentiment = mean(happs)) %>% # calculate avg score of tweet
  arrange(desc(sentiment)) %>% # sort by avg score
  mutate("score" = sentiment-5.372) # calculate new column with sentiment with 0 as neutral

glimpse(all_sentiment)
table(all_sentiment$name)

#write.csv(all_sentiment, "senate_candidates_tweets_sentiment.csv")


# Merge scored tweets with 2018 votes data

party_and_votes <- read.csv("party_and_votes.csv", 
                            header=TRUE, stringsAsFactors=FALSE) 

merged_w_votes <- merge(all_sentiment,party_and_votes,by="screen_name")

glimpse(merged_w_votes)


# create numeric column with percent of vote

merged_w_votes$percent_of_vote <- as.numeric(sub("%","",merged_w_votes$X..of.vote)) #remove %
merged_w_votes %>% glimpse()

# Let's organize by date, too. 
merged_w_votes$date <- as.Date(merged_w_votes$created_at, format = "%m/%d/%y")
merged_w_votes %>% glimpse()

# Create pivot table for final scatterplot 

joined_final_df <- merged_w_votes %>%
  group_by(name, percent_of_vote, followers_count, party, state) %>% 
  summarise("avgscore" = mean(score)) %>%
  ungroup()
joined_final_df%>% glimpse()


# Create scatterplot

vote_vs_sent <- ggplot(joined_final_df, aes(y=percent_of_vote, x=avgscore, color=party, title = name, text = state)) + 
  geom_point(aes(size=followers_count)) + 
 scale_size(name="", range = c(1.5, 8)) +
#  geom_smooth(method="lm") + 
  scale_color_manual(values=c("#404f7c", "#999999", "#34a35c", "#c63b3b")) +
  ggtitle("") +
  xlab("Average sentiment of tweets")+
  ylab("Percent of vote in 2018 midterms")+
  theme_minimal()

ggplotly(vote_vs_sent, tooltip=c("title", "text"))


# push plotly to server

Sys.setenv("plotly_username"="storybench")
Sys.setenv("plotly_api_key"="XXXXXX")

api_create(vote_vs_sent, filename = "midterm-votes-vs-sentiment")

# https://plot.ly/~storybench/271/
