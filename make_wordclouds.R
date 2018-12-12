# set wd
setwd('/Users/kgedney/Documents/georgetown/anly503/exam')

# prep
library(wordcloud)
library(stringr)
library(viridis)


# load turnout data
df_turnout  <- read.csv(file='voter_turnout_age.csv', stringsAsFactors = FALSE, 
                        check.names=FALSE)
df_turnout <- df_turnout[(df_turnout$age == 'Total'),]
df_turnout <- df_turnout[!(df_turnout$state == 'US'),]
df_turnout$state <- str_to_title(df_turnout$state)

# load results data
df_results <- read.csv(file='2016-president-by-candidate.csv', stringsAsFactors = FALSE,
                       check.names = FALSE)
df_results$points_diff <- df_results$trump_pct - df_results$clinton_pct


# plot wordcloud with turnout data
vote_freq <- as.numeric(df_turnout$pct_voted)
vote_freq <- ((vote_freq - min(vote_freq)) / (max(vote_freq) - min(vote_freq)))
wordcloud_turnout <-wordcloud(words = df_turnout$state, 
                              freq  = vote_freq, 
                              min.freq = 0,
                              random.order = FALSE,
                              scale=c(1.0,0.6),
                              colors = viridis(30, direction=1))


margin_freq <- (1 - abs(df_results$points_diff)) ** 2
wordcloud_margin <- wordcloud(words = df_results$state, 
                              freq  = margin_freq, 
                              min.freq = 0,
                              random.order = FALSE,
                              scale=c(1.0,0.6),
                              colors = viridis(30, direction=1))



