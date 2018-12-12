# make plotly scatterplot
# register: https://plot.ly/r/
library(plotly)
library(stringr)

# set wd
setwd('/Users/kgedney/Documents/georgetown/anly503/exam')

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
df_results$winner <- ifelse(df_results$points_diff > 0, "Trump", "Clinton")

# merge datasets
df_turnout <- merge(df_turnout, df_results, by=c("state"))

# set colors
pal <- c("#2166AC", "#B2182B")
pal <- setNames(pal, c("Clinton", "Trump"))

# plot
# p <- plot_ly(x = df_turnout$total_pop, y = as.integer(df_turnout$pct_voted),
#              color  = as.factor(df_turnout$winner), 
#              colors = pal,
#              text   = paste0("State: ", df_turnout$state, 
#                              '<br>Point Diff: ', round((abs(df_turnout$points_diff) * 100),0)),
#              size  = abs(df_turnout$points_diff)) %>%
#   
#   layout(title = 'Voter Turnout by State',
#          xaxis = list(zeroline = FALSE, title='State Population (000)'),
#          yaxis = list(zeroline = FALSE, title='Voter Turnout (%)'))
# 
# p


p2 <- plot_ly(x = abs(df_turnout$points_diff)*100, y = as.integer(df_turnout$pct_voted),
             color  = as.factor(df_turnout$winner), 
             colors = pal,
             text   = paste0("State: ", df_turnout$state, 
                             '<br>Point Diff: ', round((abs(df_turnout$points_diff) * 100),0),
                             '<br>Turnout %: ', round(as.integer(df_turnout$pct_voted),2)),
             size  = df_turnout$total_pop) %>%
  
  layout(title = 'Voter Turnout by State',
         xaxis = list(zeroline = FALSE, title='Margin of Victory (pts)'),
         yaxis = list(zeroline = FALSE, title='Voter Turnout (%)'))

p2

library(htmlwidgets)
saveWidget(as_widget(p2), file="voter_turnout_plotly.html")

