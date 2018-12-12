
# load libraries
library(datasets)
library(threejs)
library(htmlwidgets)

# set wd
setwd('/Users/kgedney/Documents/georgetown/anly503/exam')

# load data
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


# plot
scatter_3d <- scatterplot3js(round(df_turnout$total_pop, -1), round(df_turnout$total_citizen_pop, -1), 
                       as.numeric(df_turnout$pct_voted),
                       color = c("#2166AC", "#B2182B")[as.factor(df_turnout$winner)],
                       axisLabels=c("State Population (000)",
                                    "Voter Turnout (%)", 
                                    "State Citizen Population (000)"),
                       labels = paste0("State: ", df_turnout$state),
                       size = 0.5)

scatter_3d

saveWidget(scatter_3d, file="scatter_3d.html", selfcontained = TRUE, libdir =
             NULL, background = "white")


