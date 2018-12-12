### Prep ###
# set wd
setwd('/Users/kgedney/Documents/georgetown/anly503/exam')

# install packages
library(ggplot2) 
library(ggthemes)
library('scales')

# load datasets
df <- read.csv(file='non_voters.csv', stringsAsFactors = FALSE, 
               check.names=FALSE)

df_turnout  <- read.csv(file='voter_turnout_age.csv', stringsAsFactors = FALSE, 
               check.names=FALSE)

df_turnout_demo <- read.csv(file='voter_turnout_demo.csv', stringsAsFactors = FALSE, 
                            check.names=FALSE)

df_turnout$age_factor <- as.factor(df_turnout$age)
df_turnout$pct_voted  <- as.numeric(as.character(df_turnout$pct_voted))


### Plot 1 ###
# subset on just totals
df_agg <- df[(df$demo_1 == 'TOTAL'),]
df_agg$demo_1 <- NULL
df_agg$demo_2 <- NULL
df_agg$total_count <- NULL

df_agg <- t(df_agg)
df_agg <- as.data.frame(df_agg)
df_agg$reason <- rownames(df_agg)
rownames(df_agg) <- c()
colnames(df_agg) <- c("percent", "reason")
df_agg$percent <- as.numeric(as.character(df_agg$percent)) # make numeric
df_agg$percent[df_agg$reason == "Bad weather conditions"] <- 0

# plot
ggplot(data=df_agg, aes(reorder(reason,percent), percent)) +
  geom_col(color='mediumpurple4') + 
  coord_flip() + 
  geom_text(aes(label = percent(percent/100)), size=3, color='mediumpurple4', hjust=-0.10) + 
  labs(title = "Reasons for Not Voting") + 
  ylab('Percent of Respondents') + 
  xlab('')

### Plot 2 ###

# subset on just totals
df_total <- df_turnout[(df_turnout$state == 'US'),]
df_total <- df_total[!(df_total$age_factor == 'Total'),]

ggplot(data=df_total, aes(x=age_factor, y=total_pop)) +
  geom_col() + 
  geom_col(data=df_total, aes(x=age_factor, y=total_vted, fill=age_factor)) + 
  geom_text(aes(label = percent((pct_voted)/100), vjust=-0.5, color=age_factor), size=3) + 

  labs(title = "Voter Turnout by Age",
       fill = 'Voted') + 
  ylab('Total Population (000)') + 
  xlab('Age Group') + 
  guides(fill=FALSE, color=FALSE)


### Plot 3 ###
# boxplot of turnout rates by state and by age
df_turnout_age <- df_turnout[!(df_turnout$age_factor == 'Total'),]
df_turnout_age <- df_turnout_age[!(df_turnout_age$age_factor == ''),]

ggplot(data = df_turnout_age, aes(x=age_factor, y=pct_voted)) +
  geom_boxplot(aes(fill=age_factor)) +
  geom_jitter(size=0.5) + 
  labs(title = "Voter Turnout Distribution by State and Age", 
      fill = 'Age Group') + 
  ylab('Percent Voted') + 
  xlab('Age Group')



### Plot 4: Has Subplots ### 

# list_states <- unique(df_turnout_age$state)
df_turnout_age <- df_turnout[!(df_turnout$age_factor == 'Total'),]
df_turnout_age <- df_turnout_age[!(df_turnout_age$state == 'US'),]
df_turnout_age <- df_turnout_age[!(df_turnout_age$state == 'US'),]

df_turnout_age <- df_turnout_age[!(df_turnout_age$age_factor == ''),]

# create region bin
west <- c('ALASKA','ARIZONA', 'CALIFORNIA', 'COLORADO','HAWAII', 'IDAHO','MONTANA',
          'NEVADA','NEW MEXICO','OREGON','UTAH','WASHINGTON','WYOMING')
midwest <- c('ILLINOIS', 'INDIANA', 'IOWA', 'KANSAS','MICHIGAN','MINNESOTA','MISSOURI',
             'NEBRASKA','NORTH DAKOTA','OHIO','OKLAHOMA','SOUTH DAKOTA','WISCONSIN')
northeast <- c('CONNECTICUT','MAINE','MASSACHUSETTS', 'NEW HAMPSHIRE', 'NEW JERSEY','NEW YORK',
               'PENNSYLVANIA','RHODE ISLAND','VERMONT')
south <- c('ALABAMA','ARKANSAS', 'DELAWARE', 'DISTRICT OF COLUMBIA','FLORIDA','GEORGIA', 'KENTUCKY',
           'LOUISIANA','MARYLAND','MISSISSIPPI','NORTH CAROLINA','SOUTH CAROLINA','TENNESSEE',
           'TEXAS','VIRGINIA','WEST VIRGINIA')

df_turnout_age$region[is.element(df_turnout_age$state, west)] <- 'West'
df_turnout_age$region[is.element(df_turnout_age$state, midwest)] <- 'Midwest'
df_turnout_age$region[is.element(df_turnout_age$state, northeast)] <- 'Northeast'
df_turnout_age$region[is.element(df_turnout_age$state, south)] <- 'South'

df_turnout_age$region <- as.factor(df_turnout_age$region)

ggplot(data=df_turnout_age) +
  geom_point(mapping=aes(x = age_factor, y = pct_voted, color=age_factor)) +
  geom_smooth(mapping=aes(x = age_factor, y = pct_voted)) +
  facet_wrap(~ region, nrow=2) +
  labs(title = "Voter Turnout by Region and Age",
       color = 'Age Group') + 
  ylab('Percent Voted') + 
  xlab('Age Group') + 
  theme(axis.text.x=element_blank())
       


