# make shiny app

# set wd
setwd('/Users/kgedney/Documents/georgetown/anly503/exam')

# load data
df <- read.csv(file='non_voters.csv', stringsAsFactors = FALSE, 
                        check.names=FALSE)
# subset on just Age
df <- df[(df$demo_1 == 'Age'),]
df$demo_1      <- NULL
df$total_count <- NULL

# rearrange data
df <- t(df)
df <- as.data.frame(df)
df <- df[-c(1), ]
df$reason <- rownames(df)

colnames(df) <- c("18_to_24_years", "25_to_44_years", "45_to_64 years", "65_years_and over", "reason")
rownames(df) <- c()

df[df=="-"] <- NA

# save file
write.csv(df, file = "shiny/data_for_shiny.csv")

