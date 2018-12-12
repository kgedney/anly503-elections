#- Prep
# set wd
setwd('/Users/kgedney/Documents/georgetown/anly503/exam')

# install packages
library(dplyr)
library(igraph)
library(networkD3) 
library(reshape)

#- Data Prep
# load electoral college data
df_college <- read.csv('electoral_college.csv', stringsAsFactors = FALSE)
df_college <- df_college[c('state', 'clintonElectors', 'trumpElectors')] # drop column
colnames(df_college) <- c('state', 'Clinton Electors', 'Trump Electors') # rename columns

# melt dataset
df_college <- melt(df_college, id=c('state'))

df_college <- df_college[!(df_college$value == 0),] # drop zeros
df_college$variable <- as.character(df_college$variable) # set types
df_college$state   <- as.character(df_college$state)
df_college$value   <- as.numeric(df_college$value)


#- Set up for Sankey
colnames(df_college) <- c("source", "target", "weight")
(head(df_college))

# create node list
name_vec <- c(unique(df_college$source), unique(df_college$target))
nodes   <- data.frame(name = name_vec, id = 0:52)
nodes$name <- as.character(nodes$name)

# create edges
links <- df_college %>%
  left_join(nodes, by = c('source' = 'name')) 
# rename
links$origin_id <- links$id
# merge
links <- links %>% left_join(nodes, by = c('target' = 'name'))
# rename
links$dest_id <- links$id.y

# drop other columns
links <- links[c('source', 'target', 'weight', 'origin_id', 'dest_id')]

#- Set up Plot 
# Add a 'group' column to each connection
# (ref: https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram/)
groups_list = links$dest_id
links$group = as.factor(c(groups_list))

# make state nodes gray
nodes$group=as.factor(c("states"))

# assign color to each 
my_color <- 'd3.scaleOrdinal() .domain(["52", "51", "states"]) .range(["c96464", "4ca6ff","D3D3D3"])'

# plot sankey
sankeyNetwork <- sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
                               Value = 'weight', NodeID = 'name', fontSize = 12, fontFamily='Arial', colourScale=my_color,
                               LinkGroup='group', NodeGroup='group')

sankeyNetwork


#- Save sankey
saveNetwork(sankeyNetwork, "sankey_electoral_college.html", selfcontained = TRUE)







######## OLD VERSIONS ##########
# make network d3
# https://www.rdocumentation.org/packages/networkD3/versions/0.4/topics/simpleNetwork 
# http://personal.tcu.edu/kylewalker/interactive-flow-visualization-in-r.html 

# import data - state level 2016 presidential election results
df <- read.csv('2016-president.csv', stringsAsFactors = FALSE)

# add new column
df$candidate_pct <- (df$candidatevotes / df$totalvotes) *100

# set up data
# Data format: dataframe with 3 variables; variables 1 & 2 correspond to interactions; variable 3 is weight of interaction
df <- df[c('state', 'candidate', 'candidate_pct')]

colnames(df) <- c("source", "target", "weight")
(head(df))

# create node list
name_vec <- c(unique(df$source), unique(df$target))
nodes   <- data.frame(name = name_vec, id = 0:52)

# create edges
links <- df %>%
  left_join(nodes, by = c('source' = 'name')) %>%
  rename(origin_id = id) %>%
  left_join(nodes, by = c('target' = 'name')) %>%
  rename(dest_id = id)

# plot network
forceNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
             Value = 'weight', NodeID = 'name', Group = 'id', zoom = TRUE, height=500, width=500)


# plot sankey
# Add a 'group' column to each connection
# (ref: https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram/)
groups_list = links$dest_id
links$group = as.factor(c(groups_list))

# assign color to each 
my_color <- 'd3.scaleOrdinal() .domain(["51", "52", "group"]) .range(["firebrick", "0080FF"])'

# plot sankey
sankeyNetwork(Links = links, Nodes = nodes, Source = 'origin_id', Target = 'dest_id', 
              Value = 'weight', NodeID = 'name', fontSize = 10, fontFamily='Arial', colourScale=my_color,
              LinkGroup='group')



###################




