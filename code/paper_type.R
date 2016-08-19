###########################
# Paper types citing Odum #
###########################

# Source data import and cleaning file
source("code/dataprep.R")

# Load necessary programs
library(plyr)
library(ggplot2)

# Paper Type #
table(odum$Type.paper)
table(odum$Type.p)
plot(odum$Type.p)
papers <- ddply(odum, .(Type.p), summarize,
                count = length(Type.p))
papers$Type.p <- as.character(papers$Type.p)

# to make a tree map
library(portfolio)
map.market(id=papers$Type.p, area = papers$count, group=papers$count,
           color=c(1,2,3,4))
library(treemap)
treemap(papers, index="Type.p", vSize="count")
# pie chart (above not working well)
library("RColorBrewer")
color= brewer.pal(4,"Set3")
pie(papers$count, labels=papers$Type.p, 
    col=color)
