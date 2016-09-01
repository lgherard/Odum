#############################################
# Creating graphs of all the predictions    #
# 5 year support + number of tests per year #
#############################################

# Load necessary files and libraries
source("code/dataprep.R")
library(reshape2)
library(plyr)
library(ggplot2)
library(gridExtra)

# Determines 5 year proportion of support vs refute vs ambiguous
# and number of papers published in each interval
ontogeny <- function(hyp){
  # Select analyzed cases only
  ont <- odum[odum[,(hyp+29)] != "0", ]
  ont$out <- ont[,(hyp+29)]
  stat <- ddply(ont, .(PY_5), summarize,
                S = length(out[out == "S"]),
                R = length(out[out == "R"]),
                U = length(out[out == "U"]),
                n = length(out[out == "S" | out == "R" | out == "U"]))
  stat$S.prop <- stat$S/stat$n
  stat$R.prop <- stat$R/stat$n
  stat$U.prop <- stat$U/stat$n
  melt <- melt(stat[,c(1,5:8)], id.var=c("PY_5", "n"))
  melt } #close function

# Creat a figure for each prediction
for (i in 1:24){
  data <- ontogeny(i)
  pred.no <- paste("Prediction", i, sep = " ")
  graph.papers <- ggplot(data, aes(x=PY_5, y=n)) +
    geom_bar(stat="identity") +
    theme_classic(10) +
    scale_y_continuous(expand = c(0,0) ) + 
    xlab("Publication Year (5-yr)") +
    theme(axis.line.x=element_line(),
          axis.line.y=element_line())
  graph.prop <- ggplot(data, aes(x=PY_5, y=value)) +
    geom_area(aes(fill=variable)) +
    scale_fill_manual(values=c("black", "white", "grey"),
                      name=pred.no,
                      breaks=c("R.prop", "U.prop", "S.prop"),
                      labels=c("Refute", "Ambiguous", "Support")) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    theme_classic(10) +
    xlab("Publication Year") +
    ylab("Proportion") +
    theme(legend.key=element_rect(color="#EEEEEE"),
          axis.line.x=element_line(),
          axis.line.y=element_line())
    ab <- grid.arrange(graph.papers, graph.prop, nrow = 1, widths=c(1,1.75))
  ggsave(ab, filename = paste("figures/papers_", i, ".pdf"), height=2, width=5, units="in")
}
