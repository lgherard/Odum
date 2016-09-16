############################
# Analysis of Odum data    #
# Mosais chart             #
############################

# Source data import and cleaning file
source("code/dataprep.R")

table(odum$Type.p)

# As a paper could have multiple outcomes, they were ranked as: first, test;
#                                                               then, accept as fact;
#                                                               then, mention;
#                                                               then, no mention.
i=1247
i=470
paper.test <- matrix(ncol=7, nrow=nrow(odum))
for (i in 1:nrow(odum)){
  paper.test[i,1] <- as.numeric(odum$No.paper[i])
  paper.test[i,2] <- as.character(odum$Type.p[i])
  a <- as.numeric(odum[i,6:29])
  paper.test[i,3] <- sum(a == 0)
  paper.test[i,4] <- sum(a == 1)
  paper.test[i,5] <- sum(a == 2)
  paper.test[i,6] <- sum(a == 3)
  paper.test[i,7] <- sum(a == 4)
  #  a <- table(odum[,i])
#  hyp.men[i,] <- as.numeric(a)
} 
paper.test <- as.data.frame(paper.test)
names(paper.test) <- c("No.paper", "Type", "NoMention", "Mention", "DataOnly", "Test", "Fact")
for (i in 3:7){
  paper.test[,i] <- as.numeric(as.character(paper.test[,i]))
}
head(paper.test)

# Code for highest ranked treatment
paper.test$Treatment <- ifelse(paper.test$Test > 0, "Test",
                           ifelse(paper.test$Fact > 0, "Fact",
                                  ifelse(paper.test$Mention > 0, "Mention",
                                         ifelse(paper.test$DataOnly > 0, "Data Only", 
                                                ifelse(paper.test$NoMention > 0, "No Mention", 
                                                "err")))))

# Sort data for better graphing
paper.test <- transform(paper.test, 
                        Treatment = factor(Treatment, levels=c("No Mention", "Data Only", "Fact", "Mention", "Test")),
                        Type = factor(Type, levels=c("D", "R", "M", "A"),
                                      labels=c("Data", "Review", "Modeling", "Meta-Analysis")))

mycolors <- c("#9BDFF4", "#524C8A", "#7570B3", "#3C3E35")
pdf(file="figures/papertype_v_outcome.pdf", width=7, height=6)
mosaicplot(table(paper.test$Treatment,paper.test$Type), col=mycolors, 
           main="", cex.axis = 0.5, las=1, off=c(2,0), border=NA)
dev.off()

table(paper.test$Treatment)

table(paper.test$Type)

table(paper.test$Treatment,paper.test$Type)

# papers mentioning predictions
mentions <- c("Fact", "Mention", "Test")
sum(paper.test$Treatment %in% mentions)
sum(paper.test$Treatment %in% mentions)/1598

table(paper.test$Treatment,paper.test$Type)
