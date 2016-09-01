############################
# Analysis of Odum data    #
# ESA conference 2015      #
# Hypothesis testing       #
# Code updates 160728 for  #
# manuscript               #
############################

# Source data import and cleaning file
source("code/dataprep.R")

# Load necessary programs
library(ggplot2)
library(plyr)
library(reshape2)

#################################
# Are any hypotheses addressed? #
#################################

# This function calculates the mean of all values greater that zero
meanfun <- function(x) {
  meanprop <- mean(x > 0)
  return(meanprop)
}

rowmean <- apply(odum[,6:29], 1, meanfun)
b <- as.numeric(rowmean > 0) # gives logical yes/no, then coerced to numeric
sum(b)
# to determine how many were tested
# c <- rowmean*length(odum[1,6:29])
# sum(c)

# How many papers tested a hypothesis?
hyp.test <- function(x) {
  meanprop <- mean(x == 3)
  return(meanprop)
}
rowmean2 <- apply(odum[,6:29], 1, hyp.test)
b2 <- as.numeric(rowmean2 > 0)
sum(b2) # numbers of papers that tested a hypothesis

# write.csv(odum, "odum_practice.csv")

############################################################
# Which predictions are mentioned, testedor stated as fact #
############################################################
library(reshape2)
pred <- odum[,6:29]

hyp.men <- matrix(ncol=5, nrow=24)
for (i in 1:24){
  a <- table(pred[,i])
  hyp.men[i,] <- as.numeric(a)
} 
hyp.men <- as.data.frame(hyp.men)

#Recode for treatment on prediction
names(hyp.men) <- c("no_mention", "mention_small", "data_notest", "test", "as_fact") 
hyp.men$pred.no <- seq(1:24)

# Combine mention + data_notest into single factor
hyp.men$mention <- hyp.men$mention_small + hyp.men$data_notest

out_mentions <- subset(hyp.men, select= -c(no_mention, mention_small, data_notest))

# Plot of each prediction with proportion of time tested versus (mention + as fact)
# Generate proportional data
hyp.prop <- out_mentions
hyp.prop$total <- hyp.prop$test + hyp.prop$as_fact + hyp.prop$mention
hyp.prop$test.prop <- 100*(hyp.prop$test/hyp.prop$total)
hyp.prop$no_test.prop <- 100*(hyp.prop$as_fact + hyp.prop$mention)/hyp.prop$total

hyp.prop <- subset(hyp.prop, select = c(test.prop, no_test.prop, pred.no))

hdata <- melt(hyp.prop, id = "pred.no")
hdata$type <- ifelse(hyp.prop$pred.no < 6, c("Community Energetics"),
                     ifelse(hyp.prop$pred.no < 12, c("Community Structure"),
                            ifelse(hyp.prop$pred.no < 15,  c("Life History"),
                                   ifelse(hyp.prop$pred.no < 18, c("Nutrient Cycling"),
                                          ifelse(hyp.prop$pred.no < 20, c("Selection Pressure"),
                                                 c("Overall Homeostasis"))))))

pred.cat <- c("Community Energetics", "Community Structure", "Life History", "Nutrient Cycling", "Selection Pressure", "Overall Homeostasis")

# Plotting - proportional of tests v non-tests per prediction
plot.prop <- ggplot(hdata, aes(x=factor(pred.no), y=value)) +
  geom_bar(stat="identity", aes(fill=type)) +
  scale_fill_brewer(palette="Dark2",
                    name="Category",
                      breaks=pred.cat) +
  geom_bar(stat="identity", fill="black", aes(alpha=variable)) +
  scale_alpha_manual(values=c(0.9,0),
                     name="Recognition", 
                     breaks=c("no_test.prop", "test.prop"),      # can use to set order
                     labels=c("Mentioned + As Fact", "Tested")) +  # Relabel
  theme_classic(11) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("Prediction #") +
  ylab("Percent (%)")
plot.prop + theme(legend.key=element_rect(color="#EEEEEE"))
ggsave("figures/Pred_test_proportional.jpeg", width = 6.5, height = 3.09, units = "in")


# Prep for hypothesis prediction test count figure

out.long <- melt(out_mentions, id="pred.no")
# Group hypotheses
out.long$type <- ifelse(out.long$pred.no < 6, c("Community Energetics"),
                        ifelse(out.long$pred.no < 12, c("Community Structure"),
                               ifelse(out.long$pred.no < 15,  c("Life History"),
                                      ifelse(out.long$pred.no < 18, c("Nutrient Cycling"),
                                             ifelse(out.long$pred.no < 20, c("Selection Pressure"),
                                                    c("Overall Homeostasis"))))))
out.long$var.no <- ifelse(out.long$variable == "mention", 4,
                          ifelse(out.long$variable == "as_fact", 3,
                                 ifelse(out.long$variable == "test", 2,
                          1)))

newdata <- out.long[order(out.long$pred.no, out.long$var.no),]
pred.cat <- c("Community Energetics", "Community Structure", "Life History", "Nutrient Cycling", "Selection Pressure", "Overall Homeostasis")

# Plot of predictions and times mentioned, etc

plot.2 <- ggplot(newdata, aes(x=factor(pred.no), y=value)) +
  geom_bar(stat="identity", aes(fill=type)) +
  scale_fill_brewer(palette="Dark2",
                    name="Category",
                      breaks=pred.cat) +
  geom_bar(stat="identity", fill="black", aes(alpha=variable)) +
  scale_alpha_manual(values=c(0.9,0.4,0),
                     name="Recognition", 
                     breaks=c("mention", "as_fact", "test"),      # can use to set order
                     labels=c("Mention", "As Fact", "Test")) +  # Relabel
  theme_classic(11) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("Prediction #") +
  ylab("Count")
plot.2 + theme(legend.key=element_rect(color="#EEEEEE"))
ggsave("figures/Pred_test_total.jpeg", width = 6.5, height = 3.09, units = "in")


#################################################
# If hypothesis is tested, what is the outcome? #
#################################################

# Create subsetted dataset of papers that tested a hypothesis
odum.tested <- odum[b2==1,]

# Create a list of every paper, if it tested a hypothesis, 
# what is the outcome?

find3 <- function(data) {
  v1 <- data[grep("T[.]", colnames(odum))]
  v2 <- data[grep("O[.]", colnames(odum))]
  position <- which(v1 == 3) # pulls out position of "true" statements
  outcome <- v2[position]
  return(outcome)
}

d <- apply(odum, 1, find3)
dd <- tapply(unlist(d), unlist(d), length) # sums across all hypotheses
sum(dd) # total number of times hypotheses were tested (each paper can test more than once)

out <- aggregate(unlist(d), by = list(names(unlist(d)), unlist(d)), FUN = length)
sum(out$x)

vecpick <- function(vec, n){
  pick <- vec[n]
  return(pick)
}

out.wide <- dcast(out, Group.1 ~ Group.2, value.var = "x")
out.wide$pred <- lapply(strsplit(out.wide$Group.1, split = "[.]"), vecpick, n =2)
out.wide$pred <- as.numeric(out.wide$pred)
head(out.wide)
str(out.wide)
# Replace NA with 0
out.wide[is.na(out.wide)] <- 0

# If S/R, add count to each "S" and "R"
out.wide$S.all <- out.wide$S + out.wide[,5]
out.wide$R.all <- out.wide$R + out.wide[,5]

# How many times was a hypothesis tested?
out.wide$test <- out.wide$S.all + out.wide$R.all + out.wide$U

# Group hypotheses
out.wide$type <- ifelse(out.wide$pred < 6, c("Community Energetics"),
                        ifelse(out.wide$pred < 12, c("Community Structure"),
                               ifelse(out.wide$pred < 15,  c("Life History"),
                                      ifelse(out.wide$pred < 18, c("Nutrient Cycling"),
                                             ifelse(out.wide$pred < 20, c("Selection Pressure"),
                                                    c("Overall Homeostasis"))))))
# Results of Predictions tested
out.wide$S.perc <- 100*out.wide$S.all/out.wide$test
out.wide$R.perc <- 100*out.wide$R.all/out.wide$test
out.wide$U.perc <- 100*out.wide$U/out.wide$test

# Is overall Support, Ambiguity, or Uncertainty related to frequency tested?
# Regression analyses for supported hypotheses as a function of number of times tested
model.S_test <- lm(S.perc ~ test, data=out.wide)
summary(model.S_test)
hist(model.S_test$res)
plot(model.S_test)
# Regression analyses for rejected hypotheses as a function of number of times tested
model.R_test <- lm(R.perc ~ test, data=out.wide)
summary(model.R_test)
hist(model.R_test$res)
plot(model.R_test)
# Regression analyses for uncertain support hypotheses as a function of number of times tested
model.U_test <- lm(U.perc ~ test, data=out.wide)
summary(model.U_test)
hist(model.U_test$res)
plot(model.U_test)

# Summary stats of tests
Refute.perc <- sum(out.wide$R.all)/sum(out.wide$test)
Support.perc <- sum(out.wide$S.all)/sum(out.wide$test)
Unclear.perc <- sum(out.wide$U)/sum(out.wide$test)

# Are outcomes related to the number of tests?
ggplot(out.wide, aes(x=test, y=S.perc)) +
  geom_point(color="red", size=3) +
  theme_classic(14) 
ggplot(out.wide, aes(x=test, y=R.perc)) +
  geom_point(color="blue", size=3) +
  theme_classic(14) 
ggplot(out.wide, aes(x=test, y=U.perc)) +
  geom_point(color="green", size=3) +
  theme_classic(14) 

# Prep data so in long form
outcome.long <- melt(out.wide, id.vars = c("pred", "test", "type"), measure.vars = c("S.perc", "R.perc", "U.perc"))

#### Graph with colored background bars ####
# Sort on outcome
outcome.long$var.no <- ifelse(outcome.long$variable == "S.perc", 1,
                          ifelse(outcome.long$variable == "R.perc", 3, 2))
perc.data <- arrange(outcome.long, var.no)
pred.cat <- c("Community Energetics", "Community Structure", "Life History", "Nutrient Cycling", "Selection Pressure", "Overall Homeostasis")

plot.fig4 <- ggplot(perc.data, aes(x=factor(pred), y=value)) +
  geom_bar(stat="identity", aes(fill=type)) +
  scale_fill_brewer(palette="Dark2",
                    name="Category",
                      breaks=pred.cat) +
  geom_bar(stat="identity", fill="black", aes(alpha=variable)) +
  scale_alpha_manual(values=c(0.35,0.85,0),
                     name="Outcome", 
                     breaks=c("R.perc", "U.perc", "S.perc"),      # can use to set order
                     labels=c("Refute", "Ambiguous", "Support")) +  # Relabel
  theme_classic(11) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("Prediction #") +
  ylab("percent (%)") 
  #geom_hline(yintercept = 50, color="white")
plot.fig4 + theme(legend.key=element_rect(color="#EEEEEE")) 
ggsave("figures/Pred_outcomes_prop.jpeg", width=6.5, height=2.78, units="in")

# Extracting ggplot2 default colors
require(latticeExtra)
mycols <- dput(ggplot2like(n = 6, h.start=0, l=65)$superpose.line$col)

#################
# Extra Figures #
#################

# Grouped hypotheses tested
group.hyp <- ddply(out.wide, .(type, pred), summarize,
                   times = sum(test))
library(stringr) # used to wrap axis labels
ggplot(group.hyp, aes(x=type, y=times)) +
  geom_bar(stat="identity", aes(fill=type)) +
  theme_classic(20) +
  scale_y_continuous(expand=c(0,0.01)) +
  xlab("prediction") +
  ylab("papers tested (#)") +
  theme(legend.position="none") +
  scale_x_discrete(labels = function(x) str_wrap(x, width=10))


# Individual Hypotheses tested
ggplot(out.wide, aes(x=pred, y=test)) +
  geom_bar(stat="identity", aes(fill=type)) +
  theme_classic(20) +
  scale_y_continuous(expand=c(0,0.01)) +
  scale_x_continuous(expand=c(0.01,0.01)) +
  xlab("prediction") +
  ylab("papers tested (#)") +
  theme(legend.position=c(0.8,0.8))

