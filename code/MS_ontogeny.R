#######################
# Ontogeny of support #
# Number of papers supporting, rejecting or mentioning hypotheses over time #
#######################

source("code/dataprep.R")

library(plyr)

str(odum)
names(odum)
# odum[,i] <- as.factor(odum[,i])

############################################
# What is ontogeny of ecosystem succesion? #
############################################

# Code to reformat data file by prediction
ont.pubs <- function(hyp){
  # Select analyzed cases only
  ont <- odum[odum[,(hyp+29)] != "0", ]
  ont$out <- ont[,(hyp+29)]
  cdata <- ddply(ont, .(PY_5), summarize,
                 S = length(out[out == "S"]),
                 R = length(out[out == "R"]),
                 U = length(out[out == "U"]))
  cdata } #close function
# Determine # of papers that resulted in Support, Rrejection, or Uncertain outcome for predictions in 5 year intervals
ont.pubs.all <- c(0,0,0,0)
for (i in 1:24){
  a <- ont.pubs(i)
  pred <- rep(i,n = length(a))
  c <- cbind(a,pred)
  ont.pubs.all <- rbind(ont.pubs.all, c)
}
pubs.all <- ont.pubs.all[-1,]
# Calculate support in 5-yr interval across all tests of predictions
pubs.year <- ddply(pubs.all, .(PY_5), summarise,
                   S.tot = sum(S),
                   U.tot = sum(U),
                   R.tot = sum(R))
pubs.year
pubs.year$n <- pubs.year$S.tot + pubs.year$U.tot + pubs.year$R.tot
pubs.year$S.perc <- pubs.year$S.tot/pubs.year$n
pubs.year$R.perc <- pubs.year$R.tot/pubs.year$n
pubs.year$U.perc <- pubs.year$U.tot/pubs.year$n

pubs.year.long <- melt(pubs.year[,c(1,5:8)], id=c(1,2))

# Yearly acceptance
ggplot(pubs.year.long, aes(x=PY_5, y=value, group=variable)) +
  geom_line(size=2, aes(color=variable)) +
  theme_classic(9) +
  theme(legend.position="top") +
  scale_color_manual(name="outcome",
                       values=c("black", "#EEEEEE", "#BBBBBB"),
                       breaks=c("R.perc", "U.perc", "S.perc"),
                       labels=c("Refute", "Ambiguous", "Support")) +
  xlab("Publication Year (5-yr interval)") +
  ylab("%")+
  theme(axis.line.x=element_line(), # code to fix ggplot bug 
        axis.line.y=element_line())
ggsave("figures/Ontogeny_allpreds.jpeg", width=3, height=2, units="in")

#########################
# Cumulative acceptance #
#########################
ont.yr <- function(hyp){
  # Select analyzed cases only
  ont <- odum[odum[,(hyp+29)] != "0", ]
  ont$out <- ont[,(hyp+29)]
  cdata <- ddply(ont, .(PY), summarize,
                 S = length(out[out == "S"]),
                 R = length(out[out == "R"]),
                 U = length(out[out == "U"]))
  cdata } #close function
# Determine # of papers that S, R, or U prediction in 5 year interval
ont.yearly <- c(0,0,0,0)
for (i in 1:24){
  a <- ont.yr(i)
  pred <- rep(i,n = length(a))
  c <- cbind(a,pred)
  ont.yearly <- rbind(ont.yearly, c)
}
pubs.annual <- ont.yearly[-1,]
# Calculate support in 5-yr interval across all tests of predictions
pred.yearly <- ddply(pubs.annual, .(PY), summarise,
                   S.tot = sum(S),
                   U.tot = sum(U),
                   R.tot = sum(R))
pred.yearly$n <- pred.yearly$S.tot + pred.yearly$U.tot + pred.yearly$R.tot
for(i in 2:5){
  pred.yearly[,i+4] <- cumsum(pred.yearly[,i])
}
names(pred.yearly)[6:9] <- c("S.cum", "U.cum", "R.cum", "n_tot")
# convert to long form
yr.long <- melt(pred.yearly[,c(1,6:8)], id=1)

# Fitting an exponential 
model <- lm(value ~ PY+variable, data=yr.long)
summary(model)
model.sup <- lm(value~PY, data=subset(yr.long, variable == "S.cum"))
summary(model.sup)
model.ref <- lm(value~PY, data=subset(yr.long, variable == "R.cum"))
summary(model.ref)
# Compare rates
(model.sup$coefficients[2] - model.ref$coefficients[2])/model.ref$coefficients[2]
  
ggplot(yr.long, aes(x=PY, y=value, group=variable)) +
  geom_point(size=2, aes(color=variable)) +
  theme_classic(10)  +
  scale_color_manual(name="outcome",
                     values=c("black", "#EEEEEE", "#BBBBBB"),
                     breaks=c("R.cum", "U.cum", "S.cum"),
                     labels=c("Refute", "Ambiguous", "Support")) +
  stat_smooth(method="lm", aes(color=variable), se=FALSE, alpha=0.5) +
  theme(axis.line.x=element_line(), # code to fix ggplot bug 
        axis.line.y=element_line(),
        legend.position=c(0,1), legend.justification=c(0,1)) +
  xlab("Publication Year") +
  ylab("Total tests of prediction (#)") 
ggsave("figures/pred_cum.jpeg", width=3, height=3, units="in")

# What are the characteristics of outcomes?
model.n_S <- lm(value ~ n, data=pubs.year.long[pubs.year.long$variable == "S.perc", ] )
summary(model.n_S) # significant, but weak relationship!
model.n_R <- lm(value ~ n, data=pubs.year.long[pubs.year.long$variable == "R.perc", ] )
summary(model.n_R) # n.s.
model.n_U <- lm(value ~ n, data=pubs.year.long[pubs.year.long$variable == "U.perc", ] )
summary(model.n_U) # n.s.

######################################
# Ontogeny of individual predictions #
######################################

ont.8 <- odum[odum$O.8 != "0", ]

stat.8 <- ddply(ont.8, .(PY), summarize,
                S = length(O.8[O.8 == "S"]),
                R = length(O.8[O.8 == "R"]),
                U = length(O.8[O.8 == "U"]),
                n = length(O.8))
stat.8

stat.8$S.prop <- stat.8$S/stat.8$n
stat.8$R.prop <- stat.8$R/stat.8$n
stat.8$U.prop <- stat.8$U/stat.8$n

plot(S.prop ~ PY, data=stat.8, col="white")
lines(S.prop~PY, data=stat.8)
lines(R.prop~PY, data=stat.8, col="blue")
lines(U.prop~PY, data=stat.8, col="green")

melt.8 <- melt(stat.8[,c(1,6:8)], id.var=1)

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

stat <- ontogeny(8)

ggplot(stat, aes(x=PY_5, y=n)) +  geom_bar(stat="identity") + theme_classic(18)

# for an individual prediction dataset
ont.1 <- ontogeny(1)

ggplot(ont.1, aes(x=PY_5, y=value)) +
  geom_area(aes(fill=variable)) +
  scale_fill_manual(values=c("black", "white", "grey"),
                    name="",
                    breaks=c("R.prop", "U.prop", "S.prop"),
                    labels=c("Refute", "Ambiguous", "Support")) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  theme_classic(18) +
  xlab("Publication Year") +
  ylab("Proportion")

# Look at ontogeny across all the hypotheses #

ont.all <- c(0,0,0,0,0,0)
for (i in 1:24){
  a <- ontogeny(i)
  pred <- rep(i,n = length(a))
  c <- cbind(a,pred)
  ont.all <- rbind(ont.all, c)
}
odum.time <- ont.all[-1,]

# basic line plot
ggplot(odum.time, aes(x=PY_5, y=value, color=pred, group=pred)) +
  geom_line(aes(color=pred)) +
  theme_classic(14) +
  xlab("Year (5-year interval)") +
  ylab("%") +
  theme(legend.position="top") +
  facet_wrap(~variable, ncol=1)

# ternary plot
library(ggtern)

odata <- dcast(PY_5 + pred + n ~ variable, value.var="value", data=odum.time)

odata$type <- ifelse(odata$pred < 6, c("Community Energetics"),
                        ifelse(odata$pred < 12, c("Community Structure"),
                               ifelse(odata$pred < 15,  c("Life History"),
                                      ifelse(odata$pred < 18, c("Nutrient Cycling"),
                                             ifelse(odata$pred < 20, c("Selection Pressure"),
                                                    c("Overall Homeostasis"))))))


ggtern(odata, aes(x=R.prop, y=S.prop, z=U.prop)) +
  geom_point(aes(size=n), alpha=0.5) +
  geom_path(aes(color=PY_5), size=1.5, group=pred) +
  scale_color_gradient(low="orange", high="blue") +
  facet_wrap(~pred) +
  theme_showarrows() 

# Is support for a prediction related to frequency of testing?
ggplot(odum.time, aes(x=n, y=value)) +
  geom_point(aes(color=variable), size=3) +
  xlab("publications") +
  theme_classic(14)+
  facet_wrap(~variable, ncol=1)
model.pubs_S <- lm(value ~ n + pred, data=odum.time[odum.time$variable == "S.prop", ])
summary(model.pubs_S) # n.s.
model.pubs_R <- lm(value ~ n + pred, data=odum.time[odum.time$variable == "R.prop", ])
summary(model.pubs_R) # n.s.
model.pubs_U <- lm(value ~ n + pred, data=odum.time[odum.time$variable == "U.prop", ])
summary(model.pubs_U) # n.s.

p <- ggplot(odum.time, aes(x=PY_5, y=value, color=variable)) +
  geom_point(aes(size=n)) +
  geom_line() +
  theme_classic(14)
p + facet_wrap(~pred)

