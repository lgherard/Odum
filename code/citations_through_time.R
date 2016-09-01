##########################
# Citations through time #
# Odum 1969 Project      #
##########################

library(ggplot2)
odum.c <- as.data.frame(table(odum$PY))
odum.c$Year <- as.numeric(as.character(odum.c$Var1))
ggplot(odum.c, aes(x=Year, y=Freq)) +
  geom_point(size=3) +
  theme_classic(16) +
  ylab("Citations")

# Cumulative citations
odum.c$cum.cites <- cumsum(odum.c$Freq)

ggplot(odum.c, aes(x=Year, y=cum.cites)) +
  geom_point(size=3) +
  theme_classic(16) +
  ylab("Total Citations")

# Fitting an exponential
ex.model <- lm(log(Freq) ~ Year, data=odum.c)
summary(ex.model)
# Create non-linear figure
odum.c$pred_y <- exp(predict(ex.model, list(Year=odum.c$Year)))
ggplot(odum.c, aes(x=Year, y=Freq)) +
  geom_line(aes(x=Year, y=pred_y), color="grey", size=2) +
  geom_point(size=3) +
  theme_classic(16) +
  ylab("Total Citations")

