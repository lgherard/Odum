################################
# Data QA/QC for Odum Analysis #
################################

odum <- read.csv("data/OdumPredictions.csv")

##############
# Data QA/QC #
##############

#############################################
# Recoding as necessary for manuscript type #
#############################################

levels(odum$Type.paper)[levels(odum$Type.paper)=="D,R"] <- "D/R"
levels(odum$Type.paper)[levels(odum$Type.paper)=="D,M,A"] <- "D/M/A"
levels(odum$Type.paper)[levels(odum$Type.paper)=="DM"] <- "D/M"
levels(odum$Type.paper)[levels(odum$Type.paper)=="m"] <- "M"
levels(odum$Type.paper)[levels(odum$Type.paper)=="M,D"] <- "M/D"
levels(odum$Type.paper)[levels(odum$Type.paper)=="M/r"] <- "M/R"
levels(odum$Type.paper)[levels(odum$Type.paper)=="d"] <- "D"
# For presentation, will use first option listed if more than one.
odum$Type.paper <- as.character(odum$Type.paper)
odum$Type.p <- substring(odum$Type.paper, 1, 1)
odum$Type.p <- as.factor(odum$Type.p)

#################################
# Cleaning up data entry issues #
#################################

# Just take first entry for any hypothesis
hyp.triage <- function (x) {
  hyp <- substring(x, 1, 1) # chose 1st character in string
  hyp.n <- as.numeric(hyp) # convert to a number
  return(hyp.n)
}
hyp.ok <- apply(odum[,6:29], 2, hyp.triage)
odum[,6:29] <- hyp.ok

# Recoding as necessary for outcomes of prediction tests
rename <- function(x){
  x[x == "r"] <- "R"
  x[x == "s"] <- "S"
  x[x == "u"] <- "U"
  x[x == "U?"] <- "U"
  x[x == "1"] <- "S"
  x[x == "2"] <- "R"
  return(x)
}
odum[,30:53] <- apply(odum[,30:53], 2, rename)

# Set interval for analysis (5 year)
# 1st and final intervals > 5 yrs; 1st: 1969 - 
odum$PY_5 <- ifelse(odum$PY < 1976, 1975,
                    ifelse(odum$PY < 1981, 1980,
                           ifelse(odum$PY < 1986, 1985,
                                  ifelse(odum$PY < 1991, 1990,
                                         ifelse(odum$PY < 1996, 1995,
                                                ifelse(odum$PY < 2001, 2000,
                                                       ifelse(odum$PY < 2006, 2005,
                                                                     2010)))))))