###** Create relevant parameters for main analysis

# Description: 
# creates relevant parameters for main analysis such as: treated LAs, cutoff dates, comparators/matches

rm(list=ls())
setwd("P:/")


format.matches <- function(df, pilot.LAs) {
  df$"indicator" <- c(1:nrow(df))
  df.long <- reshape(df, direction="long", idvar="indicator", varying=list(c("Pilot LA", "Match 1","Match 2")))
  df.long <- df.long[!is.na(df.long$"Pilot LA"), -which(colnames(df.long) %in% "time")]
  # rename cols
  df.final <- df.long[,c("Pilot LA","indicator","Embedding.start","Embedding.end")]
  colnames(df.final) <- c("Local.authority","Matched.group","Embedding.start","Embedding.end")
  # add treated indicator
  df.final$"SofS.LA" <- ifelse(df.final$Local.authority %in% pilot.LAs, 1, 0)
  return(df.final)
}

### 1.) list of all LAs we will need: 

la.codes <- read_excel("xx/LAD16 to LEA09 Look Up.xlsx")
la.codes <- la.codes[,which(colnames(la.codes) %in% c("LEA09CD","LEA09NM"))]
la.codes <- la.codes[!duplicated(la.codes$LEA09NM)==TRUE,]

# read in our matches. 
all.matches <- read_excel("xx/MatchingData.xlsx", 
                          sheet="Number of unique LAs", col_names = FALSE)
colnames(all.matches) <- "Local.authority"
setdiff(all.matches$Local.authority, la.codes$LEA09NM)
all.matches$Local.authority[all.matches$Local.authority=="Telford & Wrekin"] <- "Telford and Wrekin"
all.matches$Local.authority[all.matches$Local.authority=="Southend-on-Sea"] <- "Southend on Sea"
all.matches$Local.authority[all.matches$Local.authority=="Kingston Upon Hull"] <- "Kingston upon Hull"

matches.codes <- la.codes[which(la.codes$LEA09NM %in% all.matches$Local.authority),]
colnames(matches.codes) <- c("LA.code", "Local.authority")
save(matches.codes, "matches.codes", file="P:/Working/Intermediate_output/all_LAs_codes.Rdata")

## (2.) define treated LAs, corresponding matches & embedding periods. 
outcome.groups <- c("Duration","ICPC","Referrals","Kinship Care") 
matches <- list()
for (i in seq_along(outcome.groups)) {
  matches[[i]] <- read_excel("xx/MatchingData.xlsx",skip=1, sheet=outcome.groups[[i]])
  matches[[i]] <- matches[[i]][,1:3]
  matches[[i]][matches[[i]]=="N/A"]<- NA
}

# create extra matching group since we have two referral outcomes
names(matches) <- outcome.groups
names(matches)[[3]] <- "ReferralNFA"
matches[[5]] <- matches[[3]]
names(matches)[[5]] <- "Re-referral"

# define treated LAs
treated.LAs <- unique(matches[[1]]$"Pilot LA")

# define embedding period
embedding.period <- read_excel("xx/Embedding periods pilot LAs - overview_final - formatted.xlsx")
embedding.period <- embedding.period[embedding.period$`Pilot LAs` %in% treated.LAs,c("Pilot LAs","Implementation date final", "Fully embedded final")]
embedding.period$"Implementation date final" <- as.Date(embedding.period$"Implementation date final")
embedding.period$"Fully embedded final" <- as.Date(embedding.period$"Fully embedded final")
colnames(embedding.period) <- c("Pilot LA", "Embedding.start", "Embedding.end")

# add embedding period information to list 
matches <- lapply(seq_along(matches), function(x) merge(matches[[x]], embedding.period, by.x="Pilot LA", by.y="Pilot LA", all.x=TRUE))

# remove LAs for which we don't have any matches 
matches <- lapply(seq_along(matches), function(x) matches[[x]] <- matches[[x]][!(is.na(matches[[x]]$"Match 1") & is.na(matches[[x]]$"Match 2")),])

# re-format matches from wide to long, adding an indicator for treated and adding the group allocations
matches.final <- lapply(seq_along(matches), function(x) format.matches(matches[[x]], pilot.LAs = treated.LAs))

# add LA code to matches 
matches.codes$Local.authority[matches.codes$Local.authority=="Telford and Wrekin"] <- "Telford & Wrekin"
matches.codes$Local.authority[matches.codes$Local.authority=="Southend on Sea"] <- "Southend-on-Sea"
matches.codes$Local.authority[matches.codes$Local.authority=="Kingston upon Hull"] <- "Kingston Upon Hull"

matches.final <- lapply(seq_along(matches.final), function(x) merge(matches.final[[x]], matches.codes, by="Local.authority", all.x=TRUE))

list.names <- c("Duration", "ICPC", "ReferralNFA","Kinship Care","Re-referral")
matches.final <- setNames(matches.final, list.names)
save(matches.final, "matches.final", file="P:/Working/Intermediate_output/parameters.Rdata")
                  