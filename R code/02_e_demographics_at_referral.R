# ******* Consolidate demographics at referral stage **********

# for lowinc, cat.need and previous.CPP we want to know the status/value at the start of the cin episode
# i.e. at or right after the referral. 
# This script consolidates these demographics for each referral so that we have one entry per child & referral

rm(list=ls())
setwd("P:/")
source("./Working/00_functions.R")
load("Working/Intermediate_output/CIN_formatted.Rdata") # load cin data

# **** 1. MAIN NEED *************#

cat.need <- cin.formatted[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","main.need","CIN_ACADYR")]
cat.need <- cat.need[!duplicated(cat.need),]

sum(duplicated(cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]))
sum(duplicated(cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR")]))

# most duplicates stem from different ACADYR entries for same referral --> keep only earliest one
# for the ones where it still differs, recode as NA.
head(cat.need[duplicated(cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR")]) | 
                duplicated(cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR")], fromLast = TRUE),]) 
  cat.need <- data.table(cat.need)
  cat.need[,no.needs.all:=uniqueN(main.need), by=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")] 
  table(cat.need$no.needs) 
  dim(cat.need[cat.need$no.needs.all == 2 & cat.need$main.need == "Missing",]) 
  cat.need <- cat.need[!(no.needs.all>1 & main.need=="Missing"),] 
  cat.need[,no.needs:=uniqueN(main.need), by=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR")] 
  dim(cat.need[cat.need$no.needs == 2 & cat.need$main.need == "Missing",]) 
  cat.need[no.needs>1, main.need:=NA]
  cat.need <- cat.need[,!"no.needs"]
  cat.need <- cat.need[,!"no.needs.all"]
  
  cat.need <- as.data.frame(cat.need)
  cat.need <- cat.need[!duplicated(cat.need),]

  # for remaining duplicates, choose earliest academic year value recorded. 
  sum(duplicated(cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR")]))
  sum(duplicated(cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]))
  cat.need$acadyr <- as.numeric(substr(cat.need$CIN_ACADYR,1,4))
  cat.need <- cat.need[order(cat.need$CIN_ReferralDate, cat.need$acadyr),]
  dim(cat.need) 
  cat.need <- cat.need[!duplicated(cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]),]
  dim(cat.need) 
  
stopifnot(sum(duplicated(cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]))==0)
cat.need <- cat.need[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","main.need")]

# ****** 2. Previous CPPs ********#

no.CPPs <- cin.formatted[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_NumberOfPreviousCPP","CIN_ACADYR")]
no.CPPs <- no.CPPs[!duplicated(no.CPPs),]
no.CPPs <- rename.cols("CIN_NumberOfPreviousCPP","previous.CPP",no.CPPs)

# check how we can distinguish duplicate referrals where previous CPP differs
sum(duplicated(no.CPPs[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]))
sum(duplicated(no.CPPs[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR")]))

# for the ones where no.CPPs still differs, take minimum value. 
no.CPPs <- data.table(no.CPPs)

no.CPPs[is.na(previous.CPP), previous.CPP:=99] 
no.CPPs[, previous.CPP:=min(previous.CPP), by=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR")]
no.CPPs[previous.CPP==99, previous.CPP:=NA]

no.CPPs <- as.data.frame(no.CPPs)
no.CPPs <- no.CPPs[!duplicated(no.CPPs),]

# select by earliest academic year a
no.CPPs$acadyr <- as.numeric(substr(no.CPPs$CIN_ACADYR,1,4))
no.CPPs <- no.CPPs[order(no.CPPs$CIN_ReferralDate, no.CPPs$acadyr),]
no.CPPs <- no.CPPs[!duplicated(no.CPPs[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]),]

sum(duplicated(no.CPPs[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]))
# for the remaining duplicates, take minimum value. 

stopifnot(sum(duplicated(no.CPPs[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]))==0)
no.CPPs <- no.CPPs[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","previous.CPP")]


# *** 3. Lowinc ********
FSM.ref <- cin.formatted[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR","lowinc")]
FSM.ref <- FSM.ref[!duplicated(FSM.ref),]

sum(duplicated(FSM.ref[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]))
sum(duplicated(FSM.ref[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","CIN_ACADYR")]))

FSM.ref$acadyr <- as.numeric(substr(FSM.ref$CIN_ACADYR,1,4))
FSM.ref <- FSM.ref[order(FSM.ref$CIN_ReferralDate, FSM.ref$acadyr),]
FSM.ref <- FSM.ref[!duplicated(FSM.ref[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]),]

stopifnot(sum(duplicated(FSM.ref[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]))==0)
FSM.ref <- FSM.ref[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","lowinc")]


# *** merge together
covar.referral <- merge(FSM.ref, cat.need, by=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate"), all=TRUE)
stopifnot(dim(FSM.ref)[1] == dim(covar.referral)[1])
stopifnot(dim(cat.need)[1] == dim(covar.referral)[1]) 
covar.referral <- merge(covar.referral, no.CPPs, by=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate"), all=TRUE)
stopifnot(dim(no.CPPs)[1] == dim(covar.referral)[1])

save(covar.referral, "covar.referral", file="./Working/Intermediate_output/covars_at_referral.Rdata")
