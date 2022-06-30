
#******************* Creating information for our populations ***********# 
# Description: 
#   In 02 we formatted CIN data. Before restricting the data to our populations of interest, 
#   we need to obtain the information we need from the raw data for outcomes and demographics.
#   This includes:
#     1. ICPC:        information on all ICPCs any child has ever had in wide format --> used as outcome for RQ2. 
#     2. referrals:   information on all referrals any child has ever had in wide format.
#     3. CPP:         information on all CPPs any child has ever had in wide format.   
#     4. CLA:         information on all periods of care any child has ever had in wide format.


# *******************************************************************************************#

rm(list=ls())
setwd("P:/")

source("./Working/00_Package_install.R")

load("Working/Intermediate_output/CIN_formatted.Rdata") # created in 2c


# ****************** 1. ICPCs **********#

# note that the cleaning of the section 47 dataset that contained the information on ICPCs is not 
# included in the publication of this code due to publication clearance timelines for the SRS. 
  ICPC <- Section.47[,c("CIN_LAchildID_Anon","CIN_LA","CIN_DateOfInitialCPC")]
  ICPC$CIN_DateOfInitialCPC <- as.Date(ICPC$CIN_DateOfInitialCPC)
  
  # remove missing ICPCs or false years
  print(dim(ICPC)) 
  ICPC <- ICPC[!is.na(ICPC$CIN_DateOfInitialCPC),]
  print(dim(ICPC)) 
  ICPC <- ICPC[!(ICPC$CIN_DateOfInitialCPC<="2008-03-31"),]
  print(dim(ICPC)) 
  
  # remove duplicates if any
  ICPC <- ICPC[!duplicated(ICPC),]
  dim(ICPC)
  
  # rename column for simpliciy
  colnames(ICPC)[colnames(ICPC)=="CIN_DateOfInitialCPC"] <- "ICPC"
  
  # reshape long to wide to merge to ICPC.pop
    # create ID variable of ICPC dates.
    ICPC.dt <- data.table(ICPC)
    ICPC.dt <- ICPC.dt[order(ICPC.dt$CIN_LAchildID_Anon, ICPC.dt$ICPC),] 
    ICPC.dt[,id:=seq_len(.N), by=c("CIN_LAchildID_Anon","CIN_LA")]
  
    # reshape
    ICPC.dt <- reshape(ICPC.dt, direction="wide", v.names=c("ICPC"), timevar="id",idvar=c("CIN_LAchildID_Anon","CIN_LA"))
    dim(ICPC.dt[duplicated(CIN_LAchildID_Anon) | duplicated(CIN_LAchildID_Anon, fromLast = TRUE),]) 
    
    # re-format to Date
    for (i in grep("ICPC",colnames(ICPC.dt))) {
      ICPC.dt[[i]] <- as.Date(ICPC.dt[[i]])
    }
    
  # shape back to data frame
  ICPC <- as.data.frame(ICPC.dt)
  
  # save
  save(ICPC, "ICPC", file="Working/Intermediate_output/ICPCs.Rdata")

  
# ****************** 2. Referrals ********** #
  
  referrals <- cin.formatted[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate")]
  referrals$CIN_ReferralDate <- as.Date(referrals$CIN_ReferralDate)
  
  # remove missing ICPCs or false years
  dim(referrals)
  referrals <- referrals[!is.na(referrals$CIN_ReferralDate),]
  dim(referrals)
  
  # remove duplicates 
  referrals <- referrals[!duplicated(referrals),]
  stopifnot(sum(duplicated(referrals))==0)
  dim(referrals)
  
  # rename column
  colnames(referrals)[colnames(referrals)=="CIN_ReferralDate"] <- "refs"
  
  # reshape long to wide to merge to ICPC.pop
  # create ID variable of ICPC dates.
  ref.dt <- data.table(referrals)
  ref.dt <- ref.dt[order(ref.dt$CIN_LAchildID_Anon, ref.dt$refs),] 
  ref.dt[,id:=seq_len(.N), by=c("CIN_LAchildID_Anon","CIN_LA")]
  
  # reshape
  ref.dt <- reshape(ref.dt, direction="wide", v.names=c("refs"), timevar="id",idvar=c("CIN_LAchildID_Anon","CIN_LA"))
  dim(ref.dt[duplicated(CIN_LAchildID_Anon) | duplicated(CIN_LAchildID_Anon, fromLast = TRUE),]) 
   
  # re-format to Date
  for (i in grep("refs",colnames(ref.dt))) {
    ref.dt[[i]] <- as.Date(ref.dt[[i]])
  }
  
  # shape back to data frame
  refs <- as.data.frame(ref.dt)
  
  # save
  save(refs, "refs", file="Working/Intermediate_output/all_referrals.Rdata")
  
  
# ****************** 3. CPPs **********#
  
    CPPs <- cin.formatted[,c("CIN_LAchildID_Anon","CIN_LA","CIN_CPPstartDate")]
    CPPs$CIN_CPPstartDate <- as.Date(CPPs$CIN_CPPstartDate)
  
  # keep only CPPs and no duplicates
  dim(CPPs)
  CPPs <- CPPs[!is.na(CPPs$CIN_CPPstartDate),]
  dim(CPPs)
  CPPs <- CPPs[!duplicated(CPPs),]
  dim(CPPs) 
  # remove CPPs before 01/04/2008
  CPPs <- CPPs[!CPPs$CIN_CPPstartDate<"2008-04-01",]
  dim(CPPs) 

  # rename column and prepare reshape
  colnames(CPPs)[3] <- "CPP"
  CPP.dt <- data.table(CPPs)
  CPP.dt <- CPP.dt[order(CPP.dt$CIN_LAchildID_Anon, CPP.dt$CPP),] 
  CPP.dt[,id:=seq_len(.N), by=c("CIN_LAchildID_Anon","CIN_LA")]
  
  # reshape
  CPP.dt <- reshape(CPP.dt, direction="wide", v.names=c("CPP"), timevar="id",idvar=c("CIN_LAchildID_Anon","CIN_LA"))
  dim(CPP.dt[duplicated(CIN_LAchildID_Anon) | duplicated(CIN_LAchildID_Anon, fromLast = TRUE),])
  
  # re-format to Date
  for (i in grep("CPP",colnames(CPP.dt))) {
    CPP.dt[[i]] <- as.Date(CPP.dt[[i]])
  }
  
  # shape back to data frame
  CPPs <- as.data.frame(CPP.dt)
  
  # save
  save(CPPs, "CPPs", file="Working/Intermediate_output/all_CPPs.Rdata")

  
# ****************** 4. CLAs **********#
  
    CLAs <- read.csv("./Input/DR191206.02/Part 3 CLA Data/Episode_final_CLA.csv")
  
  # remove false entries
    dim(CLAs) 
    CLAs <- CLAs[!CLAs$poc_length<0,]
    dim(CLAs)

  # reformat date variables
    date.vars <- c("poc_start","date_episode_started","date_episode_ceased")
    CLAs[,which(colnames(CLAs) %in% date.vars)] <- lapply(CLAs[,which(colnames(CLAs) %in% date.vars)], as.Date)
  
  # remove any period of care that started prior to year 2008/09
    dim(CLAs) 
    CLAs <- CLAs[which(CLAs$poc_start>="2008-04-01"),]
    dim(CLAs) 
    
  # keep only CLAs and no duplicates
    CLAs <- as.data.frame(CLAs[,c("LA", "CHILD_LA_CODE_ANON","poc_start","poc_length")])
    dim(CLAs) 
    CLAs <- CLAs[!duplicated(CLAs[,c("LA", "CHILD_LA_CODE_ANON","poc_start")]),]
    dim(CLAs) 
  # rename column and prepare reshape
    colnames(CLAs)[3] <- "CLA.start"
    CLA.dt <- data.table(CLAs)

# ********** CHECK AFTER *********  
    # check for poc_length which is quite inconsistent across the different observations
      hist(CLA.dt$poc_length)
      hist(CLA.dt$poc_length[CLA.dt$poc_length<=100], 
          col='darkmagenta',
          breaks=100)
      hist(CLA.dt$poc_length[CLA.dt$poc_length<=30], 
          col='darkmagenta',
          breaks=30)
      length(unique(CLA.dt$CHILD_LA_CODE_ANON[CLA.dt$poc_length<=7]))
  
    CLA.dt <- CLA.dt[,c("LA","CHILD_LA_CODE_ANON","CLA.start")]
  
  # create id to prepare reshape
    CLA.dt <- CLA.dt[order(CLA.dt$CHILD_LA_CODE_ANON, CLA.dt$CLA.start),] 
    CLA.dt[,id:=seq_len(.N), by=c("CHILD_LA_CODE_ANON","LA")]
  
  # reshape
    CLA.dt <- reshape(CLA.dt, direction="wide", v.names=c("CLA.start"), timevar="id",idvar=c("CHILD_LA_CODE_ANON","LA"))
    dim(CLA.dt[duplicated(CHILD_LA_CODE_ANON) | duplicated(CHILD_LA_CODE_ANON, fromLast = TRUE),]) 
    
  # re-format to Date
    for (i in grep("CLA.start",colnames(CLA.dt))) {
      CLA.dt[[i]] <- as.Date(CLA.dt[[i]])
    }
  
  # shape back to data frame
    all.pocs <- as.data.frame(CLA.dt)
  
  # save
  save(all.pocs, "all.pocs", file="Working/Intermediate_output/all_CLA_periods.Rdata")
  