
#******************* CIN initial cleaning & merging individual data sets together ***********# 
# Description: 
#   Format CIN data 
#     0. reformat data to the right data types (e.g. date, numeric, etc.)
#     1. recode missing to NA
#     2. consolidate demographics
#     3. consolidate plans 

# *******************************************************************************************#


rm(list=ls())
setwd("P:/")

source("./Working/00_Package_install.R")

load("Working/Intermediate_output/CIN_merged_raw.Rdata") # from 01_merge_cin_data

##### *************** FORMAT DATA ***************#
  # reformat "" to NA
  all.cin$CIN_CPPendDate[all.cin$CIN_CPPendDate==""] <- NA
  all.cin$CIN_CPPstartDate[all.cin$CIN_CPPstartDate==""] <- NA
  all.cin$CIN_CINClosureDate[all.cin$CIN_CINClosureDate==""] <- NA
  all.cin$CIN_DateOfInitialCPC[all.cin$CIN_DateOfInitialCPC==""] <- NA
  
  # Format dates
  date.cols <- c("CIN_DOB","CIN_LatestReferralDate","CIN_LatestClosureDate","CIN_CPPstartDate","CIN_ExpectedDOB","CIN_PersonDeathDate",
                 "CIN_CPPendDate","CIN_ReferralDate","CIN_CINClosureDate","CIN_DateOfInitialCPC") 

  all.cin[["CIN_DOB"]] <- as.Date(all.cin[["CIN_DOB"]])
  all.cin[["CIN_ReferralDate"]] <- as.Date(all.cin[["CIN_ReferralDate"]])
  all.cin[["CIN_CINClosureDate"]] <- as.Date(all.cin[["CIN_CINClosureDate"]])
  all.cin[["CIN_CPPstartDate"]] <- as.Date(all.cin[["CIN_CPPstartDate"]])
  all.cin[["CIN_CPPendDate"]] <- as.Date(all.cin[["CIN_CPPendDate"]])
  all.cin[["CIN_DateOfInitialCPC"]] <- as.Date(all.cin[["CIN_DateOfInitialCPC"]])

  # include only referrals/episodes where the referral date has been in 2008/09 or later. 
  all.cin <- all.cin[all.cin$CIN_ReferralDate>="2008-04-01",] 
  all.cin <- all.cin[year(all.cin$CIN_ReferralDate)<=2019,] 
  all.cin <- all.cin[!is.na(all.cin$CIN_ReferralDate),] 
  all.cin <- all.cin[all.cin$CIN_ReferralDate != "",]  
  
##### *** impute missing information - demographics for children that occur several times.
  DT.cin <- data.table(all.cin)

  # latest category of abuse:
  DT.cin$cat.abuse <- DT.cin$CIN_LatestCategoryOfAbuse 
  DT.cin[cat.abuse=="", cat.abuse:=NA]
  
  # primary need code: 
  DT.cin$main.need <- DT.cin$CIN_PrimaryNeedCode
  DT.cin[main.need=="n0",main.need:="N0"]
  DT.cin[main.need=="n1",main.need:="N1"]
  DT.cin[main.need=="n2",main.need:="N2"]
  DT.cin[main.need=="n4",main.need:="N4"]
  DT.cin[main.need=="n8",main.need:="N8"]
  DT.cin[main.need=="UnmappedNeedCode"|main.need=="UNMAPPEDNEEDCODE",main.need:=NA]
  
  # Gender: as specified in the TP: 0= not recorded/unborn, 1=male, 2= female, 3=indeterminate, 4= missing
  DT.cin$"gender" <- ifelse(DT.cin$CIN_Gender<=2, DT.cin$CIN_Gender, NA)
  DT.cin$gender[DT.cin$CIN_Gender==9] <- 3
  DT.cin$gender[is.na(DT.cin$CIN_Gender)|DT.cin$CIN_Gender==""] <- "Missing"

  # Ethnicity: recode to major groups
  DT.cin$"Ethnicity" <- DT.cin$CIN_Ethnicity
  DT.cin$"Ethnicity"[grep("^W",DT.cin$CIN_Ethnicity)] <- "WHIT"
  DT.cin$"Ethnicity"[grep("^B",DT.cin$CIN_Ethnicity)] <- "BLAC"
  DT.cin$"Ethnicity"[grep("^A",DT.cin$CIN_Ethnicity)] <- "ASIA" 
  DT.cin$"Ethnicity"[grep("^M",DT.cin$CIN_Ethnicity)] <- "MIXD"
  DT.cin$"Ethnicity"[grep("^CH",DT.cin$CIN_Ethnicity)] <- "CHIN"
  DT.cin$"Ethnicity"[DT.cin$CIN_Ethnicity=="OOTH"] <- "AOEG"
  DT.cin$"Ethnicity"[DT.cin$CIN_Ethnicity=="NOBT"|DT.cin$CIN_Ethnicity=="nobt"|DT.cin$CIN_Ethnicity=="REFU"] <- "UNCL"
  DT.cin$"Ethnicity"[is.na(DT.cin$Ethnicity)|DT.cin$Ethnicity==""|DT.cin$Ethnicity=="NA"|DT.cin$Ethnicity=="NA  "] <- "Missing"
  
  # take maximum value for binary indicators
  DT.cin[, disabled:=max(CIN_Disability), by=c("CIN_LAchildID_Anon","CIN_LA")] 
  
  # asylum seeking: remove entries ==2 since the NPD definitions only offer options =0 | =1
  DT.cin[CIN_AsylumSeeking==2, CIN_AsylumSeeking:=NA]
  DT.cin[, asylum:=max(CIN_AsylumSeeking, na.rm=TRUE), by=c("CIN_LAchildID_Anon","CIN_LA")]
  DT.cin[is.infinite(asylum),asylum:=NA]

  # Ever FSM 
    # --> create separate vars and consolidate.FSM_all gives information on census day, 
    # use EVERFSM_6_P & EVERFSM_6 instead of EVERFSM_ALL 
    # Use EVERFSM_6 and where we have additional information (e.g. FSM_6 or EYPP) that 
    # refers to the current referral/case, then only extrapolate information from PREVIOUS referrals
    
    # current FSM status: locf
    DT.cin[,FSM.elig:=FSMeligible]
    DT.cin[FSMeligible==0, FSM.elig:=NA] 
    DT.cin[order(CIN_ReferralDate,CIN_ACADYR),FSM.locf:=na.locf(FSM.elig, na.rm=FALSE), by=c("CIN_LAchildID_Anon","CIN_LA")] 
    DT.cin[FSMeligible==0 & is.na(FSM.locf), PPE.locf:=0]  
    
    # pupil premium eligibility: EYPPE/EYPPR (for different years). 
    DT.cin[EYPPE=="N",PPE:=0] 
    DT.cin[EYPPE=="Y",PPE:=1]
    DT.cin[PPE==0, PPE:=NA]
    DT.cin[EYPPE==""|is.na(EYPPE),PPE:=NA]
    DT.cin[order(CIN_ReferralDate,CIN_ACADYR),PPE.locf:=na.locf(PPE, na.rm=FALSE), by=c("CIN_LAchildID_Anon","CIN_LA")]
    DT.cin[EYPPE=="N" & is.na(PPE.locf), PPE.locf:=0]  
    
    DT.cin[EYPPeligible=="N",P.elig:=0] 
    DT.cin[EYPPeligible=="Y",P.elig:=1]
    DT.cin[P.elig==0, P.elig:=NA] 
    DT.cin[EYPPeligible==""|is.na(EYPPeligible),P.elig:=NA]
    DT.cin[order(CIN_ReferralDate,CIN_ACADYR),P.elig.locf:=na.locf(P.elig, na.rm=FALSE), by=c("CIN_LAchildID_Anon","CIN_LA")]
    DT.cin[EYPPeligible=="N" & is.na(P.elig.locf), P.elig.locf:=0]
    
    # consolidate all together:
    # code only children as 0 if we actually have information about that. If it is NA, code as missing.
    relev.cols <- which(colnames(DT.cin) %in% c("EVERFSM_ALL","EVERFSM_6","EVERFSM_6_P","FSM.locf","PPE.locf","P.elig.locf"))
    DT.cin[,lowinc:=rowSums(.SD, na.rm=TRUE), .SDcols=relev.cols]
    DT.cin[lowinc>=1,lowinc:=1]
    
    # replace lowinc as missing if all indicators are NA
    DT.cin[,rows.na:=rowSums(is.na(.SD)), .SDcols=relev.cols]
    DT.cin[rows.na==6, lowinc:=NA]
    
    DT.cin <- DT.cin[,-c("EVERFSM_ALL","EVERFSM_6","EVERFSM_6_P","FSM.locf","P.elig","PPE.locf","P.elig.locf","FSM.elig","FSMeligible","rows.na")]
    cin.inconsistent <- as.data.frame(DT.cin)
    save(cin.inconsistent, "cin.inconsistent", file="Working/Intermediate_output/CIN_inconsistent.Rdata")
    