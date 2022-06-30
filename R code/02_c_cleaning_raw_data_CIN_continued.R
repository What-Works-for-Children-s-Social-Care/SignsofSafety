#******************* CIN cleaning continued ***********# 
# Description: 
#   Format CIN data after data errors have been corrected
#     1. correct errors in demographics with results from 02_b_information_merge2pop
#     2. continue formatting remaining variables. 
# *************************************************************#

rm(list=ls())
setwd("P:/")

source("./Working/00_Package_install.R")

# read in data
  # cin data with inconsistencies in gender, Ethnicity & DOB: 
  load("Working/Intermediate_output/CIN_inconsistent.Rdata")
  
  # demographics data with corrected covariates: 
  load("Working/Intermediate_output/demographics.Rdata")
  load("Working/Intermediate_output/DOBs.Rdata")

# consolidate the two 
  cin.inconsistent <- cin.inconsistent[,-which(colnames(cin.inconsistent) %in% c("CIN_DOB","Ethnicity","gender"))]
  
  # Ethnicity, gender
    demographics <- as.data.frame(demographics) 
    cin.complete <- merge(cin.inconsistent, demographics[,c("CIN_LAchildID_Anon","CIN_LA","gender","Ethnicity")],
                        by=c("CIN_LA","CIN_LAchildID_Anon"), all.x=TRUE)
    
    #compare 
    load("Working/Intermediate_output/CIN_inconsistent.Rdata")
    sum(cin.complete$Ethnicity=="Missing") > sum(cin.inconsistent$Ethnicity=="Missing")
    sum(cin.complete$gender=="Missing") > sum(cin.inconsistent$gender=="Missing") 

    cin.complete <- merge(cin.complete, DOB, by=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)
  
# keep DOB if age.ref is the same for all DOBs
  # create separate age at referrals
  cin.complete$CIN_DOB.1 <- as.Date(cin.complete$CIN_DOB.1)
  cin.complete$CIN_DOB.2 <- as.Date(cin.complete$CIN_DOB.2)
  cin.complete$age.1 <- floor(as.numeric(difftime(cin.complete$CIN_ReferralDate, cin.complete$CIN_DOB.1, units="days"))/365.25)
  cin.complete$age.2 <- floor(as.numeric(difftime(cin.complete$CIN_ReferralDate, cin.complete$CIN_DOB.2, units="days"))/365.25)
  cin.complete$age.3 <- floor(as.numeric(difftime(cin.complete$CIN_ReferralDate, cin.complete$CIN_DOB.3, units="days"))/365.25)
  cin.complete$age.4 <- floor(as.numeric(difftime(cin.complete$CIN_ReferralDate, cin.complete$CIN_DOB.4, units="days"))/365.25)
  head(cin.complete[, grepl("age|DOB|ReferralDate", colnames(cin.complete))])
  
  # consolidate age at referrals
  cin.complete$unique.age <- apply(cin.complete[,grepl("age", colnames(cin.complete))], 1, function(x) length(unique(x[!is.na(x)]))) 
  cin.complete$age.ref <- NA
  cin.complete[cin.complete$unique.age==1,"age.ref"] <- apply(cin.complete[cin.complete$unique.age==1,grepl("age\\.[0-9]$", colnames(cin.complete))], 1, function(x) min(x, na.rm=TRUE)) 

  cin.complete <- cin.complete[,-grep("CIN_DOB", colnames(cin.complete))]
  cin.complete <- cin.complete[,-which(colnames(cin.complete) %in% c("age.1","age.2","age.3","age.4", "unique.age"))] 
  
# continue reformatting variables  
  DT.cin <- data.table(cin.complete)

  # remove erroneous age at referral records
    DT.cin <- DT.cin[DT.cin$age.ref>=-1 | is.na(DT.cin$age.ref),]
    DT.cin <- DT.cin[DT.cin$age.ref<=17| is.na(DT.cin$age.ref),] 

  # create academic year
    # age 5-11: primary school
    # age 12-17: secondary school
  DT.cin[DT.cin$age.ref>=12, ac.year:="SEC"]  
  DT.cin[is.na(DT.cin$ac.year) & DT.cin$age.ref>=5, ac.year:="PRIM"] 
  DT.cin[DT.cin$age.ref<5, ac.year:="NONE"]
  DT.cin[is.na(DT.cin$ac.year), ac.year:="Missing"]

  # null impute lowinc: 
    # below school age --> don't code these children as missing but as a different code: 99
  DT.cin[age.ref<5 & is.na(lowinc), lowinc:=99]
  
  # Number of previous CPP: only collected for 2008/09 & 2012/13 onwards 
  # missing for anyone referred between 01/04/2009 & 31/03/2012. 
    table(DT.cin$CIN_NumberOfPreviousCPP, DT.cin$CIN_ACADYR, useNA="always")

  # public bank holiday: 
    # create an indicator if the referral occurred 
    #   (a) the day after a bank holiday 
    #   (b) the 5 days before the school holiday 
  bank.holidays <- read_excel(path="Working/Intermediate_output/Bankholidays_UK_modified.xlsx", col_names=TRUE, sheet="all_days_to_consider")
  bank.holidays$indicator.1 <- as.Date(bank.holidays$indicator.1)
  DT.cin$elev.risk <- ifelse(DT.cin$CIN_ReferralDate %in% bank.holidays$indicator.1, 1, 0) 

  # reformat reason for closure since misspelled
  # strip spaces and capitalise
  DT.cin$CIN_REC <- toupper(trimws(DT.cin$CIN_ReasonForClosure, which = c("both")))
  DT.cin[CIN_ReasonForClosure=="", CIN_REC:=NA]
  DT.cin[CIN_REC=="UNKN"|CIN_ReasonForClosure=="UNKNOWNCLOSUREREASON"|
          CIN_ReasonForClosure=="NON"|CIN_ReasonForClosure=="NONE",CIN_REC:="Unknown"]
  DT.cin[!(CIN_REC %in% c("RC1","RC2","RC3","RC4","RC5","RC6","RC7","RC8",NA,"Unknown")), CIN_REC:="Unknown"]

  # Final individual var names: 
  individ.covars <- c("disabled","asylum","lowinc","age.ref","ac.year","gender","Ethnicity",
                      "main.need","cat.abuse","elev.risk","CIN_NumberOfPreviousCPP")

  # create relevant year to link LA level covariates 
  DT.cin$"relev.year" <- year(DT.cin$CIN_ReferralDate)-(month(DT.cin$CIN_ReferralDate)<=3)

  cin.formatted <- as.data.frame(DT.cin)

  # remove variables we don't need anymore
  to.remove <- c("EVERFSM_6","EVERFSM_ALL","EVERFSM_6_P","EYPPE","EYPPR","FSM.6p","FSM6","FSM.6","EYPPeligible",
                 "FSMeligible","PPE","AcademicYear","OnRoll","CIN_ExpectedDOB","CIN_YearOfBirth",
                 "CIN_MonthOfBirth","CIN_Gender","CIN_Ethnicity","CIN_AsylumSeeking","NCyearActual",
                 "CIN_Disability","CIN_CPPindicator","CIN_LatestReferralDate","CIN_LatestClosureDate",
                 "CIN_IDACI_Score","CIN_IDACI_Rank","CIN_LatestCategoryOfAbuse","CIN_CategoryOfAbuse",
                 "CIN_PrimaryNeedCode", "CIN_LA_9CODE","CIN_LA_LGR","CIN_CIN_LA","IDCAIScore","IDACIRank",
                 "IDACIScore_10","IDACIRank_10","IDACIScore_15","IDACIRank_15","CIN_AgeStartOfCINPeriod")
  cin.formatted <- cin.formatted[,-which(colnames(cin.formatted) %in% to.remove)]

  # create missing category for categorical variables, as these will not be imputed. 
    categ.vars <- c("gender","Ethnicity","main.need","cat.abuse","lowinc")
    for (cols in categ.vars) {
      cin.formatted[cols][is.na(cin.formatted[cols])] <- "Missing"
    }
    for (cols in categ.vars) {
      cin.formatted[cols][cin.formatted[cols]==""] <- "Missing"
    } 
    
    stopifnot(sum(is.na(cin.formatted[,categ.vars]))==0)

  save(cin.formatted, "cin.formatted", file="Working/Intermediate_output/CIN_formatted.Rdata")

