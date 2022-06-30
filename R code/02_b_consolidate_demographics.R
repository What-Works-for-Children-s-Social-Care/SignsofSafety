
#******************* Creating information for our populations ***********# 
# Description: 
#   02_ formatted CIN data. 
#   This script builds on the information and cleans the covariates for cases where 
#   the same child has differing supposedly non-varying information over different records

# *******************************************************************************************#

# *** 1. Demographics ******

rm(list=ls()) 
setwd("P:/") 
source("./Working/00_Package_install.R") 

load("Working/Intermediate_output/CIN_inconsistent.Rdata")

# ***************** CREATE DATA TO MERGE W/ ASSESSMENTS/CLA *******************~

  # for assessments:
  demographics <- cin.inconsistent[,c("CIN_LAchildID_Anon","CIN_LA","gender","asylum","Ethnicity",
                                      "disabled","CIN_DOB","CIN_ReferralDate")] 

  # remove duplicates 
  demographics <- demographics[order(demographics$CIN_ReferralDate),]
  demographics <- demographics[,-which(colnames(demographics) %in% "CIN_ReferralDate")]
  demographics <- demographics[!duplicated(demographics, fromLast=TRUE),]

  sum(duplicated(demographics[,c("CIN_LAchildID_Anon","CIN_LA")]))
  sum(duplicated(demographics[,c("CIN_LAchildID_Anon","CIN_LA","CIN_DOB")])) 
  sum(duplicated(demographics[,c("CIN_LAchildID_Anon","CIN_LA","CIN_DOB","gender")])) 
  sum(duplicated(demographics[,c("CIN_LAchildID_Anon","CIN_LA","CIN_DOB","Ethnicity")])) 
  
# *** create DOB record for multiple information ***********
  sub.DOB <- demographics[,c("CIN_LAchildID_Anon","CIN_LA","CIN_DOB")]
  sub.DOB <- sub.DOB[!duplicated(sub.DOB),]
  sum(duplicated(sub.DOB[,c("CIN_LAchildID_Anon","CIN_LA")])) 
  summary(sub.DOB$CIN_DOB)

  DOB <- data.table(sub.DOB)
  DOB <- DOB[complete.cases(DOB),] 
  
  DOB[,id:=seq_len(.N), by=c("CIN_LAchildID_Anon","CIN_LA")] 
  dim(DOB[DOB$id==2 & is.na(DOB$CIN_DOB),])[1]==0 
  
  DOB <- reshape(DOB, direction="wide", v.names=c("CIN_DOB"), timevar="id",idvar=c("CIN_LAchildID_Anon","CIN_LA"))

  save(DOB, "DOB", file="Working/Intermediate_output/DOBs.Rdata")

# ************ End DOB *************


  # *** replace as missing for other covariates that differ over time for the same child ***********

  demographics <- demographics[,-which(colnames(demographics) %in% "CIN_DOB")]
  demographics <- demographics[!duplicated(demographics),]

    # remaining covariates: 
    demographics <- data.table(demographics) 
    demographics[,n.ethnicity:=uniqueN(Ethnicity), by=c("CIN_LAchildID_Anon","CIN_LA")] 
    dim(demographics) 
    demographics <- demographics[!(n.ethnicity>1 & Ethnicity=="Missing"),] 
    dim(demographics) 
    demographics[,n.ethnicity:=uniqueN(Ethnicity), by=c("CIN_LAchildID_Anon","CIN_LA")]
    demographics[demographics$n.ethnicity>1, "Ethnicity"] <- "Missing"
    demographics <- demographics[,!"n.ethnicity"] 
    demographics <- demographics[!duplicated(demographics),]
    
  # gender 
    demographics[,n.gender:=uniqueN(gender), by=c("CIN_LAchildID_Anon", "CIN_LA")] 
    demographics <- demographics[!(n.gender>1 & gender=="Missing"),]  
    demographics[,n.gender:=uniqueN(gender), by=c("CIN_LAchildID_Anon", "CIN_LA")]
    
    demographics[demographics$n.gender>1, "gender"] <- "Missing"
    demographics <- demographics[,!"n.gender"]
    demographics <- demographics[!duplicated(demographics),]
  
  # asylum and disabled are all fine
  stopifnot(sum(duplicated(demographics[,c("CIN_LAchildID_Anon","CIN_LA")])) == sum(duplicated(demographics[,c("CIN_LAchildID_Anon","CIN_LA","asylum")])))
  stopifnot(sum(duplicated(demographics[,c("CIN_LAchildID_Anon","CIN_LA")])) == sum(duplicated(demographics[,c("CIN_LAchildID_Anon","CIN_LA","disabled")])))
  
  stopifnot(sum(duplicated(demographics))==character(0))


save(demographics, "demographics", file="Working/Intermediate_output/demographics.Rdata")
