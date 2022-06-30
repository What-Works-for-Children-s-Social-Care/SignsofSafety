
#******************* CLA demographics data ***********# 
# Description: 
#   (a) CLA files of the first period of care each year are available in individual years, merged with the school census
#       This script merges these together and obtains FSM information. 
#   (b) merge other demographics from CIN files: # of previous CPPs, disability, latest category of abuse

# *******************************************************************************************#

rm(list=ls())
setwd("P:/")
Sys.setenv(R_HISTFILE="P://Working")

source("./Working/00_Package_install.R")
source("./Working/00_Functions.R")

# **********1. FSM eligibility ******************************#
  
  # 0. read in data
    listed.files <- list.files("./xx")[grep("CLA",list.files("./xx"))]
    list.cla <- lapply(seq_along(listed.files), function(x) as.data.frame(read.csv(sprintf("xx/%s",listed.files[[x]]), stringsAsFactors = FALSE)))
    names(list.cla) <- c(2008:2018) 
    save(list.cla, "list.cla", file="Working/Intermediate_output/CLA_raw_demog.Rdata")

  load("Working/Intermediate_output/CLA_raw_demog.Rdata") 

  # ******** limit list to LAs from our pilot and comparator LAs only  ********
    load("P:/Working/Intermediate_output/all_LAs_codes.Rdata") # matches & codes
  
    ## test all codes are available in each CIN year 
    for (i in seq_along(list.cla)) {
      print(names(list.cla)[[i]])
      print(setdiff(matches.codes$LA.code, list.cla[[i]]$"CLA_CLA_LA"))
      stopifnot(length(setdiff(matches.codes$LA.code, list.cla[[i]]$"CLA_CLA_LA"))==0) 
    }
  
  list.cla2 <- lapply(seq_along(list.cla), function(x) list.cla[[x]][which(list.cla[[x]]$CLA_CLA_LA %in% matches.codes$LA.code),])

  # checking just LAs of interest selected
  for (i in seq_along(list.cla2)) {
    print(setdiff(list.cla2[[i]]$CLA_CLA_LA, matches.codes$LA.code))
    print(setdiff(matches.codes$LA.code, list.cla2[[i]]$CLA_CLA_LA))
  }
    
  ## ********* MERGE CLA data frames together *****************# 
  
    for (i in seq_along(list.cla)) {
      print(names(list.cla)[[i]])
      print(dim(list.cla[[i]]))
      print(dim(list.cla2[[i]]))
      print(dim(list.cla2[[i]])[1]/dim(list.cla[[i]])[1])
    }

  # harmonise colnames
    list.cla2 <- lapply(seq_along(list.cla2), function(x) {
     colnames(list.cla2[[x]]) <- gsub("_SPR1.*","",colnames(list.cla2[[x]]))
     colnames(list.cla2[[x]]) <- gsub("_SPR0.*","",colnames(list.cla2[[x]]))
     list.cla2[[x]]
    })
  
    all.cla <- Reduce(function(dtf1,dtf2) rbind.fill(dtf1,dtf2), list.cla2)
  
    # rename episode started
    all.cla <- rename.cols("CLA_DATE_EPI_COMM","date_episode_started",all.cla)
    
  # reduce dataset to demographics and relevant information only 
    cols.to.keep <- c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA","EVERFSM_6","EVERFSM_6_P", "EVERFSM_ALL",
                      "EYPPE","EYPPR","PLAA","EYPPeligible","date_episode_started","poc_start")
    all.cla <- all.cla[,which(colnames(all.cla) %in% cols.to.keep)]
    
# create FSM indicator. 
    # same procedure as for 02_a. more detailed description there. 
    # current FSM status: locf
    DT.cla <- data.table(all.cla)
    
    # pupil premium eligibility: EYPPE/EYPPR (for different years).
    DT.cla[EYPPE=="N",PPE:=0] 
    DT.cla[EYPPE=="Y",PPE:=1]
    DT.cla[PPE==0, PPE:=NA] 
    DT.cla[EYPPE==""|is.na(EYPPE),PPE:=NA]
    DT.cla[order(date_episode_started),PPE.locf:=na.locf(PPE, na.rm=FALSE), by=c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA")]
    DT.cla[EYPPE=="N" & is.na(PPE.locf), PPE.locf:=0]  
    
    DT.cla[EYPPeligible=="N",P.elig:=0] 
    DT.cla[EYPPeligible=="Y",P.elig:=1]
    DT.cla[P.elig==0, P.elig:=NA] 
    DT.cla[EYPPeligible==""|is.na(EYPPeligible),P.elig:=NA]
    DT.cla[order(date_episode_started),P.elig.locf:=na.locf(P.elig, na.rm=FALSE), by=c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA")]
    DT.cla[EYPPeligible=="N" & is.na(P.elig.locf), P.elig.locf:=0]
    
    # consolidate
    relev.cols <- which(colnames(DT.cla) %in% c("EVERFSM_ALL","EVERFSM_6","EVERFSM_6_P","PPE.locf","P.elig.locf"))
    DT.cla[,lowinc:=rowSums(.SD, na.rm=TRUE), .SDcols=relev.cols]
    DT.cla[lowinc>=1,lowinc:=1]
    # replace lowinc as missing if all our indicators are NA
    DT.cla[,rows.na:=rowSums(is.na(.SD)), .SDcols=relev.cols]
    DT.cla[rows.na==5, lowinc:=NA]
    
    DT.cla <- DT.cla[,-c("EVERFSM_ALL","EVERFSM_6","EVERFSM_6_P","PPE.locf","P.elig.locf","rows.na")]

    DT.cla$lowinc[is.na(DT.cla$lowinc)] <- "Missing"
    
    # collapse dataset:
    cols.to.keep.2 <- c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA","lowinc","date_episode_started")
    cla.FSM <- as.data.frame(DT.cla[,..cols.to.keep.2])
    cla.FSM <- cla.FSM[!duplicated(cla.FSM),]
    
      # identify cases where lowinc still differs
      cla.FSM <- data.table(cla.FSM)
      recode.missing <- setdiff(cla.FSM[duplicated(cla.FSM[,c("CLA_CLA_LA","CLA_CHILD_LA_CODE_Anon","date_episode_started")]),c("CLA_CHILD_LA_CODE_Anon","date_episode_started")],
                                cla.FSM[duplicated(cla.FSM[,c("CLA_CLA_LA","CLA_CHILD_LA_CODE_Anon","date_episode_started","lowinc")]),c("CLA_CHILD_LA_CODE_Anon","date_episode_started")])
    
      # subset data to look at duplicates only
      cla.remove.missing <- merge(recode.missing, cla.FSM, by=c("CLA_CHILD_LA_CODE_Anon","date_episode_started"), all.x=TRUE) 
      cla.remove.missing <- cla.remove.missing[!cla.remove.missing$lowinc=="Missing",]
    
      # identify cases with differing information on lowinc
      no.alternative <- cla.remove.missing[duplicated(cla.remove.missing[,c("CLA_CLA_LA","CLA_CHILD_LA_CODE_Anon","date_episode_started")]),] 
    
      cla.replace.missing <- setdiff(cla.remove.missing[,c("CLA_CLA_LA","CLA_CHILD_LA_CODE_Anon","date_episode_started")], no.alternative[,c("CLA_CLA_LA","CLA_CHILD_LA_CODE_Anon","date_episode_started")])
      cla.replace.missing <- merge(cla.replace.missing, cla.remove.missing, by=colnames(cla.replace.missing), all.x=TRUE)
    
      cla.FSM <- merge(cla.FSM, cla.replace.missing, by=c("CLA_CLA_LA","CLA_CHILD_LA_CODE_Anon","date_episode_started"), all.x=TRUE)
      cla.FSM <- data.table(cla.FSM)
      cla.FSM[, lowinc:=lowinc.y]
    
      cla.FSM[is.na(lowinc), lowinc:=lowinc.x]
      cla.FSM <- cla.FSM[,-c("lowinc.x","lowinc.y")]
      cla.FSM <- cla.FSM[!duplicated(cla.FSM),]  
    
    # identify any cases where lowing still differs and recode as missing.
    cla.FSM[,no.inc:=uniqueN(lowinc), by=c("CLA_CLA_LA","CLA_CHILD_LA_CODE_Anon","date_episode_started")]  
    cla.FSM[no.inc>1, lowinc:="Missing"]
    cla.FSM <- as.data.frame(cla.FSM)
    cla.FSM <- cla.FSM[!duplicated(cla.FSM),]
    cla.FSM<- cla.FSM[,-which(colnames(cla.FSM) %in% c("no.inc"))]

    stopifnot(sum(duplicated(cla.FSM[,c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA","date_episode_started")]))==0)
    save(cla.FSM, "cla.FSM",file = "Working/Intermediate_output/CLA_FSM.Rdata")
    
# add disability demographics
  # Take from 02_b_consolidate demographics which includes the disability information 
  # on all children of interest that are on CIN plans. 
  load("xx/Intermediate_output/demographics.Rdata")
  
  length(intersect(unique(cla.FSM$CLA_CHILD_LA_CODE_Anon), unique(demographics$CIN_LAchildID_Anon)))
  length(setdiff(unique(cla.FSM$CLA_CHILD_LA_CODE_Anon), unique(demographics$CIN_LAchildID_Anon)))

  cla.children <- cla.FSM[,c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA")] 
  cla.children <- cla.children[!duplicated(cla.children),]
  cla.dem <- merge(cla.children, demographics, by.x=c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA"), by.y=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)
  cla.dem <- data.table(cla.dem) 

  cla.dem <- cla.dem[,c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA","disabled")]

## consolidate with overall demographics information 
  disabilities <- read.csv("xx/CIN_Disabilities_2009_2019.csv")
  sum(duplicated(disabilities[,c("CIN_LAchildID_Anon")]))
  
  # consolidate disabilites
    disabilities <- data.table(disabilities)
    disabilities[CIN_Disability=="none"|CIN_Disability=="None"|
                   CIN_Disability=="NONE"|CIN_Disability=="", disabled:=0]
    disabilities$disabled[is.na(disabilities$disabled)] <- 1 # we're interpreting the random codes as a disability
    
  # clean disabilities
    disabilities <- disabilities[,c("CIN_LAchildID_Anon","disabled")]
    disabilities <- aggregate(disabilities$disabled, by=list(disabilities$CIN_LAchildID_Anon), FUN=max)
    colnames(disabilities) <- c("CIN_LAchildID_Anon","disabled")
    stopifnot(sum(duplicated(disabilities[,c("CIN_LAchildID_Anon")]))==0)
    
  # merge with other demographics
    cla.dem <- merge(cla.dem, disabilities, by.x=c("CLA_CHILD_LA_CODE_Anon"), by.y=c("CIN_LAchildID_Anon"), all.x=TRUE)
    
    cla.dem$disabled[cla.dem$disabled.x==1|cla.dem$disabled.y==1] <- 1
    cla.dem$disabled[(cla.dem$disabled.x==0|cla.dem$disabled.y==0) & is.na(cla.dem$disabled)] <- 0

    cla.dem <- cla.dem[,-c("disabled.x","disabled.y")]
    cla.dem <- cla.dem[!duplicated(cla.dem),] 
    
    save(cla.dem, "cla.dem", file="Working/Intermediate_output/CLA_demographics.Rdata")
    