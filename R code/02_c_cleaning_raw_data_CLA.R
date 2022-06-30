
#************************** CLA initial cleaning ********************************************# 
# Description: 
#   Format CLA data 
#     0. reformat data to the right data types (e.g. date, numeric, etc.)
#     1. recode missing to NA
#     2. consolidate demographics --> if e.g. DOB is missing in first plan but available in second use this information
#     3. consolidate plans


#************************** CLA initial cleaning ********************************************# 


rm(list=ls())
setwd("P:/")
Sys.setenv(R_HISTFILE="P://Working")

source("./Working/00_Package_install.R")

  # read in data
  CLA.raw <- read.csv("./xx/Part 3 CLA Data/Episode_final_CLA.csv")

  load("Working/Intermediate_output/CIN_formatted.Rdata")
  length(setdiff(unique(CLA.raw$CHILD_LA_CODE_ANON), unique(cin.formatted$CIN_LAchildID_Anon)))
  length(setdiff(unique(CLA.raw$CHILD_ID_ANON), unique(cin.formatted$CIN_LAchildID_Anon))) 
  length(setdiff(unique(CLA.raw$CLA_PupilMatchingRefAnonymous), unique(cin.formatted$CIN_PupilMatchingRefAnonymous)))

    
# ******** limit list to LAs from our pilot and comparator LAs only  ********
    load("P:/Working/Intermediate_output/all_LAs_codes.Rdata") # matches & codes
  
    stopifnot(setdiff(matches.codes$LA.code, CLA.raw$LA)==character(0))
    CLA.limited <- CLA.raw[which(CLA.raw$LA %in% matches.codes$LA.code),]
  
  # remove anyone who is not in care under an interim or full care order. 
    CLA.limited <- CLA.limited[which(CLA.limited$legal_status=="C1"|CLA.limited$legal_status=="C2"),] 
  
# *** ADDRESS DATA ISSUES 
    
  # remove false entries
    CLA.limited$date_episode_started <- as.Date(CLA.limited$date_episode_started) 
    CLA.limited$date_episode_ceased <- as.Date(CLA.limited$date_episode_ceased) 
    CLA.limited$poc_start <- as.Date(CLA.limited$poc_start) 
    
  # PERIOD of CARE LENGTH:  
    hist(CLA.limited$poc_length)
    hist(CLA.limited$poc_length[CLA.limited$poc_length<=100], 
         col='darkmagenta',
         breaks=100)    
    hist(CLA.limited$poc_length[CLA.limited$poc_length<=100], 
         col='darkmagenta',
         breaks=100)
    hist(CLA.limited$poc_length[CLA.limited$poc_length<=30], 
        col='darkmagenta',
         breaks=30)
 
  # PERIOD OF CARE NUMBER: 
    # check how many times children have a CLA period.
    CLA.dt <- data.table(CLA.limited)
      CLA.plan <- CLA.dt[,c("CHILD_LA_CODE_ANON","poc_start","LA","date_episode_started")]
      CLA.plan <- CLA.plan[!duplicated(CLA.plan),]
      CLA.plan <- CLA.plan[order(CLA.plan$CHILD_LA_CODE_ANON, CLA.plan$poc_start, CLA.plan$date_episode_started),
                         episode.no :=seq_len(.N), by=c("CHILD_LA_CODE_ANON","LA","poc_start")] 
      CLA.dt <- merge(CLA.dt, CLA.plan, by=c("CHILD_LA_CODE_ANON","LA","poc_start","date_episode_started"), all.x=TRUE)
    
    # create indicator for number of periods of care:
      CLA.subset <- CLA.dt[,c("CHILD_LA_CODE_ANON","poc_start","LA")]
      CLA.subset <- CLA.subset[!duplicated(CLA.subset),]
      CLA.subset <- CLA.subset[order(CLA.subset$CHILD_LA_CODE_ANON, CLA.subset$poc_start),] 
      CLA.subset[,occur:=seq_len(.N), by=c("CHILD_LA_CODE_ANON","LA")]
      CLA.subset[,no.periods:=max(occur), by=c("CHILD_LA_CODE_ANON","poc_start")]
    
      CLA.dt <- merge(CLA.dt, CLA.subset, by=c("CHILD_LA_CODE_ANON","LA","poc_start"), all.x=TRUE)
    
    # look at potential reasons for the vast amount of POCs for some children:  
      table(CLA.dt$legal_status)
      table(CLA.dt$legal_status[CLA.dt$no.periods>10]) 

  CLA.working <- as.data.frame(CLA.dt)
  
  # reformat date variables
    date.vars <- c("poc_start","date_episode_started","date_episode_ceased")
    CLA.working[,which(colnames(CLA.working) %in% date.vars)] <- lapply(CLA.working[,which(colnames(CLA.working) %in% date.vars)], as.Date)
  
  # check for duplicates
    sum(duplicated(CLA.working[,c("poc_start","date_episode_started","CHILD_LA_CODE_ANON","LA")]))

  # remove any period of care that started prior to year 2008/09
    CLA.working <- CLA.working[which(CLA.working$poc_start>="2008-04-01"),]
  
  # FORMAT VARIABLES & CREATE NEW ONES
    # Category of need is under "cin". 
      table(CLA.working$cin) 
      CLA.working$main.need <- as.character(CLA.working$cin)
  
    # gender: 
      CLA.working$gender <- as.numeric(CLA.working$SEX)
    
    # Ethnicity: recode to major groups
      CLA.working$ethnic_or <- as.character(CLA.working$ethnic_or)
      CLA.working$"Ethnicity" <- CLA.working$ethnic_or
      CLA.working$"Ethnicity"[grep("^W",CLA.working$ethnic_or)] <- "WHIT"
      CLA.working$"Ethnicity"[grep("^B",CLA.working$ethnic_or)] <- "BLAC"
      CLA.working$"Ethnicity"[grep("^A",CLA.working$ethnic_or)] <- "ASIA"
      CLA.working$"Ethnicity"[grep("^M",CLA.working$ethnic_or)] <- "MIXD"
      CLA.working$"Ethnicity"[grep("^CH",CLA.working$ethnic_or)] <- "CHIN"
      CLA.working$"Ethnicity"[CLA.working$ethnic_or=="OOTH"] <- "AOEG"
      CLA.working$"Ethnicity"[CLA.working$ethnic_or=="NOBT"|CLA.working$ethnic_or=="REFU"] <- "UNCL"
      CLA.working$"Ethnicity"[is.na(CLA.working$ethnic_or)|CLA.working$ethnic_or==""|CLA.working$ethnic_or=="NA"] <- "Missing"
  
    # age at period of care start. 
      CLA.working$DOB.proxy <- as.Date(paste(CLA.working$dob_Year, CLA.working$dob_Month,01,sep="-"))
      CLA.working$age.cla <- floor(as.numeric(difftime(CLA.working$poc_start, CLA.working$DOB.proxy,units = "days"))/365.25)
    
    # create age at episode start date too so we can exclude any episodes where the person was 18 or over 
      CLA.working$age.episode <- floor(as.numeric(difftime(CLA.working$date_episode_started, CLA.working$DOB.proxy,units = "days"))/365.25)
      CLA.working <- CLA.working[which((CLA.working$age.cla<=17) & (CLA.working$age.episode<=17)),]
    
    # create relevant year
      CLA.working$relev.year <- year(CLA.working$poc_start)-(month(CLA.working$poc_start)<=3)
    
    # create academic year
      # age 5-11: primary school
      # age 12-17: secondary school
      CLA.working$ac.year[CLA.working$age.cla>=12] <- "SEC"
      CLA.working$ac.year[CLA.working$age.cla>=5 & CLA.working$age.cla<12] <- "PRIM"
      CLA.working$ac.year[CLA.working$age.cla<5] <- "NONE"
      CLA.working$ac.year[is.na(CLA.working$age.cla)] <- "MISSING"
    
    # asylum seeking: 
      CLA.dt <- data.table(CLA.working)
      CLA.dt[, asylum:=max(uasc_status, na.rm=TRUE), by=c("CHILD_LA_CODE_ANON","LA")]
      CLA.dt[is.infinite(asylum),asylum:=NA]
    
    # reformat into data frame  
    CLA.working <- as.data.frame(CLA.dt)
    
    # add in disabilities
      load("Working/Intermediate_output/CLA_demographics.Rdata") 
      length(setdiff(CLA.working$"CHILD_LA_CODE_ANON", cla.dem$CLA_CHILD_LA_CODE_Anon))
      CLA.consolidated <- merge(CLA.working, cla.dem, by.x=c("CHILD_LA_CODE_ANON","LA"), by.y=c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA"), all.x=TRUE)

    # add in FSM:
      load("Working/Intermediate_output/CLA_FSM.Rdata")
      CLA.consolidated <- merge(CLA.consolidated, cla.FSM, by.x=c("CHILD_LA_CODE_ANON","LA","date_episode_started"), 
                                by.y=c("CLA_CHILD_LA_CODE_Anon","CLA_CLA_LA","date_episode_started"), all.x=TRUE)
                                
      # recode low income to 99 if child is younger than 5 and thus expected not to have this indicator. 
      CLA.consolidated$lowinc[is.na(CLA.consolidated$lowinc)] <- "Missing"
      CLA.consolidated$lowinc[CLA.consolidated$age.cla<5 & CLA.consolidated$lowinc=="Missing"] <- 99
      

# ** remove observations within the embedding period.
    
    ## read in embedding period data 
      load("P:/Working/Intermediate_output/parameters.Rdata") # matches.final information on matches & embedding periods
      
      stopifnot(matches.final[["Kinship Care"]]$LA.code %in% unique(CLA.working$LA))
      CLA.pop <- merge(CLA.consolidated, matches.final[["Kinship Care"]], by.x="LA",by.y="LA.code",all.y=TRUE) 
      
    # Include observations if: 
      # we include observations if they are EITHER OR
      # PRE = poc_start + months(12) < embedding.start
      # POST = (poc_start > embedding.end) & (poc_start < end of data available - 12 months)
      dim(CLA.pop) 
      CLA.pop <- CLA.pop[which((CLA.pop$poc_start>CLA.pop$Embedding.end)|(CLA.pop$poc_start+days(365)<CLA.pop$Embedding.start)),] 
      dim(CLA.pop) 
      CLA.pop <- CLA.pop[which(CLA.pop$poc_start+days(365) < "2019-03-31"),]
      dim(CLA.pop) # 
      # same rules applied to period of are start date apply to episode start date 
      CLA.pop <- CLA.pop[which((CLA.pop$date_episode_started>CLA.pop$Embedding.end)|(CLA.pop$date_episode_started<CLA.pop$Embedding.start)),]
      dim(CLA.pop) 
      CLA.pop <- CLA.pop[which(CLA.pop$date_episode_started<=CLA.pop$poc_start+days(365)),]
      dim(CLA.pop)

    # create post embed and DiD dummies
      CLA.pop$postembed <- ifelse(CLA.pop$poc_start>CLA.pop$Embedding.end, 1, 0)
      CLA.pop$DiD <- ifelse(CLA.pop$postembed==1 & CLA.pop$SofS.LA==1, 1, 0)
    
    # check for distribution
      table(CLA.pop$postembed, CLA.pop$LA, useNA="always")
      # exclude any LAs where embedding end is 2018-01-01, due to lack of post embedding obs forthis LA
      CLA.pop <- CLA.pop[-which(CLA.pop$Embedding.end>="2018-01-01"),]

      
# ***CREATE OUTCOME VARIABLE --> KINSHIP CARE during period of care? 
      
    # create indicator for cases where
    #   (a) the episode stretches over more than a year since the period start date 
    #   If that is the case, we only consider whether the child was placed in kinship care during this episode
    #   and do NOT consider whether the episode ceased because the child went into kinship care in the 
    #   form of a Special Guardianship Order (SGO)
      CLA.pop$placement.only <- ifelse(CLA.pop$date_episode_ceased>CLA.pop$poc_start+days(365),1,0)
    
      
    # count an episode as kinship care IF (in line with CLA team advice)
    #   - Placements in kinship care:
    #     - placement = U1-U3/Q1: a child is in a foster care placement with friends or relatives
    #     - placement: F1/F4 --> same as U1-U3 according to CLA stats team. 
    #     - placement = P1: child placed with parents --> we consider this kinship care as well.
    #     * note that we do NOT include REC= returned home as kinship care, but just consider SGOs there.
    #   - Ceased care due to a special guardianship order: 
    #     - REC = E45 & E47 (made through relative or friend)
    #     - include child arrangement orders as part of the care plan (E4A) & E4 since this is the pre-2014 code. 
    # private fostering arrangements are not included.
    # according to CLA stats team, E45 was previously combined with E46 under E43. Same goes for 
    # E47 & E48 under E44. Since the share of E46 and E48 respectively is low, include E43 & E44 in population
      CLA.pop <- data.table(CLA.pop)
      CLA.pop[placement=="Q1"|placement=="U1"|placement=="U2"|placement=="U3"|placement=="P1"|
               placement=="F1"|placement=="F4", kinship.placement:=1]
      CLA.pop[reason_episode_ceased=="E45"|reason_episode_ceased=="E47"|reason_episode_ceased=="E13"|
                reason_episode_ceased=="E4A"|reason_episode_ceased=="E4"|
                reason_episode_ceased=="E43"|reason_episode_ceased=="E44",
              kinship.SGO:=1]
    
    # create indicator taking into account placement.only=1/0
      CLA.pop[kinship.placement==1, kinship.care.episode:=1]
      CLA.pop[placement.only==0 & kinship.SGO==1, kinship.care.episode:=1] 
      CLA.pop[is.na(kinship.care.episode), kinship.care.episode:=0]
      
      # create indicator per period of care by aggregating kinship care
      # consolidate indicator per period of care as we only consider entire periods.
      # We do NOT consider the subsequent period of care, if two periods of care occur within a year
      CLA.pop[,kinship.care:=max(kinship.care.episode), by=c("CHILD_LA_CODE_ANON","LA","poc_start")]
    
      
  # ** Consolidate episodes of care by period of care

    # 1.) create a df listing all PERIODS of care only.  
      CLA.periods <- as.data.frame(CLA.pop)
      CLA.periods <- CLA.periods[,c("LA","CHILD_LA_CODE_ANON","gender","poc_start","Ethnicity",
                                    "age.cla","relev.year","Local.authority","Matched.group","postembed","DiD","cat.abuse",
                                 "SofS.LA","main.need","lowinc","disabled","ac.year","asylum","elev.risk","episode.no","kinship.care")] 

      # keep only the first episode of each period of care. All covariates have been consolidated except for 
      # main.need, where we want the information available at the START of the period of care 
      CLA.periods <- CLA.periods[CLA.periods$episode.no==1,]
      CLA.periods <- CLA.periods[,-which(colnames(CLA.periods) %in% c("episode.no"))]
      CLA.periods <- CLA.periods[!duplicated(CLA.periods),]
    
      stopifnot(sum(duplicated(CLA.periods[,c("LA","CHILD_LA_CODE_ANON","poc_start","Matched.group")]))==0)


      RQ5.kinship <- as.data.frame(CLA.periods)
      stopifnot(sum(is.na(RQ5.kinship$kinship.care))==0)
    
    
# **** add in remaining covars: primary need code, # of previous CPPs:
    
    # 1. identify relevant referral before poc_start
      load("Working/Intermediate_output/all_referrals.Rdata") # comes from information_merge2pop file. 
    
      length(intersect(unique(RQ5.kinship$CHILD_LA_CODE_ANON),unique(refs$CIN_LAchildID_Anon)))
      length(setdiff(unique(RQ5.kinship$CHILD_LA_CODE_ANON),unique(refs$CIN_LAchildID_Anon)))

      # list all periods of care
        pocs <- RQ5.kinship[,c("LA","CHILD_LA_CODE_ANON","poc_start","Matched.group")]
      
      # merge with information on all referrals
        pocs <- merge(pocs, refs, by.x=c("CHILD_LA_CODE_ANON","LA"), by.y=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)
    
      # exclude any referral that happen after poc_start
        cols.ref <- grep("refs\\.", colnames(pocs))
        pocs <- data.table(pocs)
        pocs[, (cols.ref):=replace(.SD, .SD>.(poc_start),NA), .SDcols=cols.ref]
    
      # take the maximum i.e. most recent referral date of all referrals left
        pocs <- as.data.frame(pocs)
        pocs$latest.referral <- apply(pocs[,cols.ref], 1, function(x) max(x, na.rm=TRUE))
        pocs$latest.referral <- as.Date(pocs$latest.referral)
    
      # remove ref.columns
        pocs <- pocs[,-grep("refs", colnames(pocs))]
    
      # find information for relevant referral date in cin_formatted 
        cin.info <- cin.formatted[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate","cat.abuse",
                                 "CIN_NumberOfPreviousCPP")]
    
        # keep cin information for relevant children in poc only. (increases speed of calculations)
        cin.info <- merge(cin.info, pocs[,c("CHILD_LA_CODE_ANON","LA","poc_start","latest.referral")],
                          by.x=c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate"), 
                          by.y=c("LA","CHILD_LA_CODE_ANON","latest.referral")) 
        cin.info <- cin.info[!is.na(cin.info$poc_start),-which(colnames(cin.info) %in% "poc_start")]
        cin.info <- cin.info[!duplicated(cin.info),]
          
      # check remaining duplicates
        sum(duplicated(cin.info[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate")]))
        sum(duplicated(cin.info[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate","cat.abuse")]))
        sum(duplicated(cin.info[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate","CIN_NumberOfPreviousCPP")]))
    
      # consolidate previous CPP info. 
      cin.dt <- data.table(cin.info)
      cin.dt[,previous.CPP:=max(CIN_NumberOfPreviousCPP, na.rm=TRUE), by=c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate")]
      cin.dt[is.infinite(previous.CPP),previous.CPP:=NA]
    
      cin.dt <- cin.dt[,-"CIN_NumberOfPreviousCPP"]
      cin.dt <- cin.dt[!duplicated(cin.dt),]
    
    # recode cat.abuse as missing if two different entries 
        cin.dt <- as.data.frame(cin.dt)
        
        # identify any cases where cat.abuse still differs.
        recode.missing <- setdiff(cin.dt[duplicated(cin.dt[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate")]),c("CIN_LAchildID_Anon","CIN_ReferralDate")],
                                  cin.dt[duplicated(cin.dt[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate","cat.abuse")]),c("CIN_LAchildID_Anon","CIN_ReferralDate")])
        
        # subset data to look at duplicates only
        cin.remove.missing <- merge(recode.missing, cin.dt, by=c("CIN_LAchildID_Anon","CIN_ReferralDate"), all.x=TRUE) 

        # remove the duplicate entries where cat.abuse is Missing.
        cin.remove.missing <- cin.remove.missing[!cin.remove.missing$cat.abuse=="Missing",]
        no.alternative <- cin.remove.missing[duplicated(cin.remove.missing[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","previous.CPP")]),] 

        cin.replace.missing <- setdiff(cin.remove.missing[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate","previous.CPP")], no.alternative[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate","previous.CPP")])
        cin.replace.missing <- merge(cin.replace.missing, cin.remove.missing, by=colnames(cin.replace.missing), all.x=TRUE)
        
        cin.dt <- merge(cin.dt, cin.replace.missing, by=c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate","previous.CPP"), all.x=TRUE)
        cin.dt <- data.table(cin.dt)
        cin.dt[, cat.abuse:=cat.abuse.y]
        
        cin.dt[is.na(cat.abuse), cat.abuse:=cat.abuse.x]
        cin.dt <- cin.dt[,-c("cat.abuse.x","cat.abuse.y")]
        cin.dt <- cin.dt[!duplicated(cin.dt),]  
      
      # identify any cases where cat.abuse still differs and recode as missing.
      cin.dt[,no.abuse:=uniqueN(cat.abuse), by=c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate")]  
      cin.dt[no.abuse>1, cat.abuse:="Missing"]
      cin.dt <- as.data.frame(cin.dt)
      cin.dt <- cin.dt[!duplicated(cin.dt),-which(colnames(cin.dt) %in% c("no.abuse"))]
  
    stopifnot(sum(duplicated(cin.dt[,c("CIN_LA","CIN_LAchildID_Anon","CIN_ReferralDate")]))==0)
    
    # merge with poc start date to then merge into RQ5 later.
      pocs.combined <- merge(pocs, cin.dt, by.x=c("CHILD_LA_CODE_ANON","LA","latest.referral"),
                             by.y=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate"), all.x=TRUE)
    
    # merge in with RQ5 to obtain full dataset. 
      RQ5.kinship <- merge(RQ5.kinship, pocs.combined, by=c("LA","CHILD_LA_CODE_ANON","poc_start","Matched.group"), all.x=TRUE)
    
      stopifnot(sum(RQ5.kinship$latest.referral>RQ5.kinship$poc_start, na.rm=TRUE)==0)
  
      RQ5.kinship.prelim <- RQ5.kinship

  save(RQ5.kinship.prelim, "RQ5.kinship.prelim", file="Working/Intermediate_output/RQ5_kinship_care_preliminary.Rdata")
    
