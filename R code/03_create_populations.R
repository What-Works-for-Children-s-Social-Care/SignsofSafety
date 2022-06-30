## creates the five different populations for each research question.

rm(list=ls())
setwd("P:/")
Sys.setenv(R_HISTFILE="P://Working")

source("./Working/00_Package_install.R")
source("./Working/00_functions.R")

load("Working/Intermediate_output/CIN_formatted.Rdata") # load cin data
load("./Working/Intermediate_output/covariates.Rdata") # load covariates
load("./Working/Intermediate_output/covars_at_referral.Rdata") # load covariates at referral stage for those that change over time. 
load("P:/Working/Intermediate_output/parameters.Rdata") # matches.final information on matches & embedding periods

# load scaled covars
load("./Working/Intermediate_output/covariates_scaled_1.Rdata")
load("./Working/Intermediate_output/covariates_scaled_2.Rdata")
load("./Working/Intermediate_output/covariates_scaled_3.Rdata")
load("./Working/Intermediate_output/covariates_scaled_4.Rdata")
load("./Working/Intermediate_output/covariates_scaled_5.Rdata")

  individ.covars <- c("disabled","asylum","lowinc","age.at","ac.year","gender","Ethnicity",
                      "main.need","cat.abuse","elev.risk","previous.CPP")

  LA.covars <- c("assessments", "ofsted.rating","pop.density","primary.fsm","secondary.fsm","innovation","percentage.WBRI","timescales.review")

  
#** create different datasets for different populations. 

# *** 1. Duration of assessment ***

  # load necessary information
  load("Working/Intermediate_output/demographics.Rdata") # demographics contains all the demographics
  load("Working/Intermediate_output/DOBs.Rdata") 
  load("Working/Intermediate_output/all_assessments.Rdata") # assessments.all
  
  # include only assessments which occurred in the pilot and comparator LAs 
  stopifnot(matches.final[[which(names(matches.final) %in% "Duration")]]$LA.code %in% unique(assessments.all$LA))
  RQ1.raw <- merge(assessments.all, matches.final[[which(names(matches.final) %in% "Duration")]], by.x="LA",by.y="LA.code",all.y=TRUE)
  
  # create relevant year: 
  RQ1.raw$relev.year <- year(RQ1.raw$Assessment.start.date)-(month(RQ1.raw$Assessment.start.date)<=3)
  
  # merge with indiviual level demographics: 
  # note that we cannot use main.need or latest category of abuse as these are not pre-assessment. 
  RQ1.dem <- merge(RQ1.raw, demographics, by.x=c("LAchildID_Anon","LA"), by.y=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)
  
  # merge with lowinc and previous cpp
  RQ1.dem <- merge(RQ1.dem, covar.referral[,c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate","lowinc","previous.CPP")], 
                   by.x=c("LAchildID_Anon","LA","latest.referral"), by.y=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate"), all.x=TRUE)
  
  # Remove observations that fall within the embedding period.  
  # exclude: only use obs w/ 
    # (a) assessment end date < embedding.start OR
    # (b) assessment start.date > embedding.end 
  RQ1.dem <- RQ1.dem[(RQ1.dem$Assessment.start.date > RQ1.dem$Embedding.end)|
                       (RQ1.dem$Assessment.end.date<RQ1.dem$Embedding.start),]
  RQ1.dem <- RQ1.dem[RQ1.dem$Assessment.start.date+days(53) < "2019-03-29",]
  
  # create age at assessment
  RQ1.dem <- merge(RQ1.dem, DOB, by.x=c("LAchildID_Anon","LA"), by.y=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)

   # create separate age at assessments
   RQ1.dem$age.1 <- floor(as.numeric(difftime(RQ1.dem$Assessment.start.date, RQ1.dem$CIN_DOB.1, units="days"))/365.25)
   RQ1.dem$age.2 <- floor(as.numeric(difftime(RQ1.dem$Assessment.start.date, RQ1.dem$CIN_DOB.2, units="days"))/365.25)
   RQ1.dem$age.3 <- floor(as.numeric(difftime(RQ1.dem$Assessment.start.date, RQ1.dem$CIN_DOB.3, units="days"))/365.25)
   RQ1.dem$age.4 <- floor(as.numeric(difftime(RQ1.dem$Assessment.start.date, RQ1.dem$CIN_DOB.4, units="days"))/365.25)
   
   # consolidate age at assessments
   RQ1.dem$age.assess <- ifelse(is.na(RQ1.dem$age.4) & is.na(RQ1.dem$age.3) & is.na(RQ1.dem$age.2),RQ1.dem$age.1,"ND")
   RQ1.dem$age.assess[which(is.na(RQ1.dem$age.4) & is.na(RQ1.dem$age.3) & (RQ1.dem$age.2==RQ1.dem$age.1))] <-  RQ1.dem$age.1[which(is.na(RQ1.dem$age.4) & is.na(RQ1.dem$age.3) & (RQ1.dem$age.2==RQ1.dem$age.1))]
   RQ1.dem$age.assess[which(is.na(RQ1.dem$age.4) & (RQ1.dem$age.2==RQ1.dem$age.1) & (RQ1.dem$age.1==RQ1.dem$age.3))] <- RQ1.dem$age.1[which(is.na(RQ1.dem$age.4) & (RQ1.dem$age.2==RQ1.dem$age.1) & (RQ1.dem$age.1==RQ1.dem$age.3))]
   RQ1.dem$age.assess[which((RQ1.dem$age.2==RQ1.dem$age.1) & (RQ1.dem$age.1==RQ1.dem$age.3) & (RQ1.dem$age.1==RQ1.dem$age.4))]<- RQ1.dem$age.1[which((RQ1.dem$age.2==RQ1.dem$age.1) & (RQ1.dem$age.1==RQ1.dem$age.3) & (RQ1.dem$age.1==RQ1.dem$age.4))]
   RQ1.dem$age.assess[RQ1.dem$age.assess=="ND"] <- NA
  
   RQ1.dem <- RQ1.dem[,-grep("CIN_DOB", colnames(RQ1.dem))]
   RQ1.dem <- RQ1.dem[,-which(colnames(RQ1.dem) %in% c("age.1","age.2","age.3","age.4"))]
   
   # create academic year
    # age 5-11: primary school
    # age 12-17: secondary school
    RQ1.dem$age.assess <-as.numeric(RQ1.dem$age.assess)
    RQ1.dem$ac.year[RQ1.dem$age.assess>=12] <- "SEC"
    RQ1.dem$ac.year[RQ1.dem$age.assess>=5 & RQ1.dem$age.assess<12] <- "PRIM"
    RQ1.dem$ac.year[RQ1.dem$age.assess<5] <- "NONE"
    RQ1.dem$ac.year[is.na(RQ1.dem$age.assess)] <- "MISSING"
   
    # remove any children aged below -1 or above 17 at assessment
    RQ1.dt <- data.table(RQ1.dem)
    RQ1.dt <- RQ1.dt[(RQ1.dt$age.assess>=-1 & RQ1.dt$age.assess<=17) | is.na(RQ1.dt$age.assess),]

  # create post embed and DiD dummies
    RQ1.dt$postembed <- ifelse(RQ1.dt$Assessment.start.date>RQ1.dt$Embedding.end, 1, 0)
    RQ1.dt$DiD <- ifelse(RQ1.dt$postembed==1 & RQ1.dt$SofS.LA==1, 1, 0)
  
  # create years since implementation indicator
    RQ1.dt[postembed==1, years.since:=floor(difftime(Assessment.start.date, Embedding.end, units = "days")/365.25)]
    RQ1.dt[postembed==0, years.since:=floor(difftime(Assessment.start.date, Embedding.start, units = "days")/365.25)]
 
 # create outcome
    RQ1.dt$"Duration" <- as.numeric(difftime(RQ1.dt$Assessment.end.date, RQ1.dt$Assessment.start.date, units="days"))
    
    #create outcome - duration of assessment in workdays (!) : 
      # need to exclude bank holidays & weekends. 
      BH <- read_excel(path="Working/Intermediate_output/Bankholidays_UK_modified.xlsx", col_names=TRUE, sheet="Bank_holiday")
      BH$`Bank holidays` <- as.Date(BH$`Bank holidays`)
    
      # build function
      Nweekdays <- Vectorize(function(a,b)
       sum(!((weekdays(seq(a, b, "days")) %in% c("Saturday","Sunday"))|(seq(a, b, "days") %in% BH$`Bank holidays`)))-1) # 
                                                                                 
    # calculate duration outcome. 
    RQ1.dt$Duration <- Nweekdays(RQ1.dt$Assessment.start.date, RQ1.dt$Assessment.end.date)
    RQ1.dt$Duration <- ifelse(weekdays(RQ1.dt$Assessment.end.date)  %in% c("Saturday","Sunday") |
                                (RQ1.dt$Assessment.end.date %in% BH$`Bank holidays`), RQ1.dt$Duration+1, RQ1.dt$Duration) 
    
    # save intermediate output for different population/cutoff in sensitivity analysis later
      save(RQ1.dt, "RQ1.dt", file="./Working/Intermediate_output/DurationofAssessment_pre-cutoff.Rdata")
      
    # look at outliers in terms of duration length: 
     hist(RQ1.dt$Duration, breaks=100)
     hist(RQ1.dt$Duration[RQ1.dt$Duration>=100], 
          col='darkmagenta',
          breaks=200)
     hist(RQ1.dt$Duration[RQ1.dt$Duration>=200], 
          col='darkmagenta',
          breaks=200)
     hist(RQ1.dt$Duration[RQ1.dt$Duration>=300], 
          col='darkmagenta',
          breaks=200)
     
     # Take general cutoff rule for outliers to avoid administrative errors:
     #  INTERQUARTILE DEVIATION METHOD: Outliers>1.5*IQR+Q3

      median.dur <- median(RQ1.dt$Duration)
      Q3.dur <- as.numeric(quantile(RQ1.dt$Duration, 0.75))
      IQR.dur <- IQR(RQ1.dt$Duration)
      outlier.upper <- Q3.dur+1.5*IQR.dur # 110
      hist(RQ1.dt$Duration[RQ1.dt$Duration<=200], 
           col='darkmagenta',
           breaks=200)
      
    RQ1.dt <- RQ1.dt[RQ1.dt$Duration<=outlier.upper,]

  # add LA level covariates
  stopifnot(unique(RQ1.dt$Local.authority) %in% covars.1$Local.authority )
  RQ1.assessment <- merge(RQ1.dt, covars.1, by.x=c("Local.authority","relev.year"), by.y=c("Local.authority","year"), all.x=TRUE)
  
  # change colnames
    RQ1.assessment <- rename.cols("LAchildID_Anon","child.ID",RQ1.assessment)
    RQ1.assessment <- rename.cols("age.assess","age.at",RQ1.assessment)
    RQ1.assessment <- rename.cols("CIN_NumberOfPreviousCPP","previous.CPP",RQ1.assessment)
    
  # final. checks
    dim(RQ1.assessment)
    RQ1.assessment <- anti_join(RQ1.assessment, RQ1.assessment[duplicated(RQ1.assessment[,c("child.ID","Matched.group","Assessment.start.date")]) | duplicated(RQ1.assessment[,c("child.ID","Matched.group","Assessment.start.date")], fromLast = TRUE),])
    dim(RQ1.assessment)

    stopifnot(sum(duplicated(RQ1.assessment[,c("child.ID","LA","Matched.group","Assessment.start.date")]))==0) 
    stopifnot(c("postembed","DiD","Matched.group") %in% colnames(RQ1.assessment))
    stopifnot(individ.covars %in% colnames(RQ1.assessment))
    setdiff(individ.covars, colnames(RQ1.assessment)) 
    table(RQ1.assessment$LA, RQ1.assessment$postembed) 
    table(RQ1.assessment$LA, RQ1.assessment$Matched.group) 
    
    to.keep <- c("postembed","DiD","SofS.LA","Matched.group","Duration","relev.year","years.since",
                 individ.covars, LA.covars,"LA","child.ID", "previous.CPP","Assessment.start.date") 
    RQ1.assessment <- as.data.frame(RQ1.assessment)
    RQ1.assessment <- RQ1.assessment[,which(colnames(RQ1.assessment) %in% to.keep)]
    
  save(RQ1.assessment, "RQ1.assessment", file="Working/baseline populations/RQ1_assessments.Rdata")
  

# 2. Assessed & Open cases that progress to an ICPC
    # load relevant information on ICPCs
    load("Working/Intermediate_output/ICPCs.Rdata") # contains all ICPCs of any child in cin.formatted
    
    # create raw data
    RQ2.raw <- merge(cin.formatted[,-which(colnames(cin.formatted) %in% c("lowinc","CIN_NumberOfPreviousCPP","main.need"))], 
                     matches.final[[which(names(matches.final) %in% "ICPC")]], 
                     by.x="CIN_LA",by.y="LA.code",all.y=TRUE) 
    
    RQ2.raw <- merge(RQ2.raw, covar.referral, by=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate"), all.x=TRUE)
  
    # (a) exclude observations within the embedding period --> 
      # CIN_ReferralDate + 3 months < embedding.start OR
      # CIN_ReferralDate > embedding.end
      RQ2.embed <- RQ2.raw[RQ2.raw$CIN_ReferralDate > RQ2.raw$Embedding.end |
                            RQ2.raw$CIN_ReferralDate+days(30*3) < RQ2.raw$Embedding.start,]
    
      # exclude entries if closure date is before referral date
      sum(RQ2.embed$CIN_ReferralDate>RQ2.embed$CIN_CINClosureDate, na.rm=TRUE)
      dim(RQ2.embed)
      RQ2.embed <- RQ2.embed[!(RQ2.embed$CIN_ReferralDate>RQ2.embed$CIN_CINClosureDate & !is.na(RQ2.embed$CIN_CINClosureDate)),] 
      dim(RQ2.embed)

      # create post embed and DiD dummies
      RQ2.embed$postembed <- ifelse(RQ2.embed$CIN_ReferralDate>RQ2.embed$Embedding.end, 1, 0)
      RQ2.embed$DiD <- ifelse(RQ2.embed$postembed==1 & RQ2.embed$SofS.LA==1, 1, 0)
    
      # create years since implementation indicator
      RQ2.embed$years.since <- ifelse(RQ2.embed$postembed==1, 
                                      floor(difftime(RQ2.embed$CIN_ReferralDate, RQ2.embed$Embedding.end, units = "days")/365.25),
                                      floor(difftime(RQ2.embed$CIN_ReferralDate, RQ2.embed$Embedding.start, units = "days")/365.25))
      
    # (b) create baseline population 
    # child was assessed and has an open case 
    RQ2.dt <- data.table(RQ2.embed)
    
    # calculate length CIN episode for checks
    RQ2.dt[,length.episode:=as.numeric(difftime(CIN_CINClosureDate, CIN_ReferralDate, units="days"))]
    
    # anyone with RC8 as closure reason is not CIN. As discussed with CIN team
    RQ2.dt$RC8 <- 0
    RQ2.dt[CIN_REC=="RC8", RC8:=1]
    RQ2.dt[,RC8:=min(RC8, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] # sometimes the assessment is NFA-ed because they are already CIN
    RQ2.dt[RC8==1, is.cin:=0]
      
      # anyone with REC= RC1 - RC7 is CIN 
      RQ2.dt[CIN_REC=="RC1"|CIN_REC=="RC2"|CIN_REC=="RC3"|
              CIN_REC=="RC4"|CIN_REC=="RC5"|CIN_REC=="RC6"|
              CIN_REC=="RC7", is.cin:=1]
      
      # unknown: 
      RQ2.dt[CIN_REC=="Unknown"|is.na(CIN_REC), is.cin:=99]
      

      # Deal with duplicate referrals
      RQ2.dt[,ref.NFA:=min(CIN_ReferralNFA, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] 
      RQ2.dt[,CIN_AnyPoint:=max(CIN_AnyPoint, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)]
      RQ2.dt[,CIN_Ended:=max(CIN_Ended, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] 
      RQ2.dt[,CIN_Started:=max(CIN_Started, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] 
      
      # remove anyone who has referral NFA==1: (as suggested by CIN team)
      RQ2.dt[RQ2.dt$ref.NFA==1,is.cin:=0]
      RQ2.dt[is.cin!=1 & CIN_CINAt31March==1, is.cin:=1] 
      table(RQ2.dt$is.cin, year(RQ2.dt$CIN_ReferralDate))
      RQ2.dt[is.cin!=1 & CIN_AnyPoint==1, is.cin:=1] 
      table(RQ2.dt$is.cin, year(RQ2.dt$CIN_ReferralDate))
      RQ2.dt[is.cin!=1 & CIN_Started==1, is.cin:=1] 
      RQ2.dt[RQ2.dt$is.cin!=1 & (RQ2.dt$"CIN_CPPstartDate" < RQ2.dt$"CIN_CINClosureDate") & (RQ2.dt$"CIN_CPPstartDate" > RQ2.dt$"CIN_ReferralDate"),is.cin:=1]
      table(RQ2.dt$is.cin, year(RQ2.dt$CIN_ReferralDate))
      RQ2.dt[RQ2.dt$is.cin!=1 & (RQ2.dt$"CIN_DateOfInitialCPC" < RQ2.dt$"CIN_CINClosureDate") & (RQ2.dt$"CIN_DateOfInitialCPC" > RQ2.dt$"CIN_ReferralDate"),is.cin:=1]
      table(RQ2.dt$is.cin, year(RQ2.dt$CIN_ReferralDate))

      # 
      table(RQ2.dt$is.cin)
      RQ2.dt[,is.cin:=min(is.cin, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] 
      table(RQ2.dt$is.cin)
      
      # keep only open cases. 
      RQ2.dt <- RQ2.dt[is.cin==1,]

      # (c) add LA level covars
      stopifnot(unique(RQ2.dt$Local.authority) %in% covars.2$Local.authority )
      RQ2.covars <- merge(RQ2.dt, covars.2, by.x=c("Local.authority","relev.year"), by.y=c("Local.authority","year"), all.x=TRUE)
      
      
      # (d) add outcome: ICPC
      RQ2.ICPC <- merge(RQ2.covars, ICPC, by=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)
      
        #### look at distribution of ICPCs 
        RQ2.ICPC <- data.table(RQ2.ICPC)
        RQ2.ICPC[ICPC.1<CIN_ReferralDate, ICPC.1:=NA]
        RQ2.ICPC[ICPC.2<CIN_ReferralDate, ICPC.2:=NA]
        RQ2.ICPC[ICPC.3<CIN_ReferralDate, ICPC.3:=NA]

      RQ2.ICPC <- data.table(RQ2.ICPC)
      RQ2.ICPC[ICPC.1>CIN_ReferralDate & ICPC.1<CIN_ReferralDate+days(30*3),ICPC:=1]
      RQ2.ICPC[ICPC.2>CIN_ReferralDate & ICPC.2<CIN_ReferralDate+days(30*3),ICPC:=1]
      RQ2.ICPC[ICPC.3>CIN_ReferralDate & ICPC.3<CIN_ReferralDate+days(30*3),ICPC:=1]
      RQ2.ICPC[is.na(ICPC),ICPC:=0]
      
        
    # tidy up df:
        RQ2.ICPC <- as.data.frame(RQ2.ICPC)
        RQ2.ICPC <- rename.cols("CIN_LA","LA", RQ2.ICPC)
        RQ2.ICPC <- rename.cols("CIN_LAchildID_Anon","child.ID", RQ2.ICPC)
        RQ2.ICPC <- rename.cols("age.ref","age.at", RQ2.ICPC)
        
        # final. checks
        stopifnot(c("postembed","DiD","Matched.group","SofS.LA","relev.year") %in% colnames(RQ2.ICPC))
        stopifnot(individ.covars %in% colnames(RQ2.ICPC))
        
        to.keep <- c("postembed","DiD","SofS.LA","Matched.group","ICPC","relev.year","years.since",
                     individ.covars, LA.covars,"LA","CIN_LA","child.ID", "previous.CPP","CIN_ReferralDate")
        
        RQ2.ICPC <- RQ2.ICPC[,which(colnames(RQ2.ICPC) %in% to.keep)]
        
        # remove bank holiday since probably not very relevant for open cases. 
        RQ2.ICPC <- RQ2.ICPC[,-which(colnames(RQ2.ICPC) %in% c("elev.risk"))]
        
        # remove cat.abuse since this is information at the CPP stage
        RQ2.ICPC <- RQ2.ICPC[,-which(colnames(RQ2.ICPC) %in% "cat.abuse")]
        
        # remove duplicates
        RQ2.ICPC <- RQ2.ICPC[!duplicated(RQ2.ICPC),]
        
        stopifnot(sum(duplicated(RQ2.ICPC[,c("child.ID","LA","CIN_ReferralDate","Matched.group")]))==0)

    # final check on numbers 
      table(RQ2.ICPC$LA, RQ2.ICPC$postembed) # make sure we have enough post embed obs
      table(RQ2.ICPC$LA, RQ2.ICPC$Matched.group) # make sure we have enough post embed obs
      table(RQ2.ICPC$ICPC, useNA="always") 
      
    save(RQ2.ICPC, "RQ2.ICPC", file="Working/baseline populations/RQ2_ICPC.Rdata")

# 3. Re-referrals within 6 months after NFA *****
    RQ3.raw <- cin.formatted 
    RQ3.raw <- data.table(RQ3.raw)
    
    # consolidate NFAs, CIN_Any point and CIN_started/ended. 
      RQ3.raw[,no.repeats:=.N, by=.(CIN_LAchildID_Anon,CIN_LA,CIN_ReferralDate)]

      # calculate length CIN episode for checks
      RQ3.raw[,length.episode:=as.numeric(difftime(CIN_CINClosureDate, CIN_ReferralDate, units="days"))]
      
      # anyone with RC8 as closure reason is not CIN. As discussed with CIN team.
      RQ3.raw$RC8 <- 0
      RQ3.raw[CIN_REC=="RC8", RC8:=1]
      RQ3.raw[,RC8:=min(RC8, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] 
      RQ3.raw[RC8==1, is.cin:=0]

      # anyone with REC= RC1 - RC7 is CIN 
      RQ3.raw[CIN_REC=="RC1"|CIN_REC=="RC2"|CIN_REC=="RC3"|
                CIN_REC=="RC4"|CIN_REC=="RC5"|CIN_REC=="RC6"|
                CIN_REC=="RC7", is.cin:=1]
      
      # unknown: 
      RQ3.raw[CIN_REC=="Unknown"|is.na(CIN_REC), is.cin:=99]
      
      table(RQ3.raw$is.cin, year(RQ3.raw$CIN_ReferralDate))
      
      RQ3.raw[,ref.NFA:=min(CIN_ReferralNFA, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)]
      RQ3.raw[,CIN_AnyPoint:=max(CIN_AnyPoint, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] 
      RQ3.raw[,CIN_Ended:=max(CIN_Ended, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] 
      RQ3.raw[,CIN_Started:=max(CIN_Started, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)]
      
      # remove anyone who has referral NFA==1: (as suggested by CIN team)
      RQ3.raw[RQ3.raw$ref.NFA==1,is.cin:=0]
      RQ3.raw[is.cin!=1 & CIN_CINAt31March==1, is.cin:=1] 
      table(RQ3.raw$is.cin, year(RQ3.raw$CIN_ReferralDate))
      RQ3.raw[is.cin!=1 & CIN_AnyPoint==1, is.cin:=1] 
      table(RQ3.raw$is.cin, year(RQ3.raw$CIN_ReferralDate))
      RQ3.raw[is.cin!=1 & CIN_Started==1, is.cin:=1] 
      RQ3.raw[RQ3.raw$is.cin!=1 & (RQ3.raw$"CIN_CPPstartDate" < RQ3.raw$"CIN_CINClosureDate") & (RQ3.raw$"CIN_CPPstartDate" > RQ3.raw$"CIN_ReferralDate"),is.cin:=1]
      table(RQ3.raw$is.cin, year(RQ3.raw$CIN_ReferralDate))
      RQ3.raw[RQ3.raw$is.cin!=1 & (RQ3.raw$"CIN_DateOfInitialCPC" < RQ3.raw$"CIN_CINClosureDate") & (RQ3.raw$"CIN_DateOfInitialCPC" > RQ3.raw$"CIN_ReferralDate"),is.cin:=1]
      table(RQ3.raw$is.cin, year(RQ3.raw$CIN_ReferralDate))
      View(RQ3.raw[(RQ3.raw$is.cin!=1 & RQ3.raw$"length.episode" > 100),c("is.cin",  "CIN_ReferralDate","CIN_CPPstartDate", "CIN_DateOfInitialCPC",  "CIN_CINClosureDate", "CIN_REC", "ref.NFA", "CIN_CINAt31March", "CIN_AnyPoint", "CIN_Started", "length.episode")])
      View(RQ3.raw[(RQ3.raw$is.cin==0 & RQ3.raw$"length.episode" > 100 & (RQ3.raw$CIN_CINAt31March==1 | RQ3.raw$CIN_AnyPoint==1 | RQ3.raw$CIN_Started==1)),c("is.cin",  "CIN_ReferralDate","CIN_CPPstartDate", "CIN_DateOfInitialCPC",  "CIN_CINClosureDate", "CIN_REC", "ref.NFA", "CIN_CINAt31March", "CIN_AnyPoint", "CIN_Started", "length.episode")])

      RQ3.raw[,is.cin:=min(is.cin, na.rm=TRUE),  by=.(CIN_LAchildID_Anon, CIN_LA, CIN_ReferralDate)] 
      
      
      # keep only the NFAs (exact opposite of above) 
      RQ3.raw$relev.NFA.date <- ifelse(is.na(RQ3.raw$CIN_CINClosureDate), as.Date(RQ3.raw$CIN_ReferralDate), as.Date(RQ3.raw$CIN_CINClosureDate)) 
      RQ3.raw$relev.NFA.date <- as.Date(RQ3.raw$relev.NFA.date)

      RQ3.raw <- as.data.frame(RQ3.raw)    
      RQ3.dem <- RQ3.raw[RQ3.raw$is.cin==0,-which(colnames(RQ3.raw) %in% c("lowinc","CIN_NumberOfPreviousCPP","main.need"))]
      
      # merge in demographics previous CPP & lowinc at referral stage
      RQ3.dem <- merge(RQ3.dem, covar.referral, by=c("CIN_LAchildID_Anon","CIN_LA","CIN_ReferralDate"), all.x=TRUE)
      
      # subset to relevant LAs
      RQ3.dem <- merge(RQ3.dem, matches.final[[which(names(matches.final) %in% "ReferralNFA")]], by.x="CIN_LA",by.y="LA.code",all.y=TRUE)
      
      # only include referrals/closed dates that have been made 
        # (a) at least 6 months before the embedding period, or 
        # (b) after the embedding period ends & 6 months before 31 March 2019
      RQ3.embed <- RQ3.dem[RQ3.dem$CIN_ReferralDate>RQ3.dem$Embedding.end | 
                             RQ3.dem$relev.NFA.date+days(30.5*6)<RQ3.dem$Embedding.start,] 
      # also implement March 2019 restriction
      RQ3.embed <- RQ3.embed[RQ3.embed$relev.NFA.date+days(30.5*6)<"2019-03-31",]
      
      # create post embed and DiD dummies
      RQ3.embed$postembed <- ifelse(RQ3.embed$relev.NFA.date>RQ3.embed$Embedding.end, 1, 0)
      RQ3.embed$DiD <- ifelse(RQ3.embed$postembed==1 & RQ3.embed$SofS.LA==1, 1, 0)
      
      # create years since implementation indicator
      RQ3.embed$years.since <- ifelse(RQ3.embed$postembed==1, 
                                      floor(difftime(RQ3.embed$relev.NFA.date, RQ3.embed$Embedding.end, units = "days")/365.25),
                                      floor(difftime(RQ3.embed$relev.NFA.date, RQ3.embed$Embedding.start, units = "days")/365.25))
      
      # create outcome --> is there a referral within 6 months of the previous referral? 
      load("Working/Intermediate_output/all_referrals.Rdata") # refs --> lists all referrals. 
      RQ3.outc <- merge(RQ3.embed, refs, by=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)
      RQ3.outc <- data.table(RQ3.outc)
      
      RQ3.outc$"cin.ref.cutoff" <- RQ3.outc$relev.NFA.date+days(6*30.5)
      
      # remove referral dates which are not within 6 months of the relev.NFA.date. 
      cols <- grep("refs", colnames(RQ3.outc))
      RQ3.outc[, (cols):=replace(.SD, .SD<=.(relev.NFA.date),NA), .SDcols=cols]
      RQ3.outc[, (cols):=replace(.SD, .SD>.(cin.ref.cutoff),NA), .SDcols=cols]
      
      # create indicator for outcome
      RQ3.df <- as.data.frame(RQ3.outc)
      RQ3.df$"re.referral" <- ifelse(rowSums(is.na(RQ3.df[,cols]))<length(cols),1,0)
     
      # create column that includes the next referral date within 6 months (if available) 
      # so that we can use it as a cutoff for RQ4.
      RQ3.df$"second.referral" <- apply(RQ3.df[,cols], 1, function(x) max(x, na.rm=TRUE))
      RQ3.df$"second.referral" <- as.Date(RQ3.df$second.referral)
      
      # overview of outcome
      table(RQ3.df$re.referral)
      prop.table(table(RQ3.df$re.referral))

      # clean up columns
      RQ3.df <- RQ3.df[,-cols] 
      RQ3.df <- RQ3.df[,-which(colnames(RQ3.df) %in% c("lagged.ref.date","lead.ref.date","cat.abuse","main.need"))] 
      
      # add LA level covars
      stopifnot(unique(RQ3.df$Local.authority) %in% covars.3$Local.authority )
      RQ3.df <- RQ3.df[order(RQ3.df$"CIN_LAchildID_Anon", RQ3.df$"CIN_LA",RQ3.df$"Matched.group",  RQ3.df$"relev.NFA.date"),] 
      RQ3.df <- RQ3.df[!duplicated(RQ3.df[,c("CIN_LA","CIN_LAchildID_Anon","relev.NFA.date","Matched.group")]),]
      
      RQ3.ref.NFA <- merge(RQ3.df, covars.3, by.x=c("Local.authority","relev.year"), by.y=c("Local.authority","year"), all.x=TRUE)
      
      RQ3.ref.NFA <- as.data.frame(RQ3.ref.NFA)

      table(RQ3.ref.NFA$CIN_LA, RQ3.ref.NFA$postembed) # make sure we have enough post embed obs
      table(RQ3.ref.NFA$CIN_LA, RQ3.ref.NFA$Matched.group) # make sure we have enough post embed obs
      table(RQ3.ref.NFA$re.referral, useNA="always")
      
      # tidy up df:
      RQ3.ref.NFA <- rename.cols("CIN_LA","LA", RQ3.ref.NFA)
      RQ3.ref.NFA <- rename.cols("CIN_LAchildID_Anon","child.ID", RQ3.ref.NFA)
      RQ3.ref.NFA <- rename.cols("age.ref","age.at", RQ3.ref.NFA)
      
      # save intermediate version for RQ4 since we need information on re-referrals there. 
      stopifnot(sum(duplicated(RQ3.ref.NFA[,c("LA","child.ID","relev.NFA.date","Matched.group")]))==0) 
      RQ3.ref.NFA <- RQ3.ref.NFA[!duplicated(RQ3.ref.NFA),]
     
      RQ3.intermed <- RQ3.ref.NFA
 
      stopifnot(sum(duplicated(RQ3.intermed[,c("LA","child.ID","relev.NFA.date","Matched.group")]))==0) 
      
      save(RQ3.intermed, "RQ3.intermed", file="./Working/Intermediate_output/RQ3_intermed.Rdata")
      
      to.keep <- c("postembed","DiD","SofS.LA","Matched.group","relev.year","CIN_ReferralDate","years.since","relev.NFA.date",
                   individ.covars, LA.covars,"LA","CIN_LA","child.ID", "previous.CPP","re.referral")
      
      RQ3.ref.NFA <- RQ3.ref.NFA[,which(colnames(RQ3.ref.NFA) %in% to.keep)]
      
      # final. checks
      stopifnot(c("postembed","DiD","Matched.group","SofS.LA","relev.year") %in% colnames(RQ3.ref.NFA))
      setdiff(individ.covars, colnames(RQ3.ref.NFA)) # fine as main.need & cat.abuse not known at referral stage
      stopifnot(sum(duplicated(RQ3.ref.NFA[,c("LA","child.ID","Matched.group","relev.NFA.date")]))==0)
      
      save(RQ3.ref.NFA, "RQ3.ref.NFA", file="Working/baseline populations/RQ3_re-referral_NFA.Rdata")
      

# 4. Re-referrals within 6 months that escalate to CPP/CLA
      load("./Working/Intermediate_output/RQ3_intermed.Rdata")
      RQ4.raw <- RQ3.intermed # we have the same baseline population for RQ3 & RQ4, just the outcome is different.
      
      # See whether, in addition to a referral within 6 months, there has been a 
      # CPP/CLA start within 6 months of the re-referral (second.referral) 
      
      # embedding period restrictions
      RQ4.embed <- RQ4.raw[RQ4.raw$relev.NFA.date+days(30*12)<RQ4.raw$Embedding.start| RQ4.raw$CIN_ReferralDate > RQ4.raw$Embedding.end,] # need 12 months cutoff rather than 6 months as in RQ3

      # also implement March 2019 restriction
      RQ4.embed <- RQ4.embed[RQ4.embed$relev.NFA.date+days(30*12)<"2019-03-31",] 

    
      #** (a): add CPPs
      load("Working/Intermediate_output/all_CPPs.Rdata") # CPP information
      RQ4.cpp <- merge(RQ4.embed, CPPs, by.x=c("child.ID", "LA"), by.y=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)
      RQ4.cpp <- data.table(RQ4.cpp)
      
      # exclude any CPPs that happen before the relevant date. 
      cols.cpp <- grep("CPP\\.", colnames(RQ4.cpp))
      RQ4.cpp[, (cols.cpp):=replace(.SD, .SD<.(relev.NFA.date),NA), .SDcols=cols.cpp]
      
      # take the minimum CPP value of all the CPPs left. 
      sum(duplicated(RQ4.cpp))
      RQ4.cpp <- as.data.frame(RQ4.cpp)
      RQ4.cpp$relev.cpp <- apply(RQ4.cpp[,cols.cpp], 1, function(x) min(x, na.rm=TRUE))
      RQ4.cpp$relev.cpp <- as.Date(RQ4.cpp$relev.cpp)
      

      RQ4.cpp <- RQ4.cpp[RQ4.cpp$relev.cpp>=RQ4.cpp$second.referral | (is.na(RQ4.cpp$relev.cpp)|is.na(RQ4.cpp$second.referral)),]
      sum(duplicated(RQ4.cpp)) 
      
      RQ4.cpp$time.cpp <- as.numeric(difftime(RQ4.cpp$relev.cpp, RQ4.cpp$second.referral, units="days")) 
      RQ4.cpp$time.cpp.2 <- as.numeric(difftime(RQ4.cpp$relev.cpp, RQ4.cpp$relev.NFA.date, units="days")) 
      sum(!is.na(RQ4.cpp$time.cpp)) 
      sum(!is.na(RQ4.cpp$time.cpp) & RQ4.cpp$re.referral==1)
      sum(RQ4.cpp$time.cpp < 180, na.rm=TRUE)
      sum(RQ4.cpp$time.cpp.2 < 180, na.rm=TRUE)
      
      # remove ref.columns
      RQ4.cpp <- RQ4.cpp[,-grep("CPP\\.", colnames(RQ4.cpp))]
      
      #** (b): add CLA periods of care
      load("Working/Intermediate_output/all_CLA_periods.Rdata") # all.pocs = CLA information
      RQ4.cla <- merge(RQ4.cpp, all.pocs, by.x=c("child.ID","LA"), 
                       by.y=c("CHILD_LA_CODE_ANON","LA"), all.x=TRUE) 
      
      RQ4.cla <- data.table(RQ4.cla)
      
      # exclude any CLA that happen before the relevant date. 
      cols.cla <- grep("CLA\\.", colnames(RQ4.cla))
      RQ4.cla[, (cols.cla):=replace(.SD, .SD<.(relev.NFA.date),NA), .SDcols=cols.cla]
      
      # take the minimum CLA value of all the CLAs left.
      RQ4.cla <- as.data.frame(RQ4.cla)
      RQ4.cla$relev.cla <- apply(RQ4.cla[,cols.cla], 1, function(x) min(x, na.rm=TRUE))
      RQ4.cla$relev.cla <- as.Date(RQ4.cla$relev.cla)
      
      # check how many CLAs occur BEFORE the 2nd referral 
      sum(!is.na(RQ4.cla$relev.cla))
      sum(RQ4.cla$relev.cla < RQ4.cla$second.referral,na.rm=TRUE) 
      RQ4.cla <- RQ4.cla[RQ4.cla$relev.cla>=RQ4.cla$second.referral | (is.na(RQ4.cla$relev.cla)|is.na(RQ4.cla$second.referral)),]
      
      RQ4.cla$time.cla <- as.numeric(difftime(RQ4.cla$relev.cla, RQ4.cla$second.referral, units="days"))
      sum(!is.na(RQ4.cla$time.cla))  
      sum(!is.na(RQ4.cla$time.cla) & RQ4.cla$re.referral==1)
      sum(RQ4.cla$time.cla < 180, na.rm=TRUE)
      
      # remove ref.columns
      RQ4.cla <- RQ4.cla[,-grep("CLA.start", colnames(RQ4.cla))]
      
      # ** determine minimum time of CPP {or} CLA
      RQ4.pop <- RQ4.cla
      RQ4.pop$"closest.plan" <- apply(RQ4.pop[,c("relev.cla", "relev.cpp")], 1, function(x) min(x, na.rm=TRUE))
      RQ4.pop$"time.until.plan" <- apply(RQ4.pop[,c("time.cla", "time.cpp")], 1, function(x) min(x, na.rm=TRUE))
      
      # ** visual overview of when escalations occur **
      hist(RQ4.pop$time.until.plan, breaks=100, col='darkmagenta')
      hist(RQ4.pop$time.until.plan[RQ4.pop$time.until.plan<=365], 
           col='darkmagenta',
           breaks=200)
      hist(RQ4.pop$time.until.plan[RQ4.pop$time.until.plan<=250], 
           col='darkmagenta',
           breaks=200)

      # create outcome variable: 
      RQ4.ref.esc <- as.data.frame(RQ4.pop)
      RQ4.ref.esc$escalate <- ifelse((RQ4.ref.esc$time.until.plan <= 30*6) & (RQ4.ref.esc$"re.referral"==1), 1, 0)
      
      # overview of outcome incidence
      table(RQ4.ref.esc$escalate, useNA="always")
      table(RQ4.ref.esc$escalate, RQ4.ref.esc$re.referral, useNA="always")
      
      RQ4.ref.esc <- RQ4.ref.esc[,-which(colnames(RQ4.ref.esc) %in% c("time.cpp", "time.cpp.2", "time.cla", "relev.cpp", "relev.cla", "closest.plan", "time.until.plan", 
                                                                      "re.referral", "cin.ref.cutoff","occur","no.repeats"))]
      
      # remove duplicates "
      to.keep <- c("postembed","DiD","SofS.LA","Matched.group","relev.year","years.since","relev.NFA.date","CIN_ReferralDate",
                   individ.covars, LA.covars,"LA","CIN_LA","child.ID", "previous.CPP","escalate")
      
      RQ4.ref.esc <- RQ4.ref.esc[,which(colnames(RQ4.ref.esc) %in% to.keep)]
      
      
      dim(RQ4.ref.esc)
      RQ4.ref.esc <- RQ4.ref.esc[!duplicated(RQ4.ref.esc),]
      dim(RQ4.ref.esc) 
      
      stopifnot(sum(duplicated(RQ4.ref.esc[,c("child.ID","LA","relev.NFA.date","Matched.group")]))==0)
      sum(duplicated(RQ4.ref.esc[,c("child.ID","LA","relev.NFA.date","Matched.group")]))
      sum(duplicated(RQ4.ref.esc[,c("child.ID","LA","relev.NFA.date","Matched.group","CIN_ReferralDate")]))
      RQ4.ref.esc <- RQ4.ref.esc[order(RQ4.ref.esc$CIN_ReferralDate),]
      RQ4.ref.esc <- RQ4.ref.esc[!duplicated(RQ4.ref.esc[,c("child.ID","LA","relev.NFA.date","Matched.group")], fromLast = TRUE),]
      stopifnot(sum(duplicated(RQ4.ref.esc[,c("child.ID","LA","relev.NFA.date","Matched.group")]))==0)
      
      RQ4.ref.esc <- RQ4.ref.esc[,-which(colnames(RQ4.ref.esc) %in% c("CIN_ReferralDate"))]

      # final checks
      stopifnot(c("postembed","DiD","Matched.group","SofS.LA","relev.year") %in% colnames(RQ4.ref.esc))
      setdiff(individ.covars, colnames(RQ4.ref.esc)) # fine as main.need & cat.abuse not known at referral stage
      
      table(RQ4.ref.esc$LA, RQ4.ref.esc$postembed) # make sure we have enough post embed obs
      table(RQ4.ref.esc$Matched.group, RQ4.ref.esc$postembed) # make sure we have enough post embed obs
      prop.table(table(RQ4.ref.esc$escalate, useNA="always"))
      
      match.to.remove <- unique(matches.final[["Re-referral"]]$Matched.group[matches.final[["Re-referral"]]$LA.code==926])
      RQ4.ref.esc <-  RQ4.ref.esc[-which(RQ4.ref.esc$Matched.group==match.to.remove),] 
      match.to.remove <- unique(matches.final[["Re-referral"]]$Matched.group[matches.final[["Re-referral"]]$LA.code==872])
      RQ4.ref.esc <-  RQ4.ref.esc[-which(RQ4.ref.esc$Matched.group==match.to.remove),]       
      
      table(RQ4.ref.esc$LA, RQ4.ref.esc$postembed) # make sure we have enough post embed obs
      table(RQ4.ref.esc$Matched.group, RQ4.ref.esc$postembed) # make sure we have enough post embed obs
      prop.table(table(RQ4.ref.esc$escalate, useNA="always"))
      
    save(RQ4.ref.esc, "RQ4.ref.esc", file="Working/baseline populations/RQ4_re-referral_escalate.Rdata")
   

# **** RQ 5: Kinship care
    load("Working/Intermediate_output/RQ5_kinship_care_preliminary.Rdata")
    
    # add LA level covariates
    stopifnot(unique(RQ5.kinship.prelim$Local.authority) %in% covars.5$Local.authority)
    RQ5.kinship <- merge(RQ5.kinship.prelim, covars.5, by.x=c("Local.authority","relev.year"), by.y=c("Local.authority","year"), all.x=TRUE)
    
    # create years since implementation indicator
    RQ5.kinship$years.since <- ifelse(RQ5.kinship$postembed==1, 
                                    floor(difftime(RQ5.kinship$poc_start, RQ5.kinship$Embedding.end, units = "days")/365.25),
                                    floor(difftime(RQ5.kinship$poc_start, RQ5.kinship$Embedding.start, units = "days")/365.25)+1) # + 1 because since we give a year to turn kinship care year.since==-1 will be empty.
    
    # tidy up df:
    RQ5.kinship <- rename.cols("CHILD_LA_CODE_ANON","child.ID", RQ5.kinship)
    RQ5.kinship <- rename.cols("age.cla","age.at", RQ5.kinship)
    
    # final. checks
    stopifnot(c("postembed","DiD","Matched.group","SofS.LA","relev.year") %in% colnames(RQ5.kinship))
    stopifnot(individ.covars %in% colnames(RQ5.kinship)) # fine 
    setdiff(individ.covars,colnames(RQ5.kinship)) 
    
    table(RQ5.kinship$LA, RQ5.kinship$postembed) # make sure we have enough post embed obs
    table(RQ5.kinship$LA, RQ5.kinship$Matched.group) 
    table(RQ5.kinship$kinship.care, useNA="always")
    

    to.keep <- c("postembed","DiD","SofS.LA","Matched.group","relev.year","years.since", individ.covars,
                 LA.covars,"LA","child.ID", "previous.CPP","kinship.care","age.cla","poc_start")
    
    RQ5.kinship <- RQ5.kinship[,which(colnames(RQ5.kinship) %in% to.keep)]
    
    RQ5.kinship <- RQ5.kinship[!duplicated(RQ5.kinship),]
    stopifnot(sum(duplicated(RQ5.kinship[,c("child.ID","LA","Matched.group","poc_start")]))==0)
    
    save(RQ5.kinship, "RQ5.kinship", file="Working/baseline populations/RQ5_kinship_care.Rdata")
    
    