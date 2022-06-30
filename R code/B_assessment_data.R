
#     ******************* Assessment data ****************# 
# Description: 
#   Format assessment data which is held separately
#     0. format individual dfs
#     1. merge together
#     2. merge with demographics in script 03_create_populations

# *******************************************************************************************#

## read in assessments
  assess.2011 <- read.csv("xx/WWCSC_Core_Assessments__2012_CIN.csv", stringsAsFactors = FALSE)
  assess.201213 <- read.csv("xxWWCSC_Core_Assessments__2013_2014_CIN.csv", stringsAsFactors = FALSE)
  assess.201418 <- read.csv("xx/DR191206_02_CIN/WWCSC_Assessments__2014_2019_CIN.csv", stringsAsFactors = FALSE)
  colnames(assess.2011) <- colnames(assess.201213) <- colnames(assess.201418) <- c("CIN_PupilMatchingRefAnonyous","ACADYR","LA","LAchildID_Anon","Assessment.start.date","Assessment.end.date")

  # format dates as dates
  assess.2011$Assessment.start.date <- as.Date(assess.2011$Assessment.start.date)
  assess.2011$Assessment.end.date <- as.Date(assess.2011$Assessment.end.date)
  assess.201213$Assessment.start.date <- as.Date(assess.201213$Assessment.start.date)
  assess.201213$Assessment.end.date <- as.Date(assess.201213$Assessment.end.date)  
  assess.201418$Assessment.start.date <- as.Date(assess.201418$Assessment.start.date)
  assess.201418$Assessment.end.date <- as.Date(assess.201418$Assessment.end.date)
  
  # subset to make sure we only have relevant dates
  
  assess.2011 <- assess.2011[year(assess.2011$Assessment.start.date)>=2011,] 
  assess.2011 <- assess.2011[year(assess.2011$Assessment.end.date)>=2011,] 
  assess.2011$LA <- as.character(assess.2011$LA)

  assess.201213 <- assess.201213[year(assess.201213$Assessment.start.date)>=2011,] 
  assess.201213$LA <- as.character(assess.201213$LA)

  assess.201418 <- assess.201418[year(assess.201418$Assessment.start.date)>=2011,] 
  assess.201418$LA <- as.character(assess.201418$LA)

# merge assessments together
  list.assessments <- list(assess.2011, assess.201213, assess.201418)
  assessments.all <- Reduce(function(dtf1, dtf2) rbind(dtf1,dtf2), list.assessments)
  assessments.all <- assessments.all[!(assessments.all$Assessment.start.date > assessments.all$Assessment.end.date),]
  
  assessments.all <- assessments.all[,-which(colnames(assessments.all) %in% c("ACADYR","CIN_PupilMatchingRefAnonyous"))]
  
  # remove duplicates
  assessments.all <- assessments.all[!duplicated(assessments.all),]
  
  # cases where only the assessment end date differs. assumption: pick later end date if between two 
  assessments.all <- assessments.all[order(assessments.all$Assessment.end.date),]
  assessments.all <- assessments.all[!duplicated(assessments.all[,c("LAchildID_Anon","LA","Assessment.start.date")], fromLast=TRUE),]

  # ** IDENTIFY MOST RECENT REFERRAL DATE ***

  load("Working/Intermediate_output/all_referrals.Rdata") # comes from information_merge2pop file. 
  assessments.all <- merge(assessments.all, refs, by.x=c("LAchildID_Anon","LA"), by.y=c("CIN_LAchildID_Anon","CIN_LA"), all.x=TRUE)
  
  # exclude any referral that happen after the assessment start date
    cols.ref <- grep("refs\\.", colnames(assessments.all))
    assessments.all <- data.table(assessments.all)
    assessments.all[, (cols.ref):=replace(.SD, .SD>.(Assessment.start.date),NA), .SDcols=cols.ref]
  
  # take the maximum i.e. most recent referral date of all referrals left
    assessments.all <- as.data.frame(assessments.all)
    assessments.all$latest.referral <- apply(assessments.all[,cols.ref], 1, function(x) max(x, na.rm=TRUE))
    assessments.all$latest.referral <- as.Date(unlist(assessments.all$latest.referral))
    
  # check for time elapsed between referral date and assessment. 
    assessments.all$time.elapsed <- as.numeric(difftime(assessments.all$Assessment.start.date, assessments.all$latest.referral, units="days"))
    summary(assessments.all$time.elapsed)
  
  # remove ref.columns
    assessments.all <- assessments.all[,-grep("refs", colnames(assessments.all))]
    assessments.all <- assessments.all[,-which(colnames(assessments.all) %in% c("time.elapsed"))]
    
  save(assessments.all,"assessments.all",file="Working/Intermediate_output/all_assessments.Rdata")
    
  