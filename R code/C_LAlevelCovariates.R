#***  CLEAN LA LEVEL COVARIATES ***# 

rm(list=ls())
setwd("P:/")

source("./Working/00_Package_install.R")
source("xx/Functions_SofS_Covariates.R")

load("P:/Working/Intermediate_output/parameters.Rdata") # matches.final information on matches & embedding periods

# Additional information: 
# Year designation: for e.g. CIN census data the year is measured as April to March. We name the year as the first year, e.g. 
# for data for April 2008 - March 2009 we name this data 2008. This is the general notation across the coding of SofS. 
# Anything happening in January - March of a year should be attributed to the year before.


#**** read in matching groups
  # read in all LAs we use
  all.LAs <- read_excel("xx/MatchingData.xlsx", sheet="Number of unique LAs", col_names = FALSE)
  colnames(all.LAs) <- "Local.authority"

  # read in matches for all outcomes
  outcome.groups <- c("ICPC","Duration","Referrals","Kinship Care") 
  matches <- list()
  for (i in seq_along(outcome.groups)) {
    matches[[i]] <- read_excel("./xx/MatchingData.xlsx",skip=1, sheet=outcome.groups[[i]])
    matches[[i]] <- matches[[i]][,1:3]
    matches[[i]][matches[[i]]=="N/A"]<- NA
  }
  names(matches) <- outcome.groups

##### read in LA level covariate information. 

#*** primary FSM ***
  # read in data
  primary.fsm <- read_excel("xx/LA Level Covariates Master Copy.xlsx", sheet="Primary FSM", skip=1)
  primary.fsm[,2:length(primary.fsm)] <- apply(primary.fsm[,2:length(primary.fsm)], 2, function(x) as.numeric(x))
  colnames(primary.fsm)[1] <- "Local.authority"

  # FSM numbers were taken in January of each year --> since our other LA level variables run from April to March (i.e. 2007 means April 2007 - March 2008), adjust years
  colnames(primary.fsm) <- c("Local.authority", 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
  primary.fsm <- primary.fsm[,-which(colnames(primary.fsm) %in% c("2007"))] 

  # consolidate LA names with all.LAs
  missing.LAs <- setdiff(unlist(all.LAs),primary.fsm$Local.authority)

  ## reshape into long format 
  primary.fsm <- data.table(primary.fsm)
  primary.fsm <- melting.years(df=primary.fsm, id.var="Local.authority", var.of.interest = "primary.fsm")

  # if data not available for individual years: linear extrapolation
  primary.fsm <- extrapolate.years(min.year=min(primary.fsm$year), max.year=max(primary.fsm$year), df=primary.fsm, var.of.interest="primary.fsm")  
  stopifnot(sum(is.na(primary.fsm$primary.fsm))==0)

#* secondary FSM
  secondary.fsm <- read_excel("xx/LA Level Covariates Master Copy.xlsx", sheet="Secondary FSM", skip=1)
  secondary.fsm[,2:length(secondary.fsm)] <- apply(secondary.fsm[,2:length(secondary.fsm)], 2, function(x) as.numeric(x))
  colnames(secondary.fsm)[1] <- "Local.authority"

  # adjust years as before for primary fsm 
  colnames(secondary.fsm) <- c("Local.authority", 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
  secondary.fsm <- secondary.fsm[,-which(colnames(secondary.fsm) %in% c("2007"))] 

  secondary.fsm <- data.table(secondary.fsm)
  secondary.fsm <- melting.years(df=secondary.fsm, id.var="Local.authority", var.of.interest = "secondary.fsm")
  secondary.fsm <- extrapolate.years(min.year=min(secondary.fsm$year), max.year=max(secondary.fsm$year), df=secondary.fsm, var.of.interest="secondary.fsm") 
  stopifnot(sum(is.na(secondary.fsm$secondary.fsm))==0)

#* Population density
  pop.density <- read_excel("xx/LA Level Covariates Master Copy.xlsx", sheet="Population density", skip=1)
  pop.density[,2:length(pop.density)] <- apply(pop.density[,2:length(pop.density)], 2, function(x) as.numeric(x))
  names(pop.density)[1] <- "Local.authority"
  pop.density <- pop.density[,-which(colnames(pop.density) %in% c("2019"))]

  pop.density <- data.table(pop.density)
  pop.density <- melting.years(df=pop.density, id.var="Local.authority", var.of.interest = "pop.density") # reshape
  pop.density <- extrapolate.years(min.year=min(pop.density$year), max.year=max(pop.density$year), df=pop.density, var.of.interest="pop.density") 
  stopifnot(sum(is.na(pop.density$pop.density))==0) 

#* Ofsted ratings
  ofsted.rating <- read_excel("xx/LA Level Covariates Master Copy.xlsx", sheet="Ofsted ratings", skip=1)
  ofsted.rating[,2:length(ofsted.rating)] <- apply(ofsted.rating[,2:length(ofsted.rating)], 2, function(x) as.numeric(x))
  names(ofsted.rating)[1] <- "Local.authority"
  ofsted.rating <- ofsted.rating[,-which(colnames(ofsted.rating) %in% c("2019"))] 

  # Ofsted rating remain the same until the next inspection --> use "last observation carried forward" (locf) rather than linear extrapolation
  ofsted.locf <- t(apply(ofsted.rating[,2:length(ofsted.rating)],1,function(x) nafill(x, "locf")))
  ofsted.locf <- t(apply(ofsted.locf,1,function(x) nafill(x, "nocb"))) # where the initial value is missing
  ofsted.locf <- as.data.frame(ofsted.locf)
  colnames(ofsted.locf) <- colnames(ofsted.rating)[2:length(ofsted.rating)]
  ofsted.locf$"Local.authority" <- ofsted.rating$Local.authority
  ofsted.locf <- ofsted.locf[,c(length(ofsted.locf), 1:(length(ofsted.locf)-1))]

  ofsted.rating <- data.table(ofsted.rating)
  ofsted.rating <- melting.years(df=ofsted.locf, id.var="Local.authority", var.of.interest = "ofsted.rating") 
  stopifnot(sum(is.na(ofsted.rating$ofsted.rating))==0)

#* number of assessments
  # 2019/20 data won't be released until 2021
  assessments <- read_excel("xx/LA Level Covariates Master Copy.xlsx", sheet="Assessments", skip=1)
  colnames(assessments) <- c("Local.authority",2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)
  assessments[,2:length(assessments)] <- apply(assessments[,2:length(assessments)], 2, function(x) as.numeric(x))

  assessments <- data.table(assessments)
  assessments <- melting.years(df=assessments, id.var="Local.authority", var.of.interest = "assessments") # reshape
  assessments <- extrapolate.years(min.year=min(assessments$year), max.year=max(assessments$year), df=assessments, var.of.interest="assessments") 
  stopifnot(sum(is.na(assessments$assessments))==0)

#* innovation projects 
  innovation <- read_excel("xx/Innovation Projects S o S.xlsx", sheet="S o S LAs", skip=2)
  innovation <- innovation[,1:6]
  colnames(innovation) <- c("Local.authority", 2014, 2015, 2016, 2017, 2018)
  innovation[,c("2008", "2009", "2010", "2011", "2012", "2013")] <- list(0)   # innovation programme started in 2014

  innovation[,2:length(innovation)] <- apply(innovation[,2:length(innovation)], 2, function(x) as.numeric(x))
  innovation <- innovation[which(innovation$Local.authority %in% all.LAs$Local.authority),]

  innovation <- data.table(innovation)
  innovation <- melting.years(df=innovation, id.var="Local.authority", var.of.interest = "innovation") # reshape
  stopifnot(sum(is.na(innovation$innovation))==0)

  
  #* CPPs reviewed within the required timescales
  reviews <- read_excel("xx/LA Level Covariates Master Copy.xlsx", sheet="CPP reviewed within timescales ", skip=1) 
  reviews[,2:length(reviews)] <- apply(reviews[,2:length(reviews)], 2, function(x) as.numeric(x))
  colnames(reviews)[1] <- "Local.authority"
  
  # adjust years 
  colnames(reviews) <- c("Local.authority", 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
  reviews <- reviews[,-which(colnames(reviews) %in% c("2019"))] 
  
  reviews <- data.table(reviews)
  reviews <- melting.years(df=reviews, id.var="Local.authority", var.of.interest = "timescales.review") # reshape
  reviews <- extrapolate.years(min.year=min(reviews$year), max.year=max(reviews$year), df=reviews, var.of.interest="timescales.review") 
  stopifnot(sum(is.na(reviews$timescales.review))==0)
  
# Overview over years
  all.data <- c("assessments", "ofsted.rating", "pop.density", "primary.fsm", "secondary.fsm","innovation","reviews")
  all.data.list <- lapply(all.data, get)
  
  for (i in seq_along(all.data)) {
    print(c(all.data[i], min(all.data.list[[i]]$"year"),max(all.data.list[[i]]$"year")))
  }

# check for different spelling of LA names
  setdiff(assessments$Local.authority, ofsted.rating$Local.authority) # Kingston upon Hull
  setdiff(ofsted.rating$Local.authority, pop.density$Local.authority) # Telford & Wrekin
  setdiff(ofsted.rating$Local.authority, reviews$Local.authority) # Telford & Wrekin
  
  assessments <- correct.spellings(assessments)
  ofsted.rating <- correct.spellings(ofsted.rating)
  pop.density <- correct.spellings(pop.density)
  primary.fsm <- correct.spellings(primary.fsm)
  secondary.fsm <- correct.spellings(secondary.fsm)
  innovation <- correct.spellings(innovation)
  reviews <- correct.spellings(reviews)

  # keep only LAs that will be used in the analysis
  assessments <- assessments[which(assessments$Local.authority %in% all.LAs$Local.authority),]
  ofsted.rating <- ofsted.rating[which(ofsted.rating$Local.authority %in% all.LAs$Local.authority),]
  pop.density <- pop.density[which(pop.density$Local.authority %in% all.LAs$Local.authority),]
  primary.fsm <- primary.fsm[which(primary.fsm$Local.authority %in% all.LAs$Local.authority),]
  secondary.fsm <- secondary.fsm[which(secondary.fsm$Local.authority %in% all.LAs$Local.authority),]
  innovation <- innovation[which(innovation$Local.authority %in% all.LAs$Local.authority),]
  reviews <- reviews[which(reviews$Local.authority %in% all.LAs$Local.authority),]
  
  stopifnot(intersect(unique(all.LAs$Local.authority), assessments$Local.authority)==unique(all.LAs$Local.authority))
  stopifnot(intersect(unique(all.LAs$Local.authority), ofsted.rating$Local.authority)==unique(all.LAs$Local.authority))
  stopifnot(intersect(unique(all.LAs$Local.authority), pop.density$Local.authority)==unique(all.LAs$Local.authority))
  stopifnot(intersect(unique(all.LAs$Local.authority), primary.fsm$Local.authority)==unique(all.LAs$Local.authority))
  stopifnot(intersect(unique(all.LAs$Local.authority), secondary.fsm$Local.authority)==unique(all.LAs$Local.authority))
  stopifnot(intersect(unique(all.LAs$Local.authority), innovation$Local.authority)==unique(all.LAs$Local.authority))
  stopifnot(intersect(unique(all.LAs$Local.authority), reviews$Local.authority)==unique(all.LAs$Local.authority))
  
  all.data.list <- lapply(all.data, get) 

  # merge datasets together
  covariate.data <- Reduce(function(x,y) merge(x,y, by=c("Local.authority", "year"), all=TRUE), all.data.list)
 
# ************ CALCULATION OF REMAINING COVARS *********************#
  
# calculate % of CSC population White British  

  load("P:/Working/Intermediate_output/all_LAs_codes.Rdata") # from "A_parameter_matches"
  load("Working/Intermediate_output/CIN_merged_raw.Rdata")
  
  cin.cov <- all.cin[,c("CIN_LAchildID_Anon","CIN_LA","CIN_Ethnicity","CIN_ReferralDate")]
  cin.cov <- merge(cin.cov, matches.codes, by.x="CIN_LA",by.y="LA.code", all.y=TRUE)
  
  cin.cov$CIN_ReferralDate <- as.Date(cin.cov$CIN_ReferralDate)
  cin.cov$"year" <- as.numeric(year(cin.cov$CIN_ReferralDate)-(month(cin.cov$CIN_ReferralDate)<=3))
  cin.cov <- cin.cov[!cin.cov$year <2008,]
  cin.cov <- cin.cov[!cin.cov$year >2018,]
  
  cin.cov <- cin.cov[,-which(colnames(cin.cov) %in% "CIN_ReferralDate")]
  
  cin.cov <- cin.cov[!is.na(cin.cov$CIN_Ethnicity),] 
  cin.cov$Ethnicity <- ifelse(cin.cov$CIN_Ethnicity=="WBRI",1,0) 
  cin.ethnicity <- aggregate(cin.cov$Ethnicity, by = list(cin.cov$"CIN_LA",cin.cov$"year",cin.cov$Local.authority),FUN=mean, na.rm=TRUE) 
  colnames(cin.ethnicity) <- c("CIN_LA","year","Local.authority","percentage.WBRI")
  
  cin.ethnicity <- correct.spellings(cin.ethnicity)
  setdiff(unique(all.LAs$Local.authority), cin.ethnicity$Local.authority)
  cin.ethnicity$Local.authority[cin.ethnicity$Local.authority=="Southend on Sea"] <- "Southend-on-Sea"
  cin.ethnicity <- cin.ethnicity[,-which(colnames(cin.ethnicity) %in% "CIN_LA")]
  stopifnot(sum(is.na(cin.ethnicity$percentage.WBRI))==0)
  
  # merge with other covariates data. 
  stopifnot(intersect(unique(all.LAs$Local.authority), cin.ethnicity$Local.authority)==unique(all.LAs$Local.authority))
  covariate.data <- merge(covariate.data,cin.ethnicity, by=c("Local.authority","year"), all=TRUE)
  
  save(covariate.data, "covariate.data", file="./Working/Intermediate_output/covariates.Rdata")
  
  # scale LA level covariates: 
  vars.to.scale <- c("assessments","pop.density","primary.fsm","secondary.fsm","timescales.review","percentage.WBRI")
  covars.list <- list()
  for (i in 1:5){
    covars.list[[i]] <- covariate.data[covariate.data$Local.authority %in% matches.final[[i]]$Local.authority,]
    covars.list[[i]][,which(colnames(covars.list[[i]]) %in% vars.to.scale)] <- scale(covars.list[[i]][,which(colnames(covars.list[[i]]) %in% vars.to.scale)])
  }
  covars.1 <- as.data.frame(covars.list[[1]])
  covars.2 <- as.data.frame(covars.list[[2]])
  covars.3 <- as.data.frame(covars.list[[3]])
  covars.4 <- as.data.frame(covars.list[[5]]) 
  covars.5 <- as.data.frame(covars.list[[4]])
  
  save(covars.1, "covars.1", file="./Working/Intermediate_output/covariates_scaled_1.Rdata")
  save(covars.2, "covars.2", file="./Working/Intermediate_output/covariates_scaled_2.Rdata")
  save(covars.3, "covars.3", file="./Working/Intermediate_output/covariates_scaled_3.Rdata")
  save(covars.4, "covars.4", file="./Working/Intermediate_output/covariates_scaled_4.Rdata")
  save(covars.5, "covars.5", file="./Working/Intermediate_output/covariates_scaled_5.Rdata")
