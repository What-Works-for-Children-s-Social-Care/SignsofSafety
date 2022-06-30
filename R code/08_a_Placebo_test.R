# PLACEBO TEST TO TEST PARALLEL TREND ASSUMPTION: 
  # also tests potential time varying treatment effects post embed. 


rm(list=ls())
setwd("P:/")
Sys.setenv(R_HISTFILE="P://Working")

source("./Working/00_Package_install.R")
source("./Working/00_functions.R")


# 0. load populations

load("Working/matched populations/RQ1_matched.Rdata") # assessments data
load("Working/matched populations/RQ2_matched.Rdata") # ICPC data
load("Working/matched populations/RQ3_matched.Rdata") # Re-referral NFA data
load("Working/matched populations/RQ4_matched.Rdata") # Re-referral & escalate data
load("Working/matched populations/RQ5_matched.Rdata") # kinship care data

# For RQ1, we used FE
RQ1.matched.FE <- lapply(seq_along(RQ1.matched), function(x) create.test.pop(RQ1.matched[[x]], "Assessment.start.date"))
RQ1.matched.FE <- use.duplicated.data.only(RQ1.matched.FE)
RQ1.matched.FE <- lapply(RQ1.matched.FE, function(x) make.composite.weights(x, "Assessment.start.date"))

# 1. create time dummy for each year pre/post embedding to check for a common trend
create.dummies.lags.only <-  function(df) {
  df$years.since <- as.numeric(as.character(df$years.since))
  df$years.since[df$years.since==0] <- 99  
  df$yrs.SofS <- as.numeric(df$years.since*df$SofS.LA)

  df$lagged.DiD <- ifelse(as.numeric(as.character(df$yrs.SofS))>0, as.numeric(as.character(df$yrs.SofS)), 0)
  df$lagged.DiD <- factor(df$lagged.DiD)
  
  return(df)
}

RQ1.matched.lags <- lapply(seq_along(RQ1.matched), function(x) create.dummies.lags.only(RQ1.matched[[x]]))
RQ2.matched.lags <- lapply(seq_along(RQ2.matched), function(x) create.dummies.lags.only(RQ2.matched[[x]]))
RQ3.matched.lags <- lapply(seq_along(RQ3.matched), function(x) create.dummies.lags.only(RQ3.matched[[x]]))
RQ4.matched.lags <- lapply(seq_along(RQ4.matched), function(x) create.dummies.lags.only(RQ4.matched[[x]]))
RQ5.matched.lags <- lapply(seq_along(RQ5.matched), function(x) create.dummies.lags.only(RQ5.matched[[x]]))

table(RQ1.matched.lags[[1]]$DiD, RQ1.matched.lags[[1]]$lagged.DiD)

# create dummy for years since start of SoS. 
create.dummies <-  function(df) {
  df$years.since <- as.numeric(as.character(df$years.since))
  df$years.since[df$years.since==0] <- 99  
  df$years.since <- factor(df$years.since)

  df$"DiDm9" <- ifelse(df$years.since==-9 & df$SofS.LA==1,1,0)
  df$"DiDm8" <- ifelse(df$years.since==-8 & df$SofS.LA==1,1,0)
  df$"DiDm7" <- ifelse(df$years.since==-7 & df$SofS.LA==1,1,0)
  df$"DiDm6" <- ifelse(df$years.since==-6 & df$SofS.LA==1,1,0)
  df$"DiDm5" <- ifelse(df$years.since==-5 & df$SofS.LA==1,1,0)
  df$"DiDm4" <- ifelse(df$years.since==-4 & df$SofS.LA==1,1,0)
  df$"DiDm3" <- ifelse(df$years.since==-3 & df$SofS.LA==1,1,0)
  df$"DiDm2" <- ifelse(df$years.since==-2 & df$SofS.LA==1,1,0) 
  
  df$"DiD0" <- ifelse(df$years.since==99 & df$SofS.LA==1,1,0)
  df$"DiD1" <- ifelse(df$years.since==1 & df$SofS.LA==1,1,0)
  df$"DiD2" <- ifelse(df$years.since==2 & df$SofS.LA==1,1,0)
  df$"DiD3" <- ifelse(df$years.since==3 & df$SofS.LA==1,1,0)
  
  df <- within(df, years.since <- relevel(years.since, ref="-1"))

  return(df)
}

RQ1.matched.FE <- lapply(seq_along(RQ1.matched.FE), function(x) create.dummies(RQ1.matched.FE[[x]]))

RQ1.matched <- lapply(seq_along(RQ1.matched), function(x) create.dummies(RQ1.matched[[x]]))
RQ2.matched <- lapply(seq_along(RQ2.matched), function(x) create.dummies(RQ2.matched[[x]]))
RQ3.matched <- lapply(seq_along(RQ3.matched), function(x) create.dummies(RQ3.matched[[x]]))
RQ4.matched <- lapply(seq_along(RQ4.matched), function(x) create.dummies(RQ4.matched[[x]]))
RQ5.matched <- lapply(seq_along(RQ5.matched), function(x) create.dummies(RQ5.matched[[x]]))


LA.covars <- c("assessments","ofsted.rating","pop.density","primary.fsm","secondary.fsm",
               "innovation", "percentage.WBRI","timescales.review")

individ.covars <- c("disabled","asylum","lowinc","age.at", "ac.year",
                    "gender","Ethnicity","main.need","cat.abuse","elev.risk","previous.CPP")

individ.covars.in.common <- c("disabled","lowinc","age.at","ac.year","gender","Ethnicity") 
leads.lags <- c("DiDm9", "DiDm8", "DiDm7", "DiDm6", "DiDm5", "DiDm4", "DiDm3", "DiDm2", "DiD0", "DiD1", "DiD2","DiD3")

# define which covars that are not common to all are in each RQ
RQ1.covars <- ""  
RQ2.covars <- ""
RQ3.covars <- c("elev.risk")
RQ4.covars <- c("elev.risk")
RQ5.covars <- "" 

# re-specify model specs
model.1 <- paste("Duration ~SofS.LA+relev.year+years.since", paste(leads.lags, collapse="+"),
                 paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"), sep="+")
model.2 <- paste("ICPC ~SofS.LA+relev.year+years.since",paste(leads.lags, collapse="+"), 
                 paste(individ.covars.in.common, collapse="+"), paste(LA.covars, collapse="+"), sep="+")
model.3 <- paste("re.referral ~SofS.LA+relev.year+years.since",paste(leads.lags, collapse="+"), paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"),paste(RQ3.covars, collapse="+"), sep="+")  
model.4 <- paste("escalate ~SofS.LA+relev.year+years.since",paste(leads.lags, collapse="+"),paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"),paste(RQ4.covars, collapse="+"), sep="+")
model.5 <- paste("kinship.care ~SofS.LA+relev.year+years.since",paste(leads.lags, collapse="+"),paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"), sep="+") 

model.1.lags <- paste("Duration ~relev.year+postembed+SofS.LA+lagged.DiD", 
                 paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"), sep="+")
model.2.lags <- paste("ICPC ~relev.year+postembed+SofS.LA+lagged.DiD",paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"), sep="+")
model.3.lags <- paste("re.referral ~relev.year+postembed+SofS.LA+lagged.DiD",paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"),paste(RQ3.covars, collapse="+"), sep="+")  
model.4.lags <- paste("escalate ~relev.year+postembed+SofS.LA+lagged.DiD",paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"),paste(RQ4.covars, collapse="+"), sep="+")
model.5.lags <- paste("kinship.care ~relev.year+postembed+SofS.LA+lagged.DiD",paste(individ.covars.in.common, collapse="+"),
                 paste(LA.covars, collapse="+"), sep="+") 



# 3. run regression to test assumptions. 

remove.time.invariant <- function(model.no){
  mv <- gsub("Ethnicity\\+","",model.no)
  mv <- gsub("gender\\+","",mv)
  mv <- gsub("disabled\\+","",mv)
  return(mv)
}
model.1.varying <- remove.time.invariant(model.1)
main.model.RQ1 <- plm.multiple.imp(RQ1.matched.FE, model.1.varying, "within") # this runs very long... 
parameters.1 <- create.regression.parameters(main.model.RQ1, model.type = "plm")
table.1 <- summary(miceadds::pool_mi(qhat=parameters.1[[1]], se=parameters.1[[2]], dfcom = dim(RQ1.matched[[1]])[1]-18, method="smallsample"),digits=2) 
table.1[,c(1:6)] <- round(table.1[,c(1:6)],4)
write.csv(table.1, file="./Working/Final output/Lagged treatment effects RQ1 FE.csv")



#### logit for RQ2 and RQ4

# RQ2 logit - leads and lags
logit.RQ2 <- lapply(RQ2.matched, FUN=function(data){
  miceadds::glm.cluster(data=data, formula=formula(model.2), cluster=data$LA, weights=data$weights, family = binomial(link = "logit"))
})
betas.logit.RQ2 <- lapply(logit.RQ2, FUN=function(rr){ coef(rr) }) 
params.logit.RQ2 <- create.regression.parameters(logit.RQ2, "lm.cluster")
table.logit.RQ2 <- summary(miceadds::pool_mi(qhat=params.logit.RQ2[[1]], se=params.logit.RQ2[[2]], dfcom = dim(RQ2.matched[[1]])[1]-17, method="smallsample"), alpha=0.05) 
write.csv(table.logit.RQ2, file="./Working/Final output/Lagged treatment effects RQ2 - logit.csv")

# RQ2 logit - lags only
logit.RQ2.lags <- lapply(RQ2.matched.lags, FUN=function(data){
  miceadds::glm.cluster(data=data, formula=formula(model.2.lags), cluster=data$LA, weights=data$weights, family = binomial(link = "logit"))
})
betas.logit.RQ2.lags <- lapply(logit.RQ2.lags, FUN=function(rr){ coef(rr) }) 
params.logit.RQ2.lags <- create.regression.parameters(logit.RQ2.lags, "lm.cluster")
table.logit.RQ2.lags <- summary(miceadds::pool_mi(qhat=params.logit.RQ2.lags[[1]], se=params.logit.RQ2.lags[[2]], dfcom = dim(RQ2.matched.lags[[1]])[1]-17, method="smallsample"), alpha=0.05) 
write.csv(table.logit.RQ2.lags, file="./Working/Final output/Lagged treatment effects RQ2 - lags only logit.csv")

# RQ4 logit - leads and lags
relevel.vars <- function(df) { 
  levels(df$gender) <- c("0", "1", "2", "Missing", "Missing") 
  levels(df$ac.year) <- c("Missing","Missing","PRIM","SEC")
  levels(df$relev.year) <- c("2008","2009","2010","2015","2016","2017","2017")
  return(df)}
RQ4.matched.relevelled <- lapply(RQ4.matched, function(x) relevel.vars(x))


logit.RQ4 <- lapply(RQ4.matched.relevelled, FUN=function(data){
  miceadds::glm.cluster(data=data, formula=formula(model.4), cluster=data$LA, weights=data$weights, family = binomial(link = "logit"))
})
betas.logit.RQ4 <- lapply(logit.RQ4, FUN=function(rr){ coef(rr) }) 
params.logit.RQ4 <- create.regression.parameters(logit.RQ4, "lm.cluster")
table.logit.RQ4 <- summary(miceadds::pool_mi(qhat=params.logit.RQ4[[1]], se=params.logit.RQ4[[2]], dfcom = dim(RQ4.matched.relevelled[[1]])[1]-17, method="smallsample"), alpha=0.05) 
write.csv(table.logit.RQ4, file="./Working/Final output/Lagged treatment effects RQ4 - logit.csv")

# RQ4 logit - lags only
RQ4.matched.lags.relevelled <- lapply(RQ4.matched.lags, function(x) relevel.vars(x))
logit.RQ4.lags <- lapply(RQ4.matched.lags.relevelled, FUN=function(data){
  miceadds::glm.cluster(data=data, formula=formula(model.4.lags), cluster=data$LA, weights=data$weights, family = binomial(link = "logit"))
})
betas.logit.RQ4.lags <- lapply(logit.RQ4.lags, FUN=function(rr){ coef(rr) }) 
params.logit.RQ4.lags <- create.regression.parameters(logit.RQ4.lags, "lm.cluster")
table.logit.RQ4.lags <- summary(miceadds::pool_mi(qhat=params.logit.RQ4.lags[[1]], se=params.logit.RQ4.lags[[2]], dfcom = dim(RQ4.matched.lags.relevelled[[1]])[1]-17, method="smallsample"), alpha=0.05) 
write.csv(table.logit.RQ4.lags, file="./Working/Final output/Lagged treatment effects RQ4 - lags only logit.csv")


# Linear models
main.model.RQ1 <- miceadds.lmcluster(RQ1.matched, model.1)
main.model.RQ2 <- miceadds.lmcluster(RQ2.matched, model.2)
main.model.RQ3 <- miceadds.lmcluster(RQ3.matched, model.3)
main.model.RQ4 <- miceadds.lmcluster(RQ4.matched, model.4)
main.model.RQ5 <- miceadds.lmcluster(RQ5.matched, model.5)

# parameters: 
parameters.1 <- create.regression.parameters(main.model.RQ1, model.type = "lm.cluster")
parameters.2 <- create.regression.parameters(main.model.RQ2, model.type = "lm.cluster")
parameters.3 <- create.regression.parameters(main.model.RQ3, model.type = "lm.cluster")
parameters.4 <- create.regression.parameters(main.model.RQ4, model.type = "lm.cluster")
parameters.5 <- create.regression.parameters(main.model.RQ5, model.type = "lm.cluster")


# Final results: 
table.1 <- summary(miceadds::pool_mi(qhat=parameters.1[[1]], se=parameters.1[[2]], dfcom = dim(RQ1.matched[[1]])[1]-18, method="smallsample"),digits=2) 
table.2 <- summary(miceadds::pool_mi(qhat=parameters.2[[1]], se=parameters.2[[2]], dfcom = dim(RQ2.matched[[1]])[1]-18, method="smallsample")) 
table.3 <- summary(miceadds::pool_mi(qhat=parameters.3[[1]], se=parameters.3[[2]], dfcom = dim(RQ3.matched[[1]])[1]-17, method="smallsample"))
table.4 <- summary(miceadds::pool_mi(qhat=parameters.4[[1]], se=parameters.4[[2]], dfcom = dim(RQ4.matched[[1]])[1]-17, method="smallsample")) 
table.5 <- summary(miceadds::pool_mi(qhat=parameters.5[[1]], se=parameters.5[[2]], dfcom = dim(RQ5.matched[[1]])[1]-17, method="smallsample"), alpha=0.05)  

# re-format and save as text file for table production in report. 
table.1[,c(1:6)] <- round(table.1[,c(1:6)],4)
table.2[,c(1:6)] <- round(table.2[,c(1:6)],4)
table.3[,c(1:6)] <- round(table.3[,c(1:6)],4)
table.4[,c(1:6)] <- round(table.4[,c(1:6)],4)
table.5[,c(1:6)] <- round(table.5[,c(1:6)],4)

write.csv(table.1, file="./Working/Final output/Lagged treatment effects RQ1.csv")
write.csv(table.2, file="./Working/Final output/Lagged treatment effects RQ2.csv")
write.csv(table.3, file="./Working/Final output/Lagged treatment effects RQ3.csv")
write.csv(table.4, file="./Working/Final output/Lagged treatment effects RQ4.csv")
write.csv(table.5, file="./Working/Final output/Lagged treatment effects RQ5.csv")


# *********** RUNNING FOR LAGS ONLY ************
lags.RQ1 <- miceadds.lmcluster(RQ1.matched.lags, model.1.lags)
lags.RQ2 <- miceadds.lmcluster(RQ2.matched.lags, model.2.lags)
lags.RQ3 <- miceadds.lmcluster(RQ3.matched.lags, model.3.lags)
lags.RQ4 <- miceadds.lmcluster(RQ4.matched.lags, model.4.lags)
lags.RQ5 <- miceadds.lmcluster(RQ5.matched.lags, model.5.lags)

# parameters: 
lags.parameters.1 <- create.regression.parameters(lags.RQ1, model.type = "lm.cluster")
lags.parameters.2 <- create.regression.parameters(lags.RQ2, model.type = "lm.cluster")
lags.parameters.3 <- create.regression.parameters(lags.RQ3, model.type = "lm.cluster")
lags.parameters.4 <- create.regression.parameters(lags.RQ4, model.type = "lm.cluster")
lags.parameters.5 <- create.regression.parameters(lags.RQ5, model.type = "lm.cluster")


# Final results: 
lags.table.1 <- summary(miceadds::pool_mi(qhat=lags.parameters.1[[1]], se=lags.parameters.1[[2]], dfcom = dim(RQ1.matched[[1]])[1]-18, method="smallsample"),digits=2) 
lags.table.2 <- summary(miceadds::pool_mi(qhat=lags.parameters.2[[1]], se=lags.parameters.2[[2]], dfcom = dim(RQ2.matched[[1]])[1]-18, method="smallsample")) 
lags.table.3 <- summary(miceadds::pool_mi(qhat=lags.parameters.3[[1]], se=lags.parameters.3[[2]], dfcom = dim(RQ3.matched[[1]])[1]-17, method="smallsample"))
lags.table.4 <- summary(miceadds::pool_mi(qhat=lags.parameters.4[[1]], se=lags.parameters.4[[2]], dfcom = dim(RQ4.matched[[1]])[1]-17, method="smallsample")) 
lags.table.5 <- summary(miceadds::pool_mi(qhat=lags.parameters.5[[1]], se=lags.parameters.5[[2]], dfcom = dim(RQ5.matched[[1]])[1]-17, method="smallsample"), alpha=0.05)  

# re-format and save as text file for table production in report. 
lags.table.1[,c(1:6)] <- round(lags.table.1[,c(1:6)],4)
lags.table.2[,c(1:6)] <- round(lags.table.2[,c(1:6)],4)
lags.table.3[,c(1:6)] <- round(lags.table.3[,c(1:6)],4)
lags.table.4[,c(1:6)] <- round(lags.table.4[,c(1:6)],4)
lags.table.5[,c(1:6)] <- round(lags.table.5[,c(1:6)],4)

write.csv(lags.table.1, file="./Working/Final output/Lagged treatment effects RQ1 - lags only.csv")
write.csv(lags.table.2, file="./Working/Final output/Lagged treatment effects RQ2 - lags only.csv")
write.csv(lags.table.3, file="./Working/Final output/Lagged treatment effects RQ3 - lags only.csv")
write.csv(lags.table.4, file="./Working/Final output/Lagged treatment effects RQ4 - lags only.csv")
write.csv(lags.table.5, file="./Working/Final output/Lagged treatment effects RQ5 - lags only.csv")

