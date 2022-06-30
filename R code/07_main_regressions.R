
# ********************** Main Analysis ****************


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


# 1. Determine model specifications:

  LA.covars <- c("assessments","ofsted.rating","pop.density","primary.fsm","secondary.fsm",
                 "innovation", "percentage.WBRI","timescales.review") # doesn't include LA level fixed effect since this is collinear with SofS.LA

  individ.covars <- c("disabled","asylum","lowinc","age.at", "ac.year",
                      "gender","Ethnicity","main.need","cat.abuse","elev.risk","previous.CPP")

  individ.covars.in.common <- c("disabled","lowinc","age.at","ac.year","gender","Ethnicity") # these are the ones each of datasets have. 

  # define which covars that are not common to all are in each RQ
    RQ1.covars <- "" # none included. 
    RQ2.covars <- ""
    RQ3.covars <- c("elev.risk")
    RQ4.covars <- c("elev.risk")
    RQ5.covars <- "" 

  model.1 <- paste("Duration ~postembed+SofS.LA+DiD+relev.year",
                         paste(individ.covars.in.common, collapse="+"),
                         paste(LA.covars, collapse="+"), sep="+")
  model.2 <- paste("ICPC ~postembed+SofS.LA+DiD+relev.year",paste(individ.covars.in.common, collapse="+"),
                         paste(LA.covars, collapse="+"), sep="+")
  model.3 <- paste("re.referral ~postembed+SofS.LA+DiD+relev.year",paste(individ.covars.in.common, collapse="+"),
                         paste(LA.covars, collapse="+"),paste(RQ3.covars, collapse="+"), sep="+")  
  model.4 <- paste("escalate ~postembed+SofS.LA+DiD+relev.year",paste(individ.covars.in.common, collapse="+"),
                         paste(LA.covars, collapse="+"),paste(RQ4.covars, collapse="+"), sep="+")
  model.5 <- paste("kinship.care ~postembed+SofS.LA+DiD+relev.year",paste(individ.covars.in.common, collapse="+"),
                         paste(LA.covars, collapse="+"), sep="+")

# 2. run analysis 
  main.model.RQ1 <- miceadds.lmcluster(RQ1.matched, model.1)
  main.model.RQ2 <- miceadds.lmcluster(RQ2.matched, model.2) # note that for the main specification provided in the addendum, the regression chosen has been a linear regression with standard errors clustered at the individual level which is not reported here due to publication clearance timelines in the SRS. 
  main.model.RQ3 <- miceadds.lmcluster(RQ3.matched, model.3)
  main.model.RQ4 <- miceadds.lmcluster(RQ4.matched, model.4)
  main.model.RQ5 <- miceadds.lmcluster(RQ5.matched, model.5)
  
  # create parameters
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

  # run logit model for RQ2 given low incidence of ICPCs
  sum(RQ2.matched[[1]]$ICPC==1)/dim(RQ2.matched[[1]])[1]
  
  logit.RQ2 <- lapply(RQ2.matched, FUN=function(data){
    miceadds::glm.cluster(data=data, formula=formula(model.2), cluster=data$LA, weights=data$weights, family = binomial(link = "logit"))
  })
  betas.logit.RQ2 <- lapply(logit.RQ2, FUN=function(rr){ coef(rr) }) 
  params.logit.RQ2 <- create.regression.parameters(logit.RQ2, "lm.cluster")
  ab <- summary(miceadds::pool_mi(qhat=params.logit.RQ2[[1]], se=params.logit.RQ2[[2]], dfcom = dim(RQ2.matched[[1]])[1]-17, method="smallsample"), alpha=0.05) 

  table.2.logit <- summary(miceadds::pool_mi(qhat=params.logit.RQ2[[1]], se=params.logit.RQ2[[2]], dfcom = dim(RQ2.matched[[1]])[1]-17, method="smallsample"), alpha=0.05)  # +VC 
  
  exp(table.2.logit["DiD", "results"])
  exp(table.2.logit["DiD", "results"])/(1+exp(table.2.logit["DiD", "results"])) 
  table.2["DiD", "results"] 
  
### *********** CALCULATION OF PREDICTIONS ***************#
  
  logit.models.RQ2 <- lapply(RQ2.matched, FUN=function(data){
    clustered.logit.RQ2 <- miceadds::glm.cluster(data=data, formula=formula(model.2), cluster=data$LA, weights=data$weights, family = binomial(link = "logit"))
    logit.RQ2 <- glm(data=data, formula=formula(model.2), weights=data$weights, family = binomial(link = "logit"))
    return(list(clustered.logit.RQ2, logit.RQ2))
  })
  
  plain.logit <- lapply(RQ2.matched, FUN=function(data){
    dfs <- glm(data=data, formula=formula(model.2), weights=data$weights, family=binomial(link="logit"))
  })
  vcov.clustered.logit.RQ2 <- lapply(logit.RQ2, FUN=function(rr){ vcov(rr) }) 
  margins.ame.RQ2 <-lapply(seq_along(plain.logit), FUN=function(x) margins(model=plain.logit[[x]], data=RQ2.matched[[x]],vcov=vcov.clustered.logit.RQ2[[x]], variables="DiD"))
  save(margins.ame.RQ2, "margins.ame.RQ2",file="./Working/Final output/Margins_ame_RQ2.Rdata")
  
  AME.RQ2 <- mean(margins.ame.RQ2[[1]]$dydx_DiD)
  AME.RQ2.std.error <- sqrt(mean(margins.ame.RQ2[[1]]$Var_dydx_DiD))#/sqrt(nrow(RQ2.matched[[1]]))
  

# ************** logit model RQ4 ***********
  # run logit model for RQ4 given low incidence of escalations
  sum(RQ4.matched[[1]]$escalate==1)/dim(RQ4.matched[[1]])[1]
  
  relevel.vars <- function(df) { 
    levels(df$gender) <- c("0", "1", "2", "Missing", "Missing") 
    levels(df$ac.year) <- c("Missing","Missing","PRIM","SEC")
    levels(df$relev.year) <- c("2008","2009","2010","2015","2016","2017","2017")
    return(df)}
  RQ4.matched.relevelled <- lapply(RQ4.matched, function(x) relevel.vars(x))
  
  logit.RQ4 <- lapply(RQ4.matched.relevelled, FUN=function(data){
    miceadds::glm.cluster(data=data, 
                          formula=formula(model.4), 
                          cluster=data$LA, weights=data$weights, family = binomial(link = "logit"))
  })

  betas.logit.RQ4 <- lapply(logit.RQ4, FUN=function(rr){ coef(rr) }) 
  params.logit.RQ4 <- create.regression.parameters(logit.RQ4, "lm.cluster")
  table.4.logit <- summary(miceadds::pool_mi(qhat=params.logit.RQ4[[1]], se=params.logit.RQ4[[2]], dfcom = dim(RQ4.matched[[1]])[1]-17, method="smallsample"), alpha=0.05)   
  
  exp(table.4.logit["DiD", "results"])/(1+exp(table.4.logit["DiD", "results"])) 
  table.4["DiD", "results"] 
  
  vars.log <- lapply(logit.RQ4, FUN=function(rr){vcov(rr)})
  semod <- lapply(logit.RQ4, FUN=function(mm){
      summary(mm)[,"Std. Error"]
    } )
  
  summary(miceadds::pool_mi(qhat=betas.logit.RQ4, u=vars.log, dfcom = dim(RQ4.matched[[1]])[1]-17, method="smallsample")) 

  plain.logit.RQ4 <- lapply(RQ4.matched, FUN=function(data){
    dfs <- glm(data=data, formula=formula(model.4), weights=data$weights, family=binomial(link="logit"))
  })
  vcov.clustered.logit.RQ4 <- lapply(logit.RQ4, FUN=function(rr){ vcov(rr) }) 
  margins.ame.RQ4 <-lapply(seq_along(plain.logit.RQ4), FUN=function(x) margins(model=plain.logit.RQ4[[x]], data=RQ4.matched[[x]],vcov=vcov.clustered.logit.RQ4[[x]], variables="DiD"))
  AME.RQ4 <- mean(margins.ame.RQ4[[1]]$dydx_DiD)
  AME.RQ4.std.error <- sqrt(mean(margins.ame.RQ4[[1]]$Var_dydx_DiD))
  
  avg.marg.effects <- cbind(c(AME.RQ2, AME.RQ2.std.error),c(AME.RQ4, AME.RQ4.std.error))
  colnames(avg.marg.effects) <- c("RQ2", "RQ4")
  rownames(avg.marg.effects) <- c("Average marginal effect","Std.Error")
  
  write.csv(avg.marg.effects, "./Working/Final output/table average marginal effects RQ2 RQ4.csv", row.names = TRUE)
 
  # ********* REPORTING *************#
  
  # re-format and save as text file for table production in report. 
  table.1[,c(1:6)] <- round(table.1[,c(1:6)],4)
  table.2[,c(1:6)] <- round(table.2[,c(1:6)],4)
  table.3[,c(1:6)] <- round(table.3[,c(1:6)],4)
  table.4[,c(1:6)] <- round(table.4[,c(1:6)],4)
  table.5[,c(1:6)] <- round(table.5[,c(1:6)],4)
  
  write.csv(table.1, file="./Working/Final output/Main regression RQ1.csv")
  write.csv(table.2, file="./Working/Final output/Main regression RQ2.csv")
  write.csv(table.3, file="./Working/Final output/Main regression RQ3.csv")
  write.csv(table.4, file="./Working/Final output/Main regression RQ4.csv")
  write.csv(table.5, file="./Working/Final output/Main regression RQ5.csv")
  
  write.csv(table.2.logit, file="./Working/Final output/Main regression RQ2 logit link.csv")

  write.csv(table.4.logit, file="./Working/Final output/Main regression RQ4 logit link.csv")

  
  # ********* NOTE *************#
  # graphs and sensitivity analyses available upon request
  
