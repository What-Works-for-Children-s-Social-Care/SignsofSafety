
# ********************** Coarsened Exact Matching (CEM) ****************

# Description: 
#   Apply Coarsened Exact Matching (CEM) to our multiply imputed datasets from 04_multiple imputation
#     1. Define matching variables
#     2. add missing categories for categorical variables 

# **********************************************************************

rm(list=ls())
setwd("P:/")
Sys.setenv(R_HISTFILE="P://Working")

source("./Working/00_Package_install.R")
source("./Working/00_functions.R")


# 0. load populations

load("Working/imputed populations/RQ1_imputed.Rdata") # assessments data
load("Working/imputed populations/RQ2_imputed.Rdata") # ICPC data
load("Working/imputed populations/RQ3_imputed.Rdata") # Re-referral NFA data
load("Working/imputed populations/RQ4_imputed.Rdata") # Re-referral & escalate data
load("Working/imputed populations/RQ5_imputed.Rdata") # kinship care data


# 1. ********** Define matching variables **********
## Coarseness of the variables: (as defined in the TP)
#   Gender: 0=male, 1=female, 2=other/unspecified
#   Age: 0-4, 5-12, 13-17
#   Ethnicity: major group --> research
#   Academic year: primary school years (Reception - Year 6), secondary school years (Year 7-13) 
#   Disabled: 0=No, 1=Yes
#   FSM/PPE: 0=No, 1=Yes
#   Unaccompanied Asylum Seeker: 0=No, 1=Yes
#   Number of previous CIN/CP plans: 0, >0
matching.vars <- c("Matched.group","postembed","gender","age.at","Ethnicity", "ac.year","disabled","lowinc","asylum","previous.CPP") 
  categorical.vars <- c("relev.year","Ethnicity", "gender", "ac.year","lowinc","cat.abuse","main.need") 
  num.vars <- c("age.at","disabled","asylum","previous.CPP") 
  LA.covars <- c("assessments","ofsted.rating","pop.density","primary.fsm","secondary.fsm","innovation","percentage.WBRI","timescales.review")
  all.covars.reg <- c(categorical.vars, num.vars, LA.covars)

# 2. ******* Prepare CEM - determine cutoffs & groupings ************

  ## 2a.) create groupings for CATEGORICAL variables

  # Ethnicity: groups are White, Mixed, Asian or Asian British, Black or Black British, Other ethnic groups 
  ethn.grp <- list(c("WBRI","WIRI","WIRT","WOTH", "WROM","WHIT"),c("MWBC", "MWBA", "MWAS", "MOTH","MIXD"),c("AIND",
                    "APKN","ABAN","AOTH","ASIA"),c("BCRB", "BAFR", "BOTH","BLAC"),c("CHNE","CHIN"),c("OOTH","AOEG"),c("NOBT","UNCL","REFU"),c("Missing")) 
  ac.year.grp <- list(c("Missing"),c("NONE"),c("PRIM"),c("SEC"))
  gend.grp <- list(c(0),c(1),c(2),c(3),c(0, 3, "Missing")) 
  FSM.grp <- list(c(0),c(1),c("Missing"))

  ## 2.b) create cutpoints for NUMERIC variables

    # treatment-comparator pairs: 
    tc.pairs <- c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5) # makes sure that we only use children in matched LAs as pairs for each pilot LA

    age.cut <- c(0,4.5, 12.5, 18)
    disab.cut <- c(0,0.5,1.5)
    UCAS.cut <- c(0,0.5,1.5)
    prev.pl.cut <- c(0,0.5,1.5) 
    time.period <- c(0,0.5,1.5) 

    # create list of matching vars that are in each list
    vars.imb.1 <- matching.vars[matching.vars %in% colnames(RQ1.list[[1]])]
    vars.imb.2 <- matching.vars[matching.vars %in% colnames(RQ2.list[[1]])]
    vars.imb.3 <- matching.vars[matching.vars %in% colnames(RQ3.list[[1]])]
    vars.imb.4 <- matching.vars[matching.vars %in% colnames(RQ4.list[[1]])]
    vars.imb.5 <- matching.vars[matching.vars %in% colnames(RQ5.list[[1]])]

  # ******** RQ1 ******************#
    
    imbalance(group=RQ1.list[[1]]$SofS.LA, data=RQ1.list[[1]][vars.imb.1])
    imbalance(group=RQ1.list[[1]]$SofS.LA, data=RQ1.list[[1]][vars.imb.1], 
              breaks=list(age.at=age.cut, disabled=disab.cut, 
                          Matched.group=tc.pairs, postembed=time.period), 
              grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp,
                            lowinc=FSM.grp)) 
    
    # ****** Conduct CEM *********** 
    
    # need to exclude the outcome variable 
    mat.RQ1 <- cem(treatment="SofS.LA", datalist=RQ1.list, drop=setdiff(colnames(RQ1.list[[1]]), matching.vars), 
                   cutpoints=list(age.at=age.cut, disabled=disab.cut, 
                                  Matched.group=tc.pairs,postembed=time.period), 
                   grouping=list(Ethnicity=ethn.grp, gender=gend.grp, 
                                 ac.year=ac.year.grp, lowinc=FSM.grp),
                   eval.imbalance = TRUE, L1.breaks=list(age.at=age.cut, disabled=disab.cut, 
                                                         Matched.group=tc.pairs, postembed=time.period), 
                   L1.grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp, lowinc=FSM.grp))

    # shows number of matches.
    mat.RQ1 
    mat.RQ1$match1$tab
    
    # create matched data set post CEM for overview 
    RQ1.matched <- create.matched.df(RQ1.list, mat.RQ1)
    save(RQ1.matched, "RQ1.matched", file="./Working/matched populations/RQ1_matched.Rdata" )
    
    # check for imbalance now when not coarsened:
    imb.1 <- imbalance(group=RQ1.list[[1]]$SofS.LA, data=RQ1.list[[1]][vars.imb.1])
    imb.1.b <- imbalance(group=RQ1.matched[[1]]$SofS.LA, data=RQ1.matched[[1]][vars.imb.1])
    
    balance.stats <- cbind("RQ1",imb.1$L1$L1,imb.1.b$L1$L1, imb.1$L1$LCS, imb.1.b$L1$LCS)
    colnames(balance.stats) <- c("RQ","L1 measure, pre CEM", "L1 measure, post CEM","LCS, pre CEM", "LCS, post CEM")


  # ******** RQ2 ******************#

    ## pre matching imbalance checks: 
    imbalance(group=RQ2.list[[1]]$SofS.LA, data=RQ2.list[[1]][vars.imb.2])
    imbalance(group=RQ2.list[[1]]$SofS.LA, data=RQ2.list[[1]][vars.imb.2], 
          breaks=list(age.at=age.cut, disabled=disab.cut, 
                      Matched.group=tc.pairs, postembed=time.period), 
          grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp,
                        lowinc=FSM.grp)) #  uses grouping as for the CEM 

    # ****** Conduct CEM *********** 

    # need to exclude the outcome variable 
      mat.RQ2 <- cem(treatment="SofS.LA", datalist=RQ2.list, drop=setdiff(colnames(RQ2.list[[1]]), matching.vars), 
                 cutpoints=list(age.at=age.cut, disabled=disab.cut, 
                               Matched.group=tc.pairs,postembed=time.period), 
                 grouping=list(Ethnicity=ethn.grp, gender=gend.grp, 
                              ac.year=ac.year.grp, lowinc=FSM.grp),
                eval.imbalance = TRUE, L1.breaks=list(age.at=age.cut, disabled=disab.cut, 
                                                      Matched.group=tc.pairs, postembed=time.period), 
                 L1.grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp, lowinc=FSM.grp))

       mat.RQ2 

      # create matched data set post CEM for overview 
      RQ2.matched <- create.matched.df(RQ2.list, mat.RQ2)
      save(RQ2.matched, "RQ2.matched", file="./Working/matched populations/RQ2_matched.Rdata" )

      # check for imbalance now when not coarsened:
      imb.2 <- imbalance(group=RQ2.list[[1]]$SofS.LA, data=RQ2.list[[1]][vars.imb.2])
      imb.2.b <- imbalance(group=RQ2.matched[[1]]$SofS.LA, data=RQ2.matched[[1]][vars.imb.2])

      balance.stats <- rbind(balance.stats, cbind("RQ2",imb.2$L1$L1,imb.2.b$L1$L1, imb.2$L1$LCS, imb.2.b$L1$LCS))
      
      ## Summary statistics of covariates that we don't apply the CEM on: 
      tapply(RQ2.matched[[1]]$cat.abuse, RQ2.matched[[1]]$SofS.LA, summary) 
      tapply(RQ2.matched[[1]]$relev.year, RQ2.matched[[1]]$SofS.LA, summary)
      tapply(RQ2.matched[[1]]$main.need, RQ2.matched[[1]]$SofS.LA, summary) 
      tapply(RQ2.matched[[1]]$elev.risk, RQ2.matched[[1]]$SofS.LA, summary) 
      tapply(RQ2.matched[[1]]$age.at, RQ2.matched[[1]]$SofS.LA, summary) 
     
     
  # ******** RQ3 ******************#
     
     ## pre matching imbalance checks: 
     imbalance(group=RQ3.list[[1]]$SofS.LA, data=RQ3.list[[1]][vars.imb.3])
     imbalance(group=RQ3.list[[1]]$SofS.LA, data=RQ3.list[[1]][vars.imb.3], 
               breaks=list(age.at=age.cut, disabled=disab.cut, 
                           Matched.group=tc.pairs, postembed=time.period), 
               grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp,
                             lowinc=FSM.grp)) 
     
     # ****** Conduct CEM *********** 
     
     # need to exclude the outcome variable 
     mat.RQ3 <- cem(treatment="SofS.LA", datalist=RQ3.list, drop=setdiff(colnames(RQ3.list[[1]]), matching.vars), 
                    cutpoints=list(age.at=age.cut, disabled=disab.cut, 
                                   Matched.group=tc.pairs,postembed=time.period), 
                    grouping=list(Ethnicity=ethn.grp, gender=gend.grp, 
                                  ac.year=ac.year.grp, lowinc=FSM.grp),
                    eval.imbalance = TRUE, L1.breaks=list(age.at=age.cut, disabled=disab.cut, 
                                                          Matched.group=tc.pairs, postembed=time.period), 
                    L1.grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp, lowinc=FSM.grp))

     mat.RQ3 
     
     # create matched data set post CEM for overview
     RQ3.matched <- create.matched.df(RQ3.list, mat.RQ3)
     save(RQ3.matched, "RQ3.matched", file="./Working/matched populations/RQ3_matched.Rdata" )
     
     # check for imbalance now when not coarsened:
     imb.3 <- imbalance(group=RQ3.list[[1]]$SofS.LA, data=RQ3.list[[1]][vars.imb.3])
     imb.3.b <- imbalance(group=RQ3.matched[[1]]$SofS.LA, data=RQ3.matched[[1]][vars.imb.3])
     # higher LCS, less L1. --> good
     balance.stats <- rbind(balance.stats, cbind("RQ3",imb.3$L1$L1,imb.3.b$L1$L1, imb.3$L1$LCS, imb.3.b$L1$LCS))
     
     ## Summary statistics of covariates that we don't apply the CEM on: 
     tapply(RQ3.matched[[1]]$cat.abuse, RQ3.matched[[1]]$SofS.LA, summary) 
     tapply(RQ3.matched[[1]]$relev.year, RQ3.matched[[1]]$SofS.LA, summary) 
     tapply(RQ3.matched[[1]]$main.need, RQ3.matched[[1]]$SofS.LA, summary) 
     tapply(RQ3.matched[[1]]$elev.risk, RQ3.matched[[1]]$SofS.LA, summary) 
     tapply(RQ3.matched[[1]]$age.at, RQ3.matched[[1]]$SofS.LA, summary) 
     

  # ******** RQ4 ******************#
     
     ## pre matching imbalance checks: 
     imbalance(group=RQ4.list[[1]]$SofS.LA, data=RQ4.list[[1]][vars.imb.4])
     imbalance(group=RQ4.list[[1]]$SofS.LA, data=RQ4.list[[1]][vars.imb.4], 
               breaks=list(age.at=age.cut, disabled=disab.cut, 
                           Matched.group=tc.pairs, postembed=time.period), 
               grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp,
                             lowinc=FSM.grp)) #  uses grouping as for the CEM 
     
     # ****** Conduct CEM *********** 
     
     # need to exclude the outcome variable 
     mat.RQ4 <- cem(treatment="SofS.LA", datalist=RQ4.list, drop=setdiff(colnames(RQ4.list[[1]]), matching.vars), 
                    cutpoints=list(age.at=age.cut, disabled=disab.cut, 
                                   Matched.group=tc.pairs,postembed=time.period), 
                    grouping=list(Ethnicity=ethn.grp, gender=gend.grp, 
                                  ac.year=ac.year.grp, lowinc=FSM.grp),
                    eval.imbalance = TRUE, L1.breaks=list(age.at=age.cut, disabled=disab.cut, 
                                                          Matched.group=tc.pairs, postembed=time.period), 
                    L1.grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp, lowinc=FSM.grp))
      mat.RQ4 
     
     # create matched data set post CEM for overview 
      RQ4.matched <- create.matched.df(RQ4.list, mat.RQ4)
    
      save(RQ4.matched, "RQ4.matched", file="./Working/matched populations/RQ4_matched.Rdata" )
     
     # check for imbalance now when not coarsened:
     imb.4 <- imbalance(group=RQ4.list[[1]]$SofS.LA, data=RQ4.list[[1]][vars.imb.4])
     imb.4.b <- imbalance(group=RQ4.matched[[1]]$SofS.LA, data=RQ4.matched[[1]][vars.imb.4])
     # lower L1, higher LCS
     balance.stats <- rbind(balance.stats, cbind("RQ4",imb.4$L1$L1,imb.4.b$L1$L1, imb.4$L1$LCS, imb.4.b$L1$LCS))
     
     ## Summary statistics of covariates that we don't apply the CEM on: 
     tapply(RQ4.matched[[1]]$cat.abuse, RQ4.matched[[1]]$SofS.LA, summary) 
     tapply(RQ4.matched[[1]]$relev.year, RQ4.matched[[1]]$SofS.LA, summary) 
     tapply(RQ4.matched[[1]]$main.need, RQ4.matched[[1]]$SofS.LA, summary) 
     tapply(RQ4.matched[[1]]$elev.risk, RQ4.matched[[1]]$SofS.LA, summary) 
     tapply(RQ4.matched[[1]]$age.at, RQ4.matched[[1]]$SofS.LA, summary) 
     
     
  # ******** RQ5 ******************#
     ## pre matching imbalance checks: 
     imbalance(group=RQ5.list[[1]]$SofS.LA, data=RQ5.list[[1]][vars.imb.5])
     imbalance(group=RQ5.list[[1]]$SofS.LA, data=RQ5.list[[1]][vars.imb.5], 
               breaks=list(age.at=age.cut, disabled=disab.cut, asylum=UCAS.cut,
                           Matched.group=tc.pairs, postembed=time.period), 
               grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp,
                             lowinc=FSM.grp)) #  uses grouping as for the CEM 
     
     # ****** Conduct CEM *********** 
     
     # need to exclude the outcome variable 
     mat.RQ5 <- cem(treatment="SofS.LA", datalist=RQ5.list, drop=setdiff(colnames(RQ5.list[[1]]), matching.vars), 
                    cutpoints=list(age.at=age.cut, disabled=disab.cut, asylum=UCAS.cut,
                                   Matched.group=tc.pairs,postembed=time.period), 
                    grouping=list(Ethnicity=ethn.grp, gender=gend.grp, 
                                  ac.year=ac.year.grp, lowinc=FSM.grp),
                    eval.imbalance = TRUE, L1.breaks=list(age.at=age.cut, disabled=disab.cut, asylum=UCAS.cut, 
                                                          Matched.group=tc.pairs, postembed=time.period),
                    L1.grouping=list(Ethnicity=ethn.grp, gender=gend.grp, ac.year=ac.year.grp, lowinc=FSM.grp))

     # shows number of matches.
     mat.RQ5 
     
     # create matched data set post CEM for overview 
     RQ5.matched <- create.matched.df(RQ5.list, mat.RQ5)
     
     save(RQ5.matched, "RQ5.matched", file="./Working/matched populations/RQ5_matched.Rdata" )
     
     # check for imbalance now when not coarsened:
     imb.5 <- imbalance(group=RQ5.list[[1]]$SofS.LA, data=RQ5.list[[1]][vars.imb.5])
     imb.5.b <- imbalance(group=RQ5.matched[[1]]$SofS.LA, data=RQ5.matched[[1]][vars.imb.5])
     # lower L1, higher LCS --> good. 
     balance.stats <- rbind(balance.stats, cbind("RQ5",imb.5$L1$L1,imb.5.b$L1$L1, imb.5$L1$LCS, imb.5.b$L1$LCS))
     balance.stats <- as.data.frame(balance.stats)
     balance.stats[,2:5] <- apply(balance.stats[,2:5], 2, function(x) as.numeric(x))
     balance.stats[,2:5] <- apply(balance.stats[,2:5], 2, function(x) round(x, 2))

     write.csv(balance.stats, file="./Working/Final output/L1 imbalance statistics.csv")
     
     ################ Re-weight 
     
     # Because we use the same comparator LAs as multiple matches for LAs, some observations are repeated
     # We need to weight to take into account the duplication:
     # - we generate individual weights (based on the inverse of the number of duplicates)
     # - we generate frequency tables weighted by the individual weights
     # - we calculate the weighted T/C frequency table of the matched strata (in cem) and the weighted T/C frequency table overall
     # - we generate new stata weights
     # - we multiply strata weights by individual weights to get the "composite.weights"
     RQ1.matched.reweighted <- lapply(RQ1.matched, function(x) make.composite.weights(x, "Assessment.start.date"))
     RQ2.matched.reweighted <- lapply(RQ2.matched, function(x) make.composite.weights(x, "CIN_ReferralDate"))
     RQ3.matched.reweighted <- lapply(RQ3.matched, function(x) make.composite.weights(x, "relev.NFA.date"))
     RQ4.matched.reweighted <- lapply(RQ4.matched, function(x) make.composite.weights(x, "relev.NFA.date"))
     RQ5.matched.reweighted <- lapply(RQ5.matched, function(x) make.composite.weights(x, "poc_start"))
     
     RQ1.matched <- RQ1.matched.reweighted
     RQ2.matched <- RQ2.matched.reweighted
     RQ3.matched <- RQ3.matched.reweighted
     RQ4.matched <- RQ4.matched.reweighted
     RQ5.matched <- RQ5.matched.reweighted
     
     save(RQ1.matched, "RQ1.matched", file="./Working/matched populations/RQ1_matched.Rdata")
     save(RQ2.matched, "RQ2.matched", file="./Working/matched populations/RQ2_matched.Rdata")
     save(RQ3.matched, "RQ3.matched", file="./Working/matched populations/RQ3_matched.Rdata")
     save(RQ4.matched, "RQ4.matched", file="./Working/matched populations/RQ4_matched.Rdata")
     save(RQ5.matched, "RQ5.matched", file="./Working/matched populations/RQ5_matched.Rdata")
     

  ## ****** Summary statistics of covariates & outcome variable ************** # 
      # functions drawn from 00_functions
    
    RQ1.summary.stats <- run.summary.stats(RQ1.matched, "Duration")
    RQ2.summary.stats <- run.summary.stats(RQ2.matched, "ICPC")
    RQ3.summary.stats <- run.summary.stats(RQ3.matched, "re.referral")
    RQ4.summary.stats <- run.summary.stats(RQ4.matched, "escalate")
    RQ5.summary.stats <- run.summary.stats(RQ5.matched, "kinship.care")
    
