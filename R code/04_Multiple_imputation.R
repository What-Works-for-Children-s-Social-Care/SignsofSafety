
#************************** Multiple imputation ********************************************# 
# Description: 
#   Conduct multiple imputation for missing data 
#     1. reformat data (categorical vs. numerical, etc.)
#     2. add missing categories for categorical variables 
#     3. Drop entries w/o outcome data
#     4. Drop columns if there is >30% missing. 
#     5. Do checks pre multiple imputation
#     6. Conduct multiple imputation via mice. 


rm(list=ls())
setwd("P:/")
Sys.setenv(R_HISTFILE="P://Working")

source("./Working/00_Package_install.R")
source("./Working/00_functions.R")

# 0. load populations

  load("Working/baseline populations/RQ1_assessments.Rdata") # assessments data
  load("Working/baseline populations/RQ2_ICPC.Rdata") # ICPC data
  load("Working/baseline populations/RQ3_re-referral_NFA.Rdata") # Re-referral NFA data
  load("Working/baseline populations/RQ4_re-referral_escalate.Rdata") # Re-referral & escalate data
  load("Working/baseline populations/RQ5_kinship_care.Rdata") # kinship care data
  
  # create list of all outcomes
  all.pops <- list(RQ1.assessment, RQ2.ICPC, RQ3.ref.NFA, RQ4.ref.esc, RQ5.kinship)
  names(all.pops) <- RQs <-  c("RQ1", "RQ2", "RQ3", "RQ4", "RQ5")
  for (i in seq_along(all.pops)) {
    all.pops[[i]] <- as.data.frame(all.pops[[i]])
  }
  
# 1. define categorical variables 
  
  individ.covars <- c("disabled","asylum","lowinc","age.at", "ac.year","years.since",
                      "gender","Ethnicity","main.need","cat.abuse","elev.risk","previous.CPP")
  
  categ.vars <- c("gender","Ethnicity","main.need","cat.abuse","lowinc","ac.year")
  num.vars <- c("disabled","asylum","age.at","elev.risk","previous.CPP")
  
    # look at differences in covariates
    for (i in seq_along(all.pops)) {
      print(names(all.pops[i]))
      print(setdiff(individ.covars, colnames(all.pops[[i]])))
    }

# 2. add missing categories for all categorical variables: 
  
    all.pops <- lapply(seq_along(all.pops), function(x) all.pops[[x]] <- add.missing(categ.vars, all.pops[[x]]))
    
    for (i in seq_along(all.pops)) {
      print(names(all.pops)[[i]])
      for (v in categ.vars) {
        print(v)
      print(table(all.pops[[i]][[v]], useNA = 'always'))
      }
    }
    
  # factorise categorical data. 
    for (i in seq_along(all.pops)){
      all.pops[[i]] <- factorise(c(categ.vars,"relev.year","disabled","asylum","years.since"), all.pops[[i]])     
    }

    
  # 3. ** Drop entries without outcome data **
    outcome.vars <- c("Duration","ICPC","re.referral","escalate","kinship.care")
    for (i in seq_along(all.pops)) {
      print(sum(is.na(all.pops[[i]][outcome.vars[i]])))
      all.pops[[i]] <- all.pops[[i]][!is.na(all.pops[[i]][outcome.vars[i]]),]
    }
    
  # 4. ** Drop columns if there are > 30% missing (NA): **
    all.pops <- lapply(seq_along(all.pops), function(x) {
                                            print(RQs[[x]])
                                            all.pops[[x]] <- check.30.pct(all.pops[[x]])
      })
    

    # 4.b) repeat for categorical variables if > 30%=="Missing" 

    for (x in 1:5) {
          print(RQs[[x]])
          for (j in categ.vars) {
            if(sum(all.pops[[x]][[j]]=="Missing")/nrow(all.pops[[x]])>=0.3) {
              print(paste("too little observations for: ", j))
              print(sum(is.na(all.pops[[x]][[j]]))/nrow(all.pops[[x]]))
              all.pops[[x]] <- all.pops[[x]][,-which(colnames(all.pops[[x]]) %in% j)]
            }
          }
    }

      stopifnot(sapply(all.pops[[1]], function(x) sum(is.na(x))/nrow(all.pops[[1]])<0.3))
      stopifnot(sapply(all.pops[[2]], function(x) sum(is.na(x))/nrow(all.pops[[2]])<0.3))
      stopifnot(sapply(all.pops[[3]], function(x) sum(is.na(x))/nrow(all.pops[[3]])<0.3))
      stopifnot(sapply(all.pops[[4]], function(x) sum(is.na(x))/nrow(all.pops[[4]])<0.3))
      stopifnot(sapply(all.pops[[5]], function(x) sum(is.na(x))/nrow(all.pops[[5]])<0.3))
      
# Create predictor matrix
      pred.mat <- function(data.fr) {
        pm.1 <- matrix(1, ncol(data.fr), ncol(data.fr))
        pm.1 <- pm.1 - diag(1, ncol(data.fr), ncol(data.fr))
        loc.child.id <- which(colnames(data.fr) %in% "child.ID") # find child.ID column
        pm.1[,loc.child.id] <- c(rep(0, ncol(data.fr))) # make child.ID PM 0 
        return(pm.1)
      }
      
      pred.mat.1 <- pred.mat(all.pops[[1]])
      pred.mat.2 <- pred.mat(all.pops[[2]])
      pred.mat.3 <- pred.mat(all.pops[[3]])
      pred.mat.4 <- pred.mat(all.pops[[4]])
      pred.mat.5 <- pred.mat(all.pops[[5]])
      
      
    # 5. ** Initialise multiple imputation ** 
      # 5.1. 
      init=mice(all.pops[[1]],m=1, maxit=1)
      meth=init$method
      
      # choose method by which to impute by: 
      meth[c("gender","Ethnicity","lowinc","ac.year","Duration","Assessment.start.date")] <- ""
      meth[c("child.ID","LA","postembed","DiD","SofS.LA","relev.year","years.since","Matched.group")] <- ""
      meth[c("ofsted.rating","assessments","pop.density","primary.fsm","secondary.fsm","innovation","percentage.WBRI","timescales.review")] <-""
      meth[c("age.at")] <- "pmm" 
      meth[c("disabled")] <- "logreg"   

      # impute: 
      RQ1 <- mice(all.pops[[1]], method=meth, m=5, maxit=5, seed=1234, predictorMatrix = pred.mat.1)
      RQ1$loggedEvents

      # check for convergence with higher number of iterations: 
      # imp.40 <- mice.mids(RQ1, maxit=35, print=FALSE)
      # png(file="./Working/imputed populations/imp40_1.png",width=600, height=350)
      # plot(imp.40)
      # dev.off() # avoid graphics error
      
      # create list of imputed dfs
      RQ1.list <- list()
      for (i in 1:5) {
        RQ1.list[[i]] <- complete(RQ1, i)
        RQ1.list[[i]]$disabled <- as.numeric(as.character(RQ1.list[[i]]$disabled)) 
      }
      
      # save imputed list
      save(RQ1.list, "RQ1.list", file="./Working/imputed populations/RQ1_imputed.Rdata")
      
  
  # 5.2. 
      init=mice(all.pops[[2]],m=1, maxit=1)
      meth.2=init$method
      
      # choose method by which to impute by 
        meth.2[c("gender","Ethnicity","lowinc","ac.year","ICPC","main.need","CIN_ReferralDate")] <- ""
        meth.2[c("child.ID","LA","postembed","DiD","SofS.LA","relev.year","years.since","Matched.group")] <- ""
        meth.2[c("ofsted.rating","assessments","pop.density","primary.fsm","secondary.fsm","innovation","percentage.WBRI","timescales.review")] <-""
        meth.2[c("age.at")] <- "pmm"
        meth.2[c("disabled")] <- "logreg"  
      
      # impute: 
      RQ2 <- mice(all.pops[[2]], method=meth.2, m=5, maxit=5, seed=1234, predictorMatrix = pred.mat.2) 
      RQ2$loggedEvents
      
      # check for convergence with higher number of iterations: 
      # imp.40.2 <- mice.mids(RQ2, maxit=35, print=FALSE)
      # png(file="./Working/imputed populations/imp40_2.png",width=600, height=350)
      # plot(imp.40.2)
      # dev.off() # avoid graphics error
      
      # create list of imputed dfs
      RQ2.list <- list()
      for (i in 1:5) {
        RQ2.list[[i]] <- complete(RQ2, i)
        RQ2.list[[i]]$disabled <- as.numeric(as.character(RQ2.list[[i]]$disabled)) 
      }
      
      # save imputed list
      save(RQ2.list, "RQ2.list", file = "./Working/imputed populations/RQ2_imputed.Rdata")
    
        
    # 5.3. 
      init=mice(all.pops[[3]],m=1, maxit=1)
      meth.3=init$method
      
      # choose method by which to impute by.
      meth.3[c("gender","Ethnicity","lowinc","ac.year","re.referral","elev.risk","CIN_ReferralDate","relev.NFA.date")] <- ""
      meth.3[c("child.ID","LA","postembed","DiD","SofS.LA","relev.year","years.since","Matched.group")] <- ""
      meth.3[c("ofsted.rating","assessments","pop.density","primary.fsm","secondary.fsm","innovation","percentage.WBRI","timescales.review")] <-""
      meth.3[c("age.at")] <- "pmm" 
      meth.3[c("disabled")] <- "logreg"   
      
      # impute: 
      RQ3 <- mice(all.pops[[3]], method=meth.3, m=5, maxit=5, seed=1234, predictorMatrix = pred.mat.3)
      RQ3$loggedEvents

      # check for convergence with higher number of iterations: 
      # imp.40.3 <- mice.mids(RQ3, maxit=35, print=FALSE)
      # png(file="./Working/imputed populations/imp40_3.png",width=600, height=350)
      # plot(imp.40.3)
      # dev.off() # avoid graphics error
      
      # create list of imputed dfs
      RQ3.list <- list()
      for (i in 1:5) {
        RQ3.list[[i]] <- complete(RQ3, i)
        RQ3.list[[i]]$disabled <- as.numeric(as.character(RQ3.list[[i]]$disabled)) 
      }
      
      # save imputed list
      save(RQ3.list, "RQ3.list", file="./Working/imputed populations/RQ3_imputed.Rdata")
      

    # 5.4. 
      init=mice(all.pops[[4]],m=1, maxit=1)
      meth.4=init$method
      
      # choose method by which to impute by. Usually these are: 
      meth.4[c("gender","Ethnicity","lowinc","ac.year","escalate","elev.risk","relev.NFA.date")] <- ""
      meth.4[c("child.ID","LA","postembed","DiD","SofS.LA","relev.year","years.since","Matched.group")] <- ""
      meth.4[c("ofsted.rating","assessments","pop.density","primary.fsm","secondary.fsm","innovation","percentage.WBRI","timescales.review")] <-""
      meth.4[c("age.at")] <- "pmm" 
      meth.4[c("disabled")] <- "logreg" 
      
      # impute: 
      RQ4 <- mice(all.pops[[4]], method=meth.4, m=5, maxit=5, seed=1234, predictorMatrix = pred.mat.4)
      RQ4$loggedEvents
      
      # check for convergence with higher number of iterations: 
      # imp.40.4 <- mice.mids(RQ4, maxit=35, print=FALSE)
      # png(file="./Working/imputed populations/imp40_4.png",width=600, height=350)
      # plot(imp.40.4)
      # dev.off() # avoid graphics error
      
      # create list of imputed dfs
      RQ4.list <- list()
      for (i in 1:5) {
        RQ4.list[[i]] <- complete(RQ4, i)
        RQ4.list[[i]]$disabled <- as.numeric(as.character(RQ4.list[[i]]$disabled))  
      }
      
      # save imputed list
      save(RQ4.list, "RQ4.list", file="./Working/imputed populations/RQ4_imputed.Rdata")
      
      
    # 5.5. 
      all.pops[[5]]$child.ID <- as.character(all.pops[[5]]$child.ID) 
      init=mice(all.pops[[5]],m=1, maxit=1)
      meth.5=init$method
      
      # choose method by which to impute by.
      meth.5[c("gender","Ethnicity","lowinc","ac.year","kinship.care","poc_start")] <- ""
      meth.5[c("child.ID","LA","postembed","DiD","SofS.LA","relev.year","years.since","Matched.group")] <- ""
      meth.5[c("ofsted.rating","assessments","pop.density","primary.fsm","secondary.fsm","innovation","percentage.WBRI","timescales.review")] <-""
      meth.5[c("age.at")] <-  "pmm" 
      meth.5[c("disabled","asylum")] <- "logreg"   
      
      # impute: 
      RQ5 <- mice(all.pops[[5]], method=meth.5, m=5, maxit=5, seed=1234, predictorMatrix = pred.mat.5)
      RQ5$loggedEvents
      
      # check for convergence with higher number of iterations: 
      # imp.40.5 <- mice.mids(RQ5, maxit=35, print=FALSE)
      # png(file="./Working/imputed populations/imp40_5.png",width=600, height=350)
      # plot(imp.40.5)
      # dev.off() # avoid graphics error
      
      # create list of imputed dfs
      RQ5.list <- list()
      for (i in 1:5) {
        RQ5.list[[i]] <- complete(RQ5, i)
        RQ5.list[[i]]$disabled <- as.numeric(as.character(RQ5.list[[i]]$disabled)) 
        RQ5.list[[i]]$asylum <- as.numeric(as.character(RQ5.list[[i]]$asylum))
      }
      
      # save imputed list
      save(RQ5.list, "RQ5.list", file="./Working/imputed populations/RQ5_imputed.Rdata")
      