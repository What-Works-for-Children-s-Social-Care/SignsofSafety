# ********************** Multilevel models ****************

# Description: 
#   Based on 06_serial_correlation_testing results, run sensitivity for multilevel regressions

# This script will:
#     (a) Run Fixed Effects
#     (b) Run Random Effects
#     (c) Run the Hausman test to compare the two. 
#     (d) Compare the results with our main results in 07_main_regressions. 


rm(list=ls())

setwd("P:/")

source("./Working/00_Package_install.R")
source("./Working/00_functions.R")
source("./Working/00_common_parameters.R")

load("Working/matched populations/RQ1_matched.Rdata") # assessments data
load("Working/matched populations/RQ2_matched.Rdata") # ICPC data
load("Working/matched populations/RQ3_matched.Rdata") # Re-referral NFA data
load("Working/matched populations/RQ4_matched.Rdata") # Re-referral & escalate data
load("Working/matched populations/RQ5_matched.Rdata") # kinship care data


mean(RQ1.matched[[1]]$Duration)
prop.table(table(RQ2.matched[[1]]$ICPC))
prop.table(table(RQ3.matched[[1]]$re.referral))
prop.table(table(RQ4.matched[[1]]$escalate))
prop.table(table(RQ5.matched[[1]]$kinship.care))

# create appropriate ID variable
RQ1.matched <- lapply(seq_along(RQ1.matched), function(x) create.test.pop(RQ1.matched[[x]], "Assessment.start.date"))
RQ2.matched <- lapply(seq_along(RQ2.matched), function(x) create.test.pop(RQ2.matched[[x]], "CIN_ReferralDate"))
RQ3.matched <- lapply(seq_along(RQ3.matched), function(x) create.test.pop(RQ3.matched[[x]], "relev.NFA.date")) 
RQ4.matched <- lapply(seq_along(RQ4.matched), function(x) create.test.pop(RQ4.matched[[x]], "relev.NFA.date")) 
RQ5.matched <- lapply(seq_along(RQ5.matched), function(x) create.test.pop(RQ5.matched[[x]], "poc_start"))

dt.all <- list()
dt.all[[1]] <- data.table(RQ1.matched[[1]])
dt.all[[2]] <- data.table(RQ2.matched[[1]])
dt.all[[3]] <- data.table(RQ3.matched[[1]])
dt.all[[4]] <- data.table(RQ4.matched[[1]])
dt.all[[5]] <- data.table(RQ5.matched[[1]])

dt.all[[1]][,no.occur:=seq_len(.N), by="ID"] 
dt.all[[2]][,no.occur:=seq_len(.N), by="ID"]
dt.all[[3]][,no.occur:=seq_len(.N), by="ID"] 
dt.all[[4]][,no.occur:=seq_len(.N), by="ID"]
dt.all[[5]][,no.occur:=seq_len(.N), by="ID"]

df.1 <- sum(dt.all[[1]]$no.occur>1)
df.2 <- sum(dt.all[[2]]$no.occur>1)
df.3 <- sum(dt.all[[3]]$no.occur>1) 
df.4 <- sum(dt.all[[4]]$no.occur>1)
df.5 <- sum(dt.all[[5]]$no.occur>1)

# check for duplicates
sum(dt.all[[1]]$no.occur>1)/nrow(dt.all[[1]]) 
sum(dt.all[[2]]$no.occur>1)/nrow(dt.all[[2]])
sum(dt.all[[3]]$no.occur>1)/nrow(dt.all[[3]]) 
sum(dt.all[[4]]$no.occur>1)/nrow(dt.all[[4]])
sum(dt.all[[5]]$no.occur>1)/nrow(dt.all[[5]])

# check for reocurring children
sum(duplicated(RQ1.matched[[1]][,c("ID")]))/nrow(RQ1.matched[[1]])
sum(duplicated(RQ2.matched[[1]][,c("ID")]))/nrow(RQ2.matched[[1]])
sum(duplicated(RQ3.matched[[1]][,c("ID")]))/nrow(RQ3.matched[[1]])
sum(duplicated(RQ4.matched[[1]][,c("ID")]))/nrow(RQ4.matched[[1]])
sum(duplicated(RQ5.matched[[1]][,c("ID")]))/nrow(RQ5.matched[[1]]) 


# remove time invariant components for FE regression
model.1.varying <- remove.time.invariant(model.1)
model.2.varying <- remove.time.invariant(model.2)
model.3.varying <- remove.time.invariant(model.3)
model.4.varying <- remove.time.invariant(model.4)
model.5.varying <- remove.time.invariant(model.5)


## 1. Fixed effects model
    FE.1 <- plm.multiple.imp(RQ1.matched, model.1.varying, "within") 
    FE.2 <- plm.multiple.imp(RQ2.matched, model.2.varying, "within")
    FE.3 <- plm.multiple.imp(RQ3.matched, gsub("relev.year\\+","", model.3.varying), "within") 
    FE.4 <- plm.multiple.imp(RQ4.matched, gsub("relev.year\\+","", model.4.varying), "within") 
    FE.5 <- plm.multiple.imp(RQ5.matched, gsub("relev.year\\+","", model.5.varying), "within") 
    
  # create parameters for pooling
    summ.FE.1 <- create.regression.parameters(FE.1, model.type = "plm") 
    summ.FE.2 <- create.regression.parameters(FE.2, model.type = "plm") 
    summ.FE.3 <- create.regression.parameters(FE.3, model.type = "plm") 
    summ.FE.4 <- create.regression.parameters(FE.4, model.type = "plm") 
    summ.FE.5 <- create.regression.parameters(FE.5, model.type = "plm") 
    
  # FE Results
    table.FE.1 <- summary(miceadds::pool_mi(qhat=summ.FE.1[[1]], se=summ.FE.1[[2]], dfcom = df.1, method="smallsample")) 
    table.FE.2 <- summary(miceadds::pool_mi(qhat=summ.FE.2[[1]], se=summ.FE.2[[2]], dfcom = df.2, method="smallsample")) 
    table.FE.3 <- summary(miceadds::pool_mi(qhat=summ.FE.3[[1]], se=summ.FE.3[[2]], dfcom = df.3, method="smallsample")) 
    table.FE.4 <- summary(miceadds::pool_mi(qhat=summ.FE.4[[1]], se=summ.FE.4[[2]], dfcom = df.4, method="smallsample")) 
    table.FE.5 <- summary(miceadds::pool_mi(qhat=summ.FE.5[[1]], se=summ.FE.5[[2]], dfcom = df.5, method="smallsample")) 

    
## 2. Random effects model (intercept)
    RE.1 <- plm.multiple.imp(RQ1.matched, model.1, "random") 
    RE.2 <- plm.multiple.imp(RQ2.matched, model.2, "random")
    RE.3 <- plm.multiple.imp(RQ3.matched, gsub("relev.year\\+","", model.3),"random") # (collinear)
    RE.4 <- plm.multiple.imp(RQ4.matched, gsub("relev.year\\+","", model.4), "random")
    RE.5 <- plm.multiple.imp(RQ5.matched, gsub("relev.year\\+","", model.5), "random")
    

  # create parameters for pooling 
    summ.RE.1 <- create.regression.parameters(RE.1, model.type = "plm") 
    summ.RE.2 <- create.regression.parameters(RE.2, model.type = "plm") 
    summ.RE.3 <- create.regression.parameters(RE.3, model.type = "plm") 
    summ.RE.4 <- create.regression.parameters(RE.4, model.type = "plm") 
    summ.RE.5 <- create.regression.parameters(RE.5, model.type = "plm") 
    
    # RE Results
    table.RE.1 <- summary(miceadds::pool_mi(qhat=summ.RE.1[[1]], se=summ.RE.1[[2]], dfcom = dim(RQ1.matched[[1]])[1]-18, method="smallsample")) 
    table.RE.2 <- summary(miceadds::pool_mi(qhat=summ.RE.2[[1]], se=summ.RE.2[[2]], dfcom = dim(RQ2.matched[[1]])[1]-18, method="smallsample")) 
    table.RE.3 <- summary(miceadds::pool_mi(qhat=summ.RE.3[[1]], se=summ.RE.3[[2]], dfcom = dim(RQ3.matched[[1]])[1]-17, method="smallsample")) 
    table.RE.4 <- summary(miceadds::pool_mi(qhat=summ.RE.4[[1]], se=summ.RE.4[[2]], dfcom = dim(RQ4.matched[[1]])[1]-17, method="smallsample")) 
    table.RE.5 <- summary(miceadds::pool_mi(qhat=summ.RE.5[[1]], se=summ.RE.5[[2]], dfcom = dim(RQ5.matched[[1]])[1]-17, method="smallsample"))
    
    # Run Hausmann test 
    # phtest(FE.1[[1]], RE.1[[1]])
    hausman.test.results <- function(FE, RE) {
      haus <- phtest(FE, RE)
      return(list(haus$statistic,  haus$parameter[["df"]]))
    }
    hausman.results.1 <- lapply(seq_along(FE.1), function(x) hausman.test.results(FE.1[[x]], RE.1[[x]]))
    hausman.results.stat.1 <- c(hausman.results.1[[1]][[1]], hausman.results.1[[2]][[1]], hausman.results.1[[3]][[1]], hausman.results.1[[4]][[1]], hausman.results.1[[5]][[1]])
    hausman.results.df.1 <- c(hausman.results.1[[1]][[2]], hausman.results.1[[2]][[2]], hausman.results.1[[3]][[2]], hausman.results.1[[4]][[2]], hausman.results.1[[5]][[2]])
    
    hausman.results.2 <- lapply(seq_along(FE.2), function(x) hausman.test.results(FE.2[[x]], RE.2[[x]]))
    hausman.results.stat.2 <- c(hausman.results.2[[1]][[1]], hausman.results.2[[2]][[1]], hausman.results.2[[3]][[1]], hausman.results.2[[4]][[1]], hausman.results.2[[5]][[1]])
    hausman.results.df.2 <- c(hausman.results.2[[1]][[2]], hausman.results.2[[2]][[2]], hausman.results.2[[3]][[2]], hausman.results.2[[4]][[2]], hausman.results.2[[5]][[2]])
    
    hausman.results.3 <- lapply(seq_along(FE.3), function(x) hausman.test.results(FE.3[[x]], RE.3[[x]]))
    hausman.results.stat.3 <- c(hausman.results.3[[1]][[1]], hausman.results.3[[2]][[1]], hausman.results.3[[3]][[1]], hausman.results.3[[4]][[1]], hausman.results.3[[5]][[1]])
    hausman.results.df.3 <- c(hausman.results.3[[1]][[2]], hausman.results.3[[2]][[2]], hausman.results.3[[3]][[2]], hausman.results.3[[4]][[2]], hausman.results.3[[5]][[2]])
    
    hausman.results.4 <- lapply(seq_along(FE.4), function(x) hausman.test.results(FE.4[[x]], RE.4[[x]]))
    hausman.results.stat.4 <- c(hausman.results.4[[1]][[1]], hausman.results.4[[2]][[1]], hausman.results.4[[3]][[1]], hausman.results.4[[4]][[1]], hausman.results.4[[5]][[1]])
    hausman.results.df.4 <- c(hausman.results.4[[1]][[2]], hausman.results.4[[2]][[2]], hausman.results.4[[3]][[2]], hausman.results.4[[4]][[2]], hausman.results.4[[5]][[2]])
    
    hausman.results.5 <- lapply(seq_along(FE.5), function(x) hausman.test.results(FE.5[[x]], RE.5[[x]]))
    hausman.results.stat.5 <- c(hausman.results.5[[1]][[1]], hausman.results.5[[2]][[1]], hausman.results.5[[3]][[1]], hausman.results.5[[4]][[1]], hausman.results.5[[5]][[1]])
    hausman.results.df.5 <- c(hausman.results.5[[1]][[2]], hausman.results.5[[2]][[2]], hausman.results.5[[3]][[2]], hausman.results.5[[4]][[2]], hausman.results.5[[5]][[2]])
    
    
    hausman.1 <- miceadds::micombine.chisquare(hausman.results.stat.1, hausman.results.df.1[[1]])
    hausman.2 <- miceadds::micombine.chisquare(hausman.results.stat.2, hausman.results.df.2[[1]])
    hausman.3 <- miceadds::micombine.chisquare(hausman.results.stat.3, hausman.results.df.3[[1]])
    hausman.4 <- miceadds::micombine.chisquare(hausman.results.stat.4, hausman.results.df.4[[1]])
    hausman.5 <- miceadds::micombine.chisquare(hausman.results.stat.5, hausman.results.df.5[[1]])

    write.csv(hausman.1, "./Working/Final output/Hausman test RQ1.csv")
    write.csv(hausman.2, "./Working/Final output/Hausman test RQ2.csv")
    write.csv(hausman.3, "./Working/Final output/Hausman test RQ3.csv")
    write.csv(hausman.4, "./Working/Final output/Hausman test RQ4.csv")
    write.csv(hausman.5, "./Working/Final output/Hausman test RQ5.csv")
    
