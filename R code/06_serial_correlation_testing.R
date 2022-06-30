### ** CONDUCT BREUSCH-GODFREY TEST FOR SERIAL CORRELATION *******

# Run tests for serial correlation of observations 

rm(list=ls())

setwd("P:/")

source("./Working/00_Package_install.R")
source("./Working/00_functions.R")
source("./Working/00_common_parameters.R")


# 0. load populations

load("Working/matched populations/RQ1_matched.Rdata") # assessments data
load("Working/matched populations/RQ2_matched.Rdata") # ICPC data
load("Working/matched populations/RQ3_matched.Rdata") # Re-referral NFA data
load("Working/matched populations/RQ4_matched.Rdata") # Re-referral & escalate data
load("Working/matched populations/RQ5_matched.Rdata") # kinship care data


create.test.pop <- function(df, time.var) {
  df.new <- data.table(df)
  df.new[,ID:=.GRP, by=.(child.ID, LA, Matched.group)] 
  df.new[,occur:=seq_len(.N), by=ID] 
  df.new[,n.occur:=max(occur), by=ID]
  df.new <- as.data.frame(df.new)
  return(df.new)
} 

RQ1 <- lapply(seq_along(RQ1.matched), function(x) create.test.pop(RQ1.matched[[x]], "Assessment.start.date"))
RQ2 <- lapply(seq_along(RQ2.matched), function(x) create.test.pop(RQ2.matched[[x]], "CIN_ReferralDate"))
RQ3 <- lapply(seq_along(RQ3.matched), function(x) create.test.pop(RQ3.matched[[x]], "relev.NFA.date")) 
RQ4 <- lapply(seq_along(RQ4.matched), function(x) create.test.pop(RQ4.matched[[x]], "relev.NFA.date")) 
RQ5 <- lapply(seq_along(RQ5.matched), function(x) create.test.pop(RQ5.matched[[x]], "poc_start"))


# determine complete model: 
LA.covars <- c("assessments","ofsted.rating","pop.density","primary.fsm","secondary.fsm",
               "innovation", "percentage.WBRI","timescales.review")

individ.covars <- c("disabled","asylum","lowinc","age.at", "ac.year",
                    "gender","Ethnicity","main.need","cat.abuse","elev.risk","previous.CPP")

individ.covars.in.common <- c("disabled","lowinc","age.at","ac.year","gender","Ethnicity") # these are the ones each of datasets have. 

# define which covars that are not common to all are in each RQ
RQ1.covars <- "" 
RQ2.covars <- ""
RQ3.covars <- c("elev.risk")
RQ4.covars <- c("elev.risk")
RQ5.covars <- "" 


# RUN BREUSCH-GODFREY TESTS
RQ1.BG <- list()
for (i in seq_along(RQ1)) {
  RQ1.BG[[i]] <- pbgtest(plm(model.1, data=pdata.frame(RQ1[[i]], index=c("ID","Assessment.start.date")), 
              index=c("ID","Assessment.start.date"), model="pooling", weights=RQ1[[i]]$"weights"),
          order=1, order.by=RQ1[[i]]$"Assessment.start.date")
}  
dk.1 <- c(RQ1.BG[[1]]$statistic, RQ1.BG[[2]]$statistic, RQ1.BG[[3]]$statistic, RQ1.BG[[4]]$statistic,
        RQ1.BG[[5]]$statistic)
df <- 1


RQ2.BG <- list()

  for (i in 1:5){
  RQ2.BG[[i]] <- pbgtest(plm(model.2, data=pdata.frame(RQ2[[i]], index=c("ID","CIN_ReferralDate")), 
                index=c("ID","CIN_ReferralDate"), model="pooling", weights=RQ2[[i]]$"weights"),
                order=1, order.by=RQ2[[i]]$"CIN_ReferralDate")
  }
  dk.2 <- c(RQ2.BG[[1]]$statistic, RQ2.BG[[2]]$statistic, RQ2.BG[[3]]$statistic, 
            RQ2.BG[[4]]$statistic, RQ2.BG[[5]]$statistic)

# RQ3:
  
  RQ3.BG <- list()
  for (i in 1:5){
    RQ3.BG[[i]] <- pbgtest(plm(model.3, data=pdata.frame(RQ3[[i]], index=c("ID","relev.NFA.date")), 
                               index=c("ID","relev.NFA.date"), model="pooling", weights=RQ3[[i]]$"weights"),
                           order=1, order.by=RQ3[[i]]$"relev.NFA.date")
  }
  dk.3 <- c(RQ3.BG[[1]]$statistic, RQ3.BG[[2]]$statistic, RQ3.BG[[3]]$statistic, 
            RQ3.BG[[4]]$statistic, RQ3.BG[[5]]$statistic)


# RQ4:
  RQ4.BG <- list()
  for (i in 1:5) {
    RQ4.BG[[i]] <-pbgtest(plm(model.4, data=pdata.frame(RQ4[[i]], index=c("ID","relev.NFA.date")), 
                              index=c("ID","relev.NFA.date"), model="pooling", weights=RQ4[[i]]$"weights"),
                          order=1, order.by=RQ4[[i]]$"relev.NFA.date")
  }
  dk.4 <- c(RQ4.BG[[1]]$statistic, RQ4.BG[[2]]$statistic, RQ4.BG[[3]]$statistic, 
            RQ4.BG[[4]]$statistic, RQ4.BG[[5]]$statistic)
  
# RQ5:
  RQ5.BG <- list()
  for (i in 1:5) {
    RQ5.BG[[i]] <- pbgtest(plm(model.5, data=pdata.frame(RQ5[[i]], index=c("ID","poc_start")), 
                    index=c("ID","poc_start"), model="pooling", weights=RQ5[[i]]$"weights"),
                    order=1, order.by=RQ5[[i]]$"poc_start")
  }
  dk.5 <- c(RQ5.BG[[1]]$statistic, RQ5.BG[[2]]$statistic, RQ5.BG[[3]]$statistic, 
            RQ5.BG[[4]]$statistic, RQ5.BG[[5]]$statistic)
  

## Results:
BG.test.RQ1 <- micombine.chisquare(dk.1,df, display=TRUE, version=1) 
BG.test.RQ2 <- micombine.chisquare(dk.2,df, display=TRUE, version=1)
BG.test.RQ3 <- micombine.chisquare(dk.3,df, display=TRUE, version=1)
BG.test.RQ4 <- micombine.chisquare(dk.4,df, display=TRUE, version=1) 
BG.test.RQ5 <- micombine.chisquare(dk.5,df, display=TRUE, version=1) 


BGtest <- as.data.frame(cbind(c("RQ1", "RQ2", "RQ3", "RQ4", "RQ5"), 
                              c(BG.test.RQ1[2], BG.test.RQ2[2], BG.test.RQ3[2], BG.test.RQ4[2], BG.test.RQ5[2]),
                              c(BG.test.RQ1[2], BG.test.RQ2[2], BG.test.RQ3[2], BG.test.RQ4[2], BG.test.RQ5[2])))
BGtest <- cbind(BG.test.RQ1, BG.test.RQ2, BG.test.RQ3, BG.test.RQ4, BG.test.RQ5)
write.csv(BGtest, "./Working/Final output/BG test results.csv",row.names = TRUE)
