# *** FUNCTIONS ******#

rename.cols <- function(old.name, new.name, df) {
  colnames(df)[colnames(df)==old.name] <- new.name
  return(df)
}

correct.spellings <- function(df) {
  df$"Local.authority"[grepl("upon hull", tolower(df$"Local.authority"))==TRUE] <- as.character(all.LAs[grep("upon hull", tolower(unlist(all.LAs))),1])
  df$"Local.authority"[grepl("telford", tolower(df$"Local.authority"))==TRUE] <- "Telford & Wrekin"
  return(df)
}

# for multiple imputation: 
  # drop any columns where the number of missing observations is > 30%
check.30.pct <- function(df){
  for(cols in colnames(df)) {
    if((sum(is.na(df[[cols]]))/nrow(df) >= 0.3)) {
      print(paste("too little observations for: ", cols))
      print(sum(is.na(df[[cols]]))/nrow(df))
      #print(sum(df[[cols]]=="Missing")/nrow(df))
      df <- df[,-which(colnames(df) %in% cols)]
    }
  }
  return(df)
}

# factorise categorical data
factorise <- function(categ.vars, df) {
  df[,colnames(df) %in% categ.vars] <- lapply(df[,colnames(df) %in% categ.vars], factor)
  return(df)
}

# add missing categories to categorical variables
add.missing <- function(categ.vars, df) {
  for (cols in categ.vars[categ.vars %in% colnames(df)]) {
    df[cols][is.na(df[cols])] <- "Missing"
    df[cols][df[cols]==""] <- "Missing"
    df[cols][df[cols]=="MISSING"] <- "Missing"
  }
  return(df)
}


# for CEM: 
# create matched datasets using the CEM outcomes. 
create.matched.df <- function(df.old, cem.name) {
  df.new <- list()
  for (i in 1:5){
    df.new[[i]] <- cbind(df.old[[i]], cem.name[[paste("match",i,sep="")]]$mstrata, cem.name[[paste("match",i,sep="")]]$w)
    colnames(df.new[[i]])[length(df.new[[i]])-1] <- "Strata"
    colnames(df.new[[i]])[length(df.new[[i]])] <- "weights"
    df.new[[i]] <- df.new[[i]][-which(is.na(df.new[[i]]$Strata)),] 
  }
  return(df.new)
}

# *** Regressions ***

# regression for lm.cluster w/ multiple imputation
miceadds.lmcluster <- function(data.list, model){
  lapply(data.list, FUN=function(data){
    miceadds::lm.cluster(data=data, formula=formula(model), cluster=data$LA, weights=data$"weights")
  })
}

# regression for plm w/ multiple imputation
plm.multiple.imp <- function(data.list, model.formula, model){
  lapply(seq_along(data.list), function(x) {
  plm(formula(model.formula), data=data.list[[x]],effect="individual", index=c("ID"), model=model, weights = data.list[[x]]$weights)
})
}

# regression for lmer w/ multiple imputation
lmer.multiple.imp <- function(datalist, model) {
  lapply(seq_along(datalist), function(x){
    lmer(formula(paste(model, "(1|child.ID)", sep="+")), data=datalist[[x]], weights=datalist[[x]]$"weights")
  })
}

# create regression parameters --> betas, variance and standard errors that we need to pool our estimates later.
create.regression.parameters <- function(model, model.type) {
  if (model.type == "lm.cluster" || model.type == "plm") {
    betas <- lapply(model, FUN=function(rr){ coef(rr) })
    betas <- lapply(seq_along(betas), function(x) betas[[x]] <- betas[[x]][!is.na(betas[[x]])])
  } else {
    betas <- lapply(model, FUN=function(rr){ fixef(rr) }) 
  }
  vars <- lapply(model, FUN=function(rr){vcov(rr, complete=TRUE)})
  semod <- lapply(model, FUN=function(mm){
    if (model.type == "lm.cluster") {
      summary(mm)[,"Std. Error"]
    } else if (model.type == "plm") {
      summary(mm)$"coefficients"[,"Std. Error"]
    } else {
      coef(summary(mm))[,"Std. Error"]} 
  }
  )
  return(list(betas, semod))
}

run.regression <- function(df.list, formula.model){
  reg.results <- lapply(df.list, FUN=function(data){ miceadds::lm.cluster(data=data, formula=formula(formula.model), cluster=data$LA, weights=data$"weights")})
  betas <- lapply(reg.results, FUN=function(rr){ coef(rr) }) 
  betas <- lapply(seq_along(betas), function(x) betas[[x]] <- betas[[x]][!is.na(betas[[x]])])
  semod <- lapply(reg.results, FUN=function(mm){
    smm <- summary(mm) 
    smm[,"Std. Error"]
  })
  ab <- summary(miceadds::pool_mi(qhat=betas, se=semod, dfcom = 1e+07, method="smallsample"),digits=2) 
  ab[,c(1:6)] <- round(ab[,c(1:6)], 4)
  return(ab)
}

# F-test 
f.test.mi <- function(model) {
  # Note: Requires library("mitml")
  betas <- lapply(model, FUN=function(rr){ coef(rr) }) 
  vars <- lapply(model, FUN=function(rr){vcov(rr)})
  Nimp <- 5
  np <- length(betas[[1]])
  beta_names <- names(betas[[1]])
  constraints <- beta_names[-1]
  qhat <- matrix(unlist(betas), ncol = Nimp)
  rownames(qhat) <- beta_names
  uhat <- array(unlist(vars), dim=c(np, np, Nimp))
  dimnames(uhat) <- list(beta_names, beta_names, NULL)
  Ftest <- mitml::testConstraints(qhat=betas, uhat=vars, constraints=constraints)
}

wwcsc_theme <- theme(
  # removed for publication
  )

DfE_theme <- theme(
  # title and labels
  plot.title=element_text(family="sans", face="bold", size=(10), colour="#0b0c0c"),
  plot.subtitle=element_text(family="sans", size=(9), colour="#0b0c0c"), 
  plot.caption=element_text(hjust=0, family="sans", size=(8), colour="#0b0c0c"),
  axis.text.x=element_text(family="sans",size=(8), colour="#0b0c0c"),
  axis.title.y=element_text(family="sans",size=(8), colour="#0b0c0c"), 
  axis.ticks=element_blank(),
  # background
  panel.border=element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.ontop=TRUE,
  # legend
  legend.position="top",
  legend.text=element_text(family="sans", size=(8), colour="#0b0c0c"),
  legend.key.size = unit(0.25, "cm")
)

# *** summary statistics ***

# used in 05_CEM and 10_Multilevel Models
percent <- function(x, digits=2, format="f"){paste0(formatC(100*x, format=format, digits=digits),"%")}


create.summary.stats <- function(df, outcome, cond.1, cond.weighted){
  summary.stats <- as.data.frame(colnames(df[which(colnames(df) %in% c(num.vars,outcome))]))
  colnames(summary.stats) <-"variable"
  
  # create subset with only pre embedding period values
  df2 <- df[df$postembed==cond.1,]
  # Requires library("TAM")
  if (cond.weighted=="TRUE"){
    summary.stats$"mean" <- round(sapply(df2[which(colnames(df2) %in% c(num.vars, outcome))], function(x) weighted.mean(x,df2$weights)),3) 
  } else{
    summary.stats$"mean" <- round(sapply(df2[which(colnames(df2) %in% c(num.vars, outcome))], mean, na.rm=TRUE),3)      
  }
  summary.stats$"category" <- colnames(df2)[which(colnames(df2) %in% c(num.vars, outcome))]
  
  # create means for categorical variables
  dt <- data.table(df2)
  for (cvars in categorical.vars[categorical.vars!="relev.year"]) {
    if ((cvars %in% colnames(df2))==TRUE){
      if(cond.weighted=="TRUE"){
        full.table <- setDT(dt)[, .(Freq=sum(weights)), by=cvars][,Prop:=Freq/sum(Freq)][]
        D <- as.data.frame(full.table[,c(1,3)])
      } else{
        B <- table(df2[[cvars]])
        D <- as.data.frame(round(prop.table(B,),3))
      }
      colnames(D) <- c("variable","mean")
      D$"category" <- rep(cvars, nrow(D))
      summary.stats <- rbind(summary.stats, D)
    }
  }
  summary.stats$variable <- as.character(summary.stats$variable)
  summary.stats$mean <- as.numeric(summary.stats$mean) 
  summary.stats$category <- as.character(summary.stats$category) 
  summary.stats <- rbind(summary.stats, c(as.character("number.of.observations"),as.numeric(nrow(df2)),"number of observations"))
  summary.stats$"mean" <- as.numeric(summary.stats$"mean")
  
  return(summary.stats)
}

average.summary.stats <- function(stats.list){
  stats.list <- lapply(seq_along(stats.list), 
                       function(x) {
      stats.list[[x]]$category.variable <- paste(as.character(stats.list[[x]]$category), as.character(stats.list[[x]]$variable, sep = "_"))
      return(stats.list[[x]])})

  stats.list.all <- Reduce(function(x, y) merge(x, y, by = "category.variable", all = TRUE), stats.list)
  # }
  stats.list.all.mean <- stats.list.all[,grep("mean", colnames(stats.list.all))]
  stats.list.all.mean[is.na(stats.list.all.mean)] <- 0
  mean <- rowMeans(stats.list.all.mean)

  stats.total <- cbind(as.data.frame(stats.list.all$"category"),as.data.frame(stats.list.all$"variable"),mean) # removed std.dev
  colnames(stats.total)[2] <- "variable"
  colnames(stats.total)[1] <- "category"
  stats.total$mean[stats.total$category %in% categorical.vars] <- percent(stats.total$mean[stats.total$category %in% categorical.vars])
  return(stats.total)
}

run.summary.stats <- function(data.list, outcome) {
  weighted.T <- lapply(seq_along(data.list), function(x) create.summary.stats(data.list[[x]][data.list[[x]]$SofS.LA==1,],outcome, cond.1=0, cond.weighted = "TRUE")) # pre embed obs only
  weighted.C <- lapply(seq_along(data.list), function(x) create.summary.stats(data.list[[x]][data.list[[x]]$SofS.LA==0,],outcome, cond.1=0, cond.weighted = "TRUE")) # pre embed obs only
  unweighted.T <- lapply(seq_along(data.list), function(x) create.summary.stats(data.list[[x]][data.list[[x]]$SofS.LA==1,],outcome, cond.1=0, cond.weighted = "FALSE"))
  unweighted.C <- lapply(seq_along(data.list), function(x) create.summary.stats(data.list[[x]][data.list[[x]]$SofS.LA==0,],outcome, cond.1=0, cond.weighted = "FALSE"))
  
  averaged.weighted.T <- average.summary.stats(weighted.T)
  averaged.weighted.C <- average.summary.stats(weighted.C)
  averaged.unweighted.T <- average.summary.stats(unweighted.T)
  averaged.unweighted.C <- average.summary.stats(unweighted.C)
  
  # add overall number of obs
  obs <- mean(nrow(data.list[[1]]),nrow(data.list[[2]]),nrow(data.list[[3]]),nrow(data.list[[4]]),nrow(data.list[[5]]))

  averaged.weighted.T$variable <- as.character(averaged.weighted.T$variable)
  averaged.weighted.T$category <- as.character(averaged.weighted.T$category)

  averaged.weighted.C$variable <- as.character(averaged.weighted.C$variable)
  averaged.weighted.C$category <- as.character(averaged.weighted.C$category)

  averaged.unweighted.T$variable <- as.character(averaged.unweighted.T$variable)
  averaged.unweighted.T$category <- as.character(averaged.unweighted.T$category)

  averaged.unweighted.C$variable <- as.character(averaged.unweighted.C$variable)
  averaged.unweighted.C$category <- as.character(averaged.unweighted.C$category)

  averaged.weighted.T <- rbind(averaged.weighted.T, c("number of observations - entire sample",NA,obs))
  averaged.weighted.C <- rbind(averaged.weighted.C, c(as.character("number of observations - entire sample"),NA,as.numeric(obs)))
  averaged.unweighted.T <- rbind(averaged.unweighted.T, c(as.character("number of observations - entire sample"),NA,as.numeric(obs)))
  averaged.unweighted.C <- rbind(averaged.unweighted.C, c(as.character("number of observations - entire sample"),NA,as.numeric(obs)))

 return(list(averaged.weighted.T, averaged.weighted.C, averaged.unweighted.T, averaged.unweighted.C))
} 

# for multiple hypothesis testing in 09
lh.multiple <- function(model, hypothesis.matrix) {
  lh.results <- linearHypothesis(model, hypothesis.matrix, singular.ok=TRUE)
  return(list(lh.results$Df[2], lh.results$Chisq[2]))
}

# for multiple hypothesis testing in 09
combined.hypothesis.testing <- function(hypothesis, models, degrees.freedom){  
  lh.list <- lapply(models, function(x) lh.multiple(x, hypothesis))
  lh.list.chi.sq <- c(lh.list[[1]][[2]], lh.list[[2]][[2]], lh.list[[3]][[2]], lh.list[[4]][[2]], lh.list[[5]][[2]])
  lh.list.df <- c(lh.list[[1]][[1]], lh.list[[2]][[1]], lh.list[[3]][[1]], lh.list[[4]][[1]], lh.list[[5]][[1]])
  combined.results <- micombine.chisquare(lh.list.chi.sq, df=degrees.freedom)
  return(list(combined.results, lh.list.df))
}



make.composite.weights <- function(single.matched.df, identifying.date) {
  # We restricted the data to just those matched
  # That these are equal means that we can sum the ind.w to create an equivalent overall proportion of Cs / Ts
  # We need to downweight the contribution of duplicates BEFORE they are counted in the proportions
  # of matched observations in each strata and overall
  # Therefore we then need to recalculate the cem weights manually
  
  # 1) Generate weights which are the inverse of the number of duplicates - this would be the weight if we weren't matching
  
  RQ.dt <- data.table(single.matched.df) # RQ.matched - single dataset (wrap in lapply)
  RQ.dt[, n.duplicated := .N, by = c("LA", "child.ID", identifying.date)] # Count the number of duplicate observations for each child and date
  RQ.dt[,ind.w:= 1 / RQ.dt$n.duplicated] # individual weighting = inverse of the multiplicity
  
  # 2) We then find the weighted ratios of control observations to treated observations 
  # (weighted to take into account that we're artificially inflating the number of controls by using some controls for multiple matches)
  
  # a) Overall
  id.w.table <- data.frame()
  id.w.table["Matched", "G0"] <- sum(RQ.dt[RQ.dt$SofS.LA==0,"ind.w"])
  id.w.table["Matched", "G1"] <- sum(RQ.dt[RQ.dt$SofS.LA==1,"ind.w"])
  
  # b) And within each strata
  tmp <- RQ.dt[, sum(ind.w), by = c("Strata", "SofS.LA")] # Strata represents all the breakdown of characteristics we are matching on
  tmp2 <- as.data.frame(dcast(tmp, Strata ~ SofS.LA, value.var="V1")) # reshaped to match structure 
  tmp2[is.na(tmp2)] <- 0
  
  # 3) To find the weights for each strata for treatments and controls We then multiply the weighted ratios for the strata and overall together
  bg <- "1"
  bgn <- "G1"
  wh.df <- data.frame("Strata"=numeric(length( tmp2$Strata)))
  wh.df$Strata <- tmp2$Strata 
  tmp2$Strata <- NULL
  wh <- t(sapply(1:NROW(tmp2), function(x) tmp2[x,bg] / tmp2[x,]))
  wh <- as.data.frame(cbind(unlist(wh[,"0"]), unlist(wh[,"1"]))) 
  colnames(wh) <- c("0", "1")
  
  comparator.weights <- id.w.table[,"G0"] / id.w.table[, "G1"] # multiply by the overall proportion of T to C
  wh[,"0"] <- unlist(wh[,"0"]) * rep(comparator.weights, dim(wh)[1])
  wh.df[, c("0", "1")] <- wh[, c("0", "1")]
  colnames(wh.df) <- c("Strata", "0", "1")
  
  # 4) We then merge into the overall dataframe
  setdiff(RQ.dt$Strata, wh.df$Strata)
  RQ.dt.weights <- merge(RQ.dt, wh.df, by="Strata", all = TRUE)
  RQ.dt.weights$new.weight <- ifelse(RQ.dt.weights$SofS.LA==0, RQ.dt.weights$`0`,
                                     ifelse(RQ.dt.weights$SofS.LA==1, RQ.dt.weights$`1`, NA))
  # 5) And create a composite weight
  RQ.dt.weights[,composite.weights:= new.weight * ind.w]
  cat("Sum of composite weight == sum of individual weight ", sum(RQ.dt.weights$composite.weights, na.rm = TRUE) == sum(RQ.dt.weights$ind.w, na.rm = TRUE))
  cat("Sum of composite weight == sum of individual weight in table ", sum(RQ.dt.weights$composite.weights, na.rm = TRUE) == sum(id.w.table))
  print(sum(RQ.dt.weights$composite.weights, na.rm = TRUE))
  print(sum(RQ.dt.weights$ind.w, na.rm = TRUE))
  RQ.weights <- as.data.frame(RQ.dt.weights)
  RQ.weights[,c("weights","n.duplicated", "ind.w","0", "1", "new.weight")] <- list(NULL)
  RQ.weights$weights <- RQ.weights$composite.weights
  RQ.weights$composite.weights <- NULL
  return(RQ.weights)
}

use.duplicated.data.only <- function(data.list){
  df <- list()
  df <- lapply(seq_along(data.list), function(x) {
    df[[x]] <- data.table(data.list[[x]])
    df[[x]][, no.occur:=.N, by=c("child.ID", "LA", "Matched.group")] 
    df[[x]] <- df[[x]][df[[x]]$no.occur>1,] 
    
    # drop observations from strata with no matches
    t <- table(df[[x]]$Strata, df[[x]]$SofS.LA)
    t <- as.data.frame(t)
    colnames(t) <- c("Strata", "SofS.LA", "N.strata")
    t$Strata <- as.integer(as.character(t$Strata))
    t$SofS.LA<- as.double(as.character(t$SofS.LA))
    df[[x]] <- merge(df[[x]], t, by = c("Strata", "SofS.LA"), all = TRUE)
    df[[x]] <- data.table(df[[x]])
    df[[x]][,min.N.strata:=min(N.strata), by = c("Strata")]
    df[[x]] <- df[[x]][df[[x]]$min.N.strata!=0,]
    print(sum(is.na(df[[x]])))
    print(sum(duplicated(df[[x]])))
    return(as.data.frame(df[[x]]))})
  return(df)}


identify.duplicate.data <- function(data.list){
  df <- list()
  df <- lapply(seq_along(data.list), function(x) {
    df[[x]] <- data.table(data.list[[x]])
    df[[x]][, no.occur:=.N, by=c("child.ID", "LA", "Matched.group")] 
    df[[x]][, no.occur_mt1:= no.occur > 1,] 
    return(as.data.frame(df[[x]]))})
  return(df)}

create.test.pop <- function(df, time.var) {
  df.new <- data.table(df)
  df.new[,ID:=.GRP, by=.(child.ID, LA, Matched.group)] # create ID var
  df.new <- as.data.frame(df.new)
  return(df.new)
} 

remove.time.invariant <- function(model.no){
  mv <- gsub("Ethnicity\\+","",model.no)
  mv <- gsub("gender\\+","",mv)
  mv <- gsub("disabled\\+","",mv)
  return(mv)
}