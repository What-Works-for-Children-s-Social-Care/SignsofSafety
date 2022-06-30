
#******************* CIN initial cleaning & merging individual data sets together *********** # 
# Description: 
#   CIN files are available in individual years - this script merges them, keeping only: 
#     (a) relevant columns
#     (b) information on children from comparator or pilot local authorities
# ******************************************************************************************* #

rm(list=ls())
setwd("P:/")

source("./Working/00_Package_install.R")


#  ********read in CIN data: **********

list.cin.files <- list.files("xx")[grep("CIN_2",list.files("xx"))]
list.cin <- lapply(seq_along(list.cin.files), function(x) as.data.frame(read.csv(sprintf("xx/%s",list.cin.files[[x]]), stringsAsFactors = FALSE)))
names(list.cin) <- c(2008:2018) 
save(list.cin, "list.cin", file="Working/Intermediate_output/CIN_data_raw.Rdata")
load("Working/CIN_Data_raw.Rdata") # list.cin

# ******** limit list to LAs from pilot and comparator LAs only  ********
load("xx/Intermediate_output/all_LAs_codes.Rdata") # matches & codes

## test all codes are available in each CIN year 
for (i in seq_along(list.cin)) {
  print(names(list.cin)[[i]])
  print(setdiff(matches.codes$LA.code, list.cin[[i]]$"CIN_LA"))
}

setdiff(matches.codes$LA.code, list.cin[[1]]$CIN_LA_LGR)
setdiff(matches.codes$LA.code, list.cin[[2]]$CIN_LA) 

allocate.cols <- c("CIN_LA_LGR","CIN_LA","CIN_LA","CIN_LA","CIN_LA","CIN_LA","CIN_LA","CIN_LA","CIN_LA","CIN_LA","CIN_LA")
list.cin2 <- lapply(seq_along(list.cin), function(x) list.cin[[x]][which(list.cin[[x]][[allocate.cols[[x]]]] %in% matches.codes$LA.code),])

for (i in seq_along(list.cin2)) {
  print(setdiff(list.cin2[[i]]$CIN_LA, matches.codes$LA.code))
  print(setdiff(matches.codes$LA.code, list.cin2[[i]]$CIN_LA))
}

## ********* MERGE CIN data frames together *****************# 

for (i in seq_along(list.cin)) {
  print(names(list.cin)[[i]])
  print(dim(list.cin[[i]]))
  print(dim(list.cin2[[i]]))
  print(dim(list.cin2[[i]])[1]/dim(list.cin[[i]])[1])
}

# harmonise colnames before merging - usually indicative of the relevant year
list.cin2 <- lapply(seq_along(list.cin2), function(x) {
  colnames(list.cin2[[x]]) <- gsub("_SPR1.*","",colnames(list.cin2[[x]]))
  colnames(list.cin2[[x]]) <- gsub("_SPR0.*","",colnames(list.cin2[[x]]))
  list.cin2[[x]] 
})

all.cin <- Reduce(function(dtf1,dtf2) rbind.fill(dtf1,dtf2), list.cin2)

# ************ consolidate LA column and remove all columns that are not needed. ****#

all.cin$CIN_LA <- ifelse(is.na(all.cin$CIN_LA), all.cin$CIN_LA_LGR, all.cin$CIN_LA)
to.drop <- c("ExtendedHours","ThirtyHourCode","Connexions","ModeOfTravel","FundedHours","TypeOfClass",
             "UnitContactTime","YSSA", "CensusDate", "CensusTerm", "SENprovisionMajor","SchoolLunchTaken",
             "HomeLA","DOB","HoursAtSetting","EVERFSM_3","PrimarySENtype","Phase","EnrolStatus","LA","LA_9Code",
             "HomeLA_9Code","OnRoll","RecordStatus","CIN_IDACI_Score","CIN_IDACI_Rank","CIN_PersonDeathDate",
             "CIN_LA_09","CIN_LA_9CODE","CIN_ExpectedDOB","CIN_CPPindicator","AcademicYear","IDACIScore",
             "IDACIRank","IDACIScore_10","IDACIScore_15","IDACIRank_10","IDACIRank_15","AdoptedFromCare_From2005",
             "AdoptedFromCare_AllYears","CIN_LatestClosureDate","CIN_LatestReferralDate","CIN_MonthOfBirth","CIN_YearOfBirth") 
all.cin <- all.cin[,-which(colnames(all.cin) %in% to.drop)]

save(all.cin, "all.cin", file="xx/CIN_merged_raw.Rdata")
