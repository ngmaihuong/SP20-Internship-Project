#Name: Nguyen, Sierra
#Dickinson College
#Date created: 4/23/2020
#Date last updated: 4/23/2020
#Project: Talent Acquisition Analytics SP20

#Opening Tools ----

#Set Working Directory ----
setwd("~/Downloads/Suzy_02122020223915/Requisitions")

#Importing Data ----
requisitions <- read.csv("Requisitions.csv")

#Processing ----
req_mod <- requisitions %>% select(Title, JobId, RequisitionId, Category, jobType)

write.csv(req_mod, "full_requisitions_uncleaned", row.names = F) #export the dataset to clean in OpenRefine