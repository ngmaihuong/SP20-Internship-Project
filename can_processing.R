#Name: Nguyen, Sierra
#Dickinson College
#Date created: 2/29/2020
#Date last updated: 4/26/2020
#Project: Talent Acquisition Analytics SP20

install.packages("arsenal")

#Opening Tools ----
library(base)
library(httr)
library(readxl)
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(GGally)
library(hexbin)
library(arsenal)
library(lubridate)

#Data Processing (Part 1) ----
candidaterec0 <- read.csv("CandidateRecordsA_0637171477619300500.csv")
candidaterec1 <- read.csv("CandidateRecordsA_1637171515153834517.csv")
candidaterec2 <- read.csv("CandidateRecordsA_2637171552633563593.csv")
candidaterec3 <- read.csv("CandidateRecordsA_3637171593902632419.csv")
candidaterec4 <- read.csv("CandidateRecordsA_4637171658861484928.csv")
candidaterec5 <- read.csv("CandidateRecordsA_5637171684326650167.csv")

comparedf(candidaterec3, candidaterec4) #comparing 2 data frames to see the differences in terms of 
#columns as it appears: candidaterec3 has 106 variables while candidaterec4 has 107.
#rbind() function returns error because numbers of columns of arguments do not match.
summary(comparedf(candidaterec3, candidaterec4))
#difference between two data frames found: EmploymentApplication

candidaterec4 <- select(candidaterec4, -EmploymentApplication)
candidaterec5 <- select(candidaterec5, -EmploymentApplication)

comparedf(candidaterec0, candidaterec1)
comparedf(candidaterec0, candidaterec2)
comparedf(candidaterec0, candidaterec3)
comparedf(candidaterec0, candidaterec4) #Not shared: 2 variables and 0 observations.
comparedf(candidaterec0, candidaterec5) #Not shared: 2 variables and 2925 observations.

summary(comparedf(candidaterec0, candidaterec4)) #Variables not shared: x: Title89, y: Title90
summary(comparedf(candidaterec0, candidaterec5)) #Variables not shared: x: Title89, y: Title90.
#Too much output so can't really see results.

candidaterec4 <- rename(candidaterec4, Title89 = Title90)
candidaterec5 <- rename(candidaterec5, Title89 = Title90)

master_canrec <- rbind(candidaterec0, candidaterec1, candidaterec2, 
                             candidaterec3, candidaterec4, candidaterec5)

master_canrec_mod <- master_canrec %>% 
  select(-OfferApprovals, -WorkStatus, -Race, -Gender, -City, -State, -Country, -Email, -HomePhone, -Mobile,
         -InterviewTypeResponses, -OfferApprovals, -Comments, -Resume, -Veteran,
         -ApplicationId, -CandidateId, -WorkFlowStates, -Address, -PostalCode, -WorkPhone, -Company,
         -OriginalDocument, -Portfolios, -StartDate, -HireDate, -Title, -Rating, -Notefiles, -noteAttachments,
         -Messages, -messageAttachments, -BackgroundCheck, -Notes, -ScreenerResponses)
  #just to get a general sense of the variables
master_canrec_mod <- master_canrec_mod %>% select(FirstName:Disposition, Origin)
  #clean for necessary variables only
summary(master_canrec_mod)

rm(candidaterec0, candidaterec1, candidaterec2, candidaterec3, candidaterec4, candidaterec5, master_canrec)

#Master Data Frame Modification in Preparation for the Cleaning Phase ----

master_canrec_mod1 <- separate(master_canrec_mod, Source, c("Source","SourceDetails"), sep=": ", remove=T)
#The source column is quite confusing and contain cumulative information 
#so I am trying to separate it in order to use group_by(), hopefully to separate dataset for effective analysis

master_canrec_mod1$Submitted <- mdy_hm(master_canrec_mod1$Submitted,tz="EST")
master_canrec_mod1$LastUpdate <- mdy_hm(master_canrec_mod1$LastUpdate,tz="EST")

write.csv(master_canrec_mod1, "full_dataset_uncleaned", row.names = F) #export the dataset to clean in OpenRefine

#Test Analysis ----

source_pop <- master_canrec_mod1 %>% group_by(Source) %>% count(Source) %>% arrange(desc(n))
sum(source_pop$n) #because I am not sure if NA obsverations are automatically altered

source_d_pop <- master_canrec_mod1 %>% group_by(SourceDetails) %>% count(SourceDetails) %>% arrange(desc(n))
sum(source_d_pop$n)

#To find the records of candidates from a certain source: An example - "Added manually"
addman <- master_canrec_mod1
addman$SourceDetails <- ifelse(addman$SourceDetails == "Added manually", T, F)

addman <- addman %>% filter(SourceDetails == T)
#The output returns 163 candidates whose data had been imported manually into the system

#Examine the "Import" source
imported <- master_canrec_mod1
imported$Source <- ifelse(imported$Source == "Import", T, F)

imported <- imported %>% filter(Source == T)
source_imp <- imported %>% group_by(SourceDetails) %>% count(SourceDetails) %>% arrange(desc(n))

#Data Processing (Part 2) ----
#This step takes place after I have processed Requisitions dataset and cleaned the Master Candidate dataset
setwd("~/Downloads/Suzy_02122020223915/Requisitions")
req <- read.csv("part_requisitions_cleaned.csv")
setwd("~/Downloads/Suzy_02122020223915/Candidates")
master_data <- read.csv("full_dataset_cleaned_3.csv")

#pick out only 2 columns I need to join
req <- req %>% select(RequisitionId, Category)

#add Category column to the Master data frame
master_data_1 <- left_join(master_data, req, by=c("RequisitionID"="RequisitionId")) 
master_data_2 <- master_data_1 %>% group_by(Category) %>% count(Category)

#move Category column inward for visibility
master_data_1 <- master_data_1[,c(1:3,14,4:13)] 

write.csv(master_data_1, "comp_data", row.names = F)
