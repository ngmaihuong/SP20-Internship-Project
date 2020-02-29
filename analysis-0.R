#Name: Nguyen, Sierra
#Dickinson College
#Suzy Inc.
#Date created: 2/29/2020
#Date last updated:
#Project:

library(base)
library(httr)
library(readxl)
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(GGally)
library(hexbin)

candidaterec0 <- read.csv("CandidateRecordsA_0637171477619300500.csv")
candidaterec0_mod <- candidaterec0 %>% 
  select(-OfferApprovals, -WorkStatus, -Race, -Gender, -City, -State, -Country,
         -InterviewTypeResponses, -OfferApprovals, -Comments, -Resume, -Veteran,
         -ApplicationId, -CandidateId, -WorkFlowStates, -Address, -PostalCode, -WorkPhone, -Company,
         -OriginalDocument, -Portfolios, -StartDate, -HireDate, -Title, -Rating, -Notefiles, -noteAttachments,
         -Messages, -messageAttachments, -BackgroundCheck, -Notes, -ScreenerResponses)
  #just to get a general sense of the variables
candidaterec0_mod1 <- candidaterec0_mod %>% select(FirstName:Disposition, Origin)
  #clean for necessary variables only
