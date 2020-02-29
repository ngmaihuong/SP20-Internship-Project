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
  select(-OfferApprovals, -WorkStatus, -InterviewTypeResponses, -OfferApprovals, -Comments, -Resume,
         -ApplicationId, -CandidateId, -WorkFlowStates, -Address, -PostalCode, -WorkPhone, -Company,
         -OriginalDocument, -Portfolios, -StartDate, -HireDate, -Title, -Rating, -Notefiles, -noteAttachments,
         -Messages, -messageAttachments, -BackgroundCheck, -Notes, -ScreenerResponses,
         -Candidate_Full_Name.VEVRAA_Pre.Offer, -Candidate_Full_Name.VEVRAA_Post.Offer,
         -Active_Wartime_or_Campaign_Badge_Veteran.VEVRAA_Post.Offer,
         -Armed_Forces_Service_Medal_Veteran.VEVRAA_Post.Offer, 
         -Candidate_Full_Name.503_Post.Offer, -Candidate_Full_Name.503_Pre.Offer, 
         -Candidate_Full_Name.EEO_Pre.Offer, -Candidate_Full_Name.EEO_Post.Offer)
