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

candidaterec1 <- read.csv("CandidateRecordsA_1637171515153834517.csv")
candidaterec2 <- read.csv("CandidateRecordsA_2637171552633563593.csv")
candidaterec3 <- read.csv("CandidateRecordsA_3637171593902632419.csv")
candidaterec4 <- read.csv("CandidateRecordsA_4637171658861484928.csv")
candidaterec5 <- read.csv("CandidateRecordsA_5637171684326650167.csv")

#CODE TESTING ----

#vec1 <- c("a","b","c","d")
#vec2 <- c(1,2,3,4)
#vec3 <- c("A","B","C","D")
#vec4 <- c(T,T,F,T)
#df1 <- data.frame(vec1,vec2,vec3,vec4)

#vec1 <- c("mot","hai","ba","bon")
#vec2 <- c("one","two","three","four")
#vec3 <- c(F,F,F,T)
#vec4 <- c(9,8,7,6)
#df2 <- data.frame(vec1,vec2,vec3,vec4)

#df3 <- rbind(df1,df2)"""

#IS THERE A WAY TO MERGE DATA FRAMES WHILE ONLY KEEP THE COLUMNS AS IN candidaterec0_mod1?
