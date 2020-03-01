#Name: Nguyen, Sierra
#Dickinson College
#Suzy Inc.
#Date created: 2/29/2020
#Date last updated:
#Project: Talent Acquisition Analytics SP20

install.packages("arsenal")

#Opening tools ----
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

#Data Preparation ----
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

#df3 <- rbind(df1,df2)

