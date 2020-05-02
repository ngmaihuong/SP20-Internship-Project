#Name: Nguyen, Sierra
#Dickinson College
#Date created: 4/2/2020
#Date last updated: 5/1/2020
#Project: Talent Acquisition Analytics SP20

#Opening Tools ----
library(base)
library(httr)
library(readxl)
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(GGally)
library(arsenal)
library(lubridate)
library(plotrix)
library(plotly)
library(wesanderson)
library(RColorBrewer)

#Set Working Directory ----
setwd("~/Downloads/Suzy_02122020223915/Candidates")

#Importing Data ----
full_data <- read.csv("comp_data")

#Analysis ----
summary(full_data) #Display summary statistics of the dataset

#Display the number of candidates in each variable of interest for visibility only
data_0 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(RequisitionTitle) %>% count(RequisitionTitle)%>% arrange(desc(n))
data_1 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(Category) %>% count(Category) %>% arrange(desc(n))
data_2 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(Source) %>% count(Source) %>% arrange(desc(n))
data_3 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(SourceDetails) %>% count(SourceDetails) %>% arrange(desc(n))
data_4 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(Status) %>% count(Status) %>% arrange(desc(n))
data_5 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(JobLevel) %>% count(JobLevel) %>% arrange(desc(n))
#These are not necessary for ggplot2. Most of the time, ggplot2 will automatically count.

#Examine number of candidates coming from Job Board
full_data_0 <- full_data
#full_data_0$Source <- ifelse(full_data_0$Source == "Job Board", T, F)
#full_data_0 <- full_data_0 %>% filter(Source == T) #11,058 observations
full_data_0$Source <- ifelse(full_data_0$Source == "Job Board", "Job Board", NA)
full_data_0 <- full_data_0  %>% filter(!is.na(Source))
data_3a <- full_data_0 %>% filter(!is.na(SourceDetails)) %>% group_by(SourceDetails) %>% 
  count(SourceDetails) %>% arrange(desc(n))

#Survey and Visualize ----

#What are the strengths of each source?
full_data %>% 
  filter(!is.na(SourceDetails)) %>%
  ggplot(aes(x=fct_rev(fct_infreq(Source)))) + 
  geom_bar(aes(fill=Source)) + 
  labs(title="Fig 1. Distribution of Applications across Types of Source", x="Types of Source", y="Count") + 
  coord_flip()

full_data_0 %>%
  filter(!is.na(SourceDetails)) %>%
  ggplot(aes(x=fct_rev(fct_infreq(SourceDetails)))) + 
  geom_bar(aes(fill=SourceDetails)) + 
  labs(title="Fig 2. Distribution of Applications across Job Boards", x="Job Boards", y="Count") + 
  coord_flip()

#What is the application per hire ratio?
fig3a <- data_4 %>% plot_ly(labels = ~Status, values = ~n, textposition = 'outside')
fig3a <- fig3a %>% add_pie(hole = 0.6)
fig3a <- fig3a %>% layout(title = 'Fig 3. Shares of Application Status', showlegend = T,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig3a

#What source receives the highest quality applications?
full_data %>%
  filter(JobLevel != "Open Resume Submission", Source != "Import", Source != "Career Site") %>% 
  group_by(Source, JobLevel) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=JobLevel, y=count)) +
  labs(title="Fig 4a. Shares of Types of Source among \nCandidates of Each Job Level", 
       x="Levels of Job", y="Shares of Types of Source") +
  geom_bar(stat="identity", position="fill", color = "white", aes(fill=Source)) + coord_flip()

full_data %>%
  filter(JobLevel != "Open Resume Submission", Source != "Import") %>% 
  group_by(Source, JobLevel) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=Source, y=count)) +
  labs(title="Fig 4b. Shares of Applications to Different Job Levels \ncoming from Each Type of Source", 
       x="Types of Source", y="Shares of Job Levels") +
  geom_bar(stat="identity", position="fill", color = "white", aes(fill=JobLevel)) + coord_flip()

full_data %>%
  filter(JobLevel != "Open Resume Submission", Source != "Import", Status != "New") %>% 
  group_by(JobLevel, Status) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=JobLevel, y=count)) +
  labs(title="Fig 4c. Shares of Recruiting Stages among Candidates of Each Job Level", 
       x="Levels of Job", y="Shares of Recruiting Stages") +
  geom_bar(stat="identity", position="fill", color = "white", aes(fill=Status)) + coord_flip()

#Model Building ----

#Question 1
#Define functions that create new data frames by Category and JobLevel based on Source and/or SourceDetails
create.by_spec <- function(df, new_df_spec){
  new_df_spec <- data.frame()
  new_df_spec <- df %>% group_by(Category) %>% count(Category)%>% arrange(desc(n))
  return(new_df_spec)
}
create.by_lvl <- function(df, new_df_lvl){
  new_df_lvl <- data.frame()
  new_df_lvl <- df %>% group_by(JobLevel) %>% count(JobLevel) %>% arrange(desc(n))
  return(new_df_lvl)
}

full_data_1 <- full_data %>% filter(Source=='Career Site')
by_spec_CR <- create.by_spec(full_data_1, by_spec_CR)
by_lvl_CR <- create.by_lvl(full_data_1, by_lvl_CR)

full_data_1 <- full_data %>% filter(Source=='Job Board')
by_spec_JB <- create.by_spec(full_data_1, by_spec_JB)
by_lvl_JB <- create.by_lvl(full_data_1, by_lvl_JB)

full_data_1 <- full_data %>% filter(Source=='Employee')
by_spec_EM <- create.by_spec(full_data_1, by_spec_EM)
by_lvl_EM <- create.by_lvl(full_data_1, by_lvl_EM)

full_data_1 <- full_data %>% filter(Source=='Agency')
by_spec_AG <- create.by_spec(full_data_1, by_spec_AG)
by_lvl_AG <- create.by_lvl(full_data_1, by_lvl_AG)

full_data_1 <- full_data %>% filter(Source=='Campus')
by_spec_CA <- create.by_spec(full_data_1, by_spec_CA)
by_lvl_CA <- create.by_lvl(full_data_1, by_lvl_CA)

full_data_1 <- full_data %>% filter(Source=='Hiring Manager')
by_spec_HR <- create.by_spec(full_data_1, by_spec_HR)
by_lvl_HR <- create.by_lvl(full_data_1, by_lvl_HR)

full_data_1 <- full_data %>% filter(Source=='Internal Hire')
by_spec_IH <- create.by_spec(full_data_1, by_spec_IH)
by_lvl_IH <- create.by_lvl(full_data_1, by_lvl_IH)

full_data_1 <- full_data %>% filter(Source=='External Referral')
by_spec_ER <- create.by_spec(full_data_1, by_spec_ER)
by_lvl_ER <- create.by_lvl(full_data_1, by_lvl_ER)

join.by_spec <- function(by_spec, a, b, c, d, e , f, g, h){
  by_spec <- data.frame()
  by_spec <- full_join(a, b, by=c("Category"="Category"))
  by_spec <- full_join(by_spec, c, by=c("Category"="Category"))
  by_spec <- full_join(by_spec, d, by=c("Category"="Category"))
  by_spec <- full_join(by_spec, e, by=c("Category"="Category"))
  by_spec <- full_join(by_spec, f, by=c("Category"="Category"))
  by_spec <- full_join(by_spec, g, by=c("Category"="Category"))
  by_spec <- full_join(by_spec, h, by=c("Category"="Category"))
  return(by_spec)
}

by_spec <- join.by_spec(by_spec, 
                        by_spec_AG, 
                        by_spec_CR, 
                        by_spec_CA, 
                        by_spec_EM, 
                        by_spec_ER, 
                        by_spec_HR, 
                        by_spec_IH, 
                        by_spec_JB)

rm(by_spec_AG, 
   by_spec_CR, 
   by_spec_CA, 
   by_spec_EM, 
   by_spec_ER, 
   by_spec_HR, 
   by_spec_IH, 
   by_spec_JB)

by_spec <- rename(by_spec, 
                  AgencyCount=n.x, 
                  CareerSiteCount=n.y, 
                  CampusCount=n.x.x, 
                  EmployeeCount=n.y.y,
                  ExternalReferralCount=n.x.x.x,
                  HiringManagerCount=n.y.y.y,
                  InternalHireCount=n.x.x.x.x,
                  JobBoardCount=n.y.y.y.y)

join.by_lvl <- function(by_lvl, a, b, c, d, e , f, g, h){
  by_lvl <- data.frame()
  by_lvl <- full_join(a, b, by=c("JobLevel"="JobLevel"))
  by_lvl <- full_join(by_lvl, c, by=c("JobLevel"="JobLevel"))
  by_lvl <- full_join(by_lvl, d, by=c("JobLevel"="JobLevel"))
  by_lvl <- full_join(by_lvl, e, by=c("JobLevel"="JobLevel"))
  by_lvl <- full_join(by_lvl, f, by=c("JobLevel"="JobLevel"))
  by_lvl <- full_join(by_lvl, g, by=c("JobLevel"="JobLevel"))
  by_lvl <- full_join(by_lvl, h, by=c("JobLevel"="JobLevel"))
  return(by_lvl)
}

by_lvl <- join.by_lvl(by_lvl, 
                      by_lvl_AG, 
                      by_lvl_CR, 
                      by_lvl_CA, 
                      by_lvl_EM, 
                      by_lvl_ER, 
                      by_lvl_HR, 
                      by_lvl_IH, 
                      by_lvl_JB)

rm(by_lvl_AG, 
   by_lvl_CR, 
   by_lvl_CA, 
   by_lvl_EM, 
   by_lvl_ER, 
   by_lvl_HR, 
   by_lvl_IH, 
   by_lvl_JB)

by_lvl <- rename(by_lvl, 
                  AgencyCount=n.x, 
                  CareerSiteCount=n.y, 
                  CampusCount=n.x.x, 
                  EmployeeCount=n.y.y,
                  ExternalReferralCount=n.x.x.x,
                  HiringManagerCount=n.y.y.y,
                  InternalHireCount=n.x.x.x.x,
                  JobBoardCount=n.y.y.y.y)

full_data_2 <- full_data %>% filter(SourceDetails=='LinkedIn')
by_spec_LinkedIn <- create.by_spec(full_data_2, by_spec_LinkedIn)
by_lvl_LinkedIn <- create.by_lvl(full_data_2, by_lvl_LinkedIn)

full_data_2 <- full_data %>% filter(SourceDetails=='Indeed')
by_spec_Indeed <- create.by_spec(full_data_2, by_spec_Indeed)
by_lvl_Indeed <- create.by_lvl(full_data_2, by_lvl_Indeed)

full_data_2 <- full_data %>% filter(SourceDetails=='Glassdoor')
by_spec_Glassdoor <- create.by_spec(full_data_2, by_spec_Glassdoor)
by_lvl_Glassdoor <- create.by_lvl(full_data_2, by_lvl_Glassdoor)

full_data_2 <- full_data %>% filter(SourceDetails=='BuiltinNYC')
by_spec_BuiltinNYC <- create.by_spec(full_data_2, by_spec_BuiltinNYC)
by_lvl_BuiltinNYC <- create.by_lvl(full_data_2, by_lvl_BuiltinNYC)

join.by_spec_JB <- function(by_spec, a, b, c, d){
  by_spec <- data.frame()
  by_spec <- full_join(a, b, by=c("Category"="Category"))
  by_spec <- full_join(by_spec, c, by=c("Category"="Category"))
  by_spec <- full_join(by_spec, d, by=c("Category"="Category"))
  return(by_spec)
}

by_spec_JB <- join.by_spec_JB(by_spec_JB,
                              by_spec_LinkedIn,
                              by_spec_Indeed,
                              by_spec_Glassdoor,
                              by_spec_BuiltinNYC)
rm(by_spec_LinkedIn,
   by_spec_Indeed,
   by_spec_Glassdoor,
   by_spec_BuiltinNYC)

by_spec_JB <- rename(by_spec_JB,
                     LinkedInCount=n.x,
                     IndeedCount=n.y,
                     GlassdoorCount=n.x.x,
                     BuiltinNYCCount=n.y.y)

join.by_lvl_JB <- function(by_lvl, a, b, c, d){
  by_lvl <- data.frame()
  by_lvl <- full_join(a, b, by=c("JobLevel"="JobLevel"))
  by_lvl <- full_join(by_lvl, c, by=c("JobLevel"="JobLevel"))
  by_lvl <- full_join(by_lvl, d, by=c("JobLevel"="JobLevel"))
  return(by_lvl)
}

by_lvl_JB <- join.by_lvl_JB(by_lvl_JB,
                            by_lvl_LinkedIn,
                            by_lvl_Indeed,
                            by_lvl_Glassdoor,
                            by_lvl_BuiltinNYC)
rm(by_lvl_LinkedIn,
   by_lvl_Indeed,
   by_lvl_Glassdoor,
   by_lvl_BuiltinNYC)

by_lvl_JB <- rename(by_lvl_JB,
                    LinkedInCount=n.x,
                    IndeedCount=n.y,
                    GlassdoorCount=n.x.x,
                    BuiltinNYCCount=n.y.y)
