#Name: Nguyen, Sierra
#Dickinson College
#Date created: 4/2/2020
#Date last updated: 5/15/2020
#Project: Talent Acquisition Analytics SP20

#Opening Tools ----
library(base)
library(httr)
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(GGally)
library(arsenal)
library(lubridate)
library(plotrix)
library(plotly)
library(RColorBrewer)
library(data.table)
library(e1071)

#Setting Working Directory ----
setwd("~/Downloads/Suzy_02122020223915/Candidates")

#Importing Data ----
full_data <- read.csv("comp_data")

#Data Overview ----
summary(full_data) #Display summary statistics of the dataset

#Display the number of candidates in each variable of interest for visibility only
data_0 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(RequisitionTitle) %>% count(RequisitionTitle)%>% arrange(desc(n)) %>% 
  rename('Number of Candidates'=n)
data_1 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(Category) %>% count(Category) %>% arrange(desc(n)) %>% 
  rename('Number of Candidates'=n)
data_2 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(Source) %>% count(Source) %>% arrange(desc(n)) %>% 
  rename('Number of Candidates'=n)
data_3 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(SourceDetails) %>% count(SourceDetails) %>% arrange(desc(n)) %>% 
  rename('Number of Candidates'=n)
data_4 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(Status) %>% count(Status) %>% arrange(desc(n)) %>% 
  rename('Number of Candidates'=n)
data_5 <- full_data %>% filter(!is.na(SourceDetails)) %>% 
  group_by(JobLevel) %>% count(JobLevel) %>% arrange(desc(n)) %>% 
  rename('Number of Candidates'=n)
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

#Question 1 ----

#Modeling ----
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

by_spec[is.na(by_spec)] <- 0 #Replace all NA's with 0's

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

by_lvl[is.na(by_lvl)] <- 0 #Replace all NA's with 0's

#Create new data frames to evaluate the number of application coming from each job board
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

by_spec_JB[is.na(by_spec_JB)] <- 0 #Replace all NA's with 0's

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

by_lvl_JB[is.na(by_lvl_JB)] <- 0 #Replace all NA's with 0's

#Graphing ----

#Reshape the data frames for graphing

by_spec <- by_spec %>% gather(TypeCount, Count, AgencyCount:JobBoardCount) %>%
  arrange(Category, TypeCount, Count) %>%
  separate(TypeCount, c("TypeCount", "empty"), sep="Count") %>%
  select(-empty)

by_lvl <- by_lvl %>% gather(TypeCount, Count, AgencyCount:JobBoardCount) %>%
  arrange(JobLevel, TypeCount, Count) %>%
  separate(TypeCount, c("TypeCount", "empty"), sep="Count") %>%
  select(-empty)

by_spec_JB <- by_spec_JB %>% gather(TypeCount, Count, LinkedInCount:BuiltinNYCCount) %>%
  arrange(Category, TypeCount, Count) %>%
  separate(TypeCount, c("TypeCount", "empty"), sep="Count") %>%
  select(-empty)

by_lvl_JB <- by_lvl_JB %>% gather(TypeCount, Count, LinkedInCount:BuiltinNYCCount) %>%
  arrange(JobLevel, TypeCount, Count) %>%
  separate(TypeCount, c("TypeCount", "empty"), sep="Count") %>%
  select(-empty)

#Supporting information

data_7 <- full_join(data_2, ind_aphratio, by=c("Source"="Source"))

data_1_a <- full_data %>% filter(!is.na(SourceDetails), Source=="Job Board") %>% 
  group_by(Category) %>% count() %>% arrange(desc(n)) %>% 
  rename('Number of Candidates from Job Boards'=n)

data_1_b <- full_data[full_data$SourceDetails %in% c('LinkedIn', 'Indeed', 'Glassdoor', 'BuiltinNYC'),]
data_1_b <- data_1_b %>% group_by(Category) %>% 
  count(Category) %>% arrange(desc(n)) %>% 
  rename('Number of Candidates from 4 Specified Job Boards'=n)

data_6 <- full_data %>% select(Category, RequisitionTitle)
data_6 <- data_6[!duplicated(data_6$RequisitionTitle),]
data_6 <- data_6 %>% group_by(Category) %>% count() %>% arrange(desc(n)) %>% 
  rename('Number of Requisitions'=n)
data_6 <- full_join(data_6, data_1, by=c("Category"="Category"))
data_6 <- full_join(data_6, data_1_a, by=c("Category"="Category"))
data_6 <- full_join(data_6, data_1_b, by=c("Category"="Category"))

data_6 <- data_6 %>% mutate(new = (`Number of Candidates from 4 Specified Job Boards`/
                                     `Number of Candidates from Job Boards`)*100) %>%
  rename('% of Candidates from 4 Specified Job Boards out of all Job Boards'=new)

#Graphing

by_spec %>%
  group_by(Category, TypeCount) %>%
  #filter(TypeCount != 'CareerSite') %>%
  ggplot(aes(x=Category, y=Count)) +
  labs(title="Fig 5a. Types of Source by Candidates from Each Category", x="Category", y="Count") +
  geom_bar(stat="identity", position="dodge", aes(fill=TypeCount)) +
  coord_flip()

by_lvl %>%
  group_by(JobLevel, TypeCount) %>%
  filter(JobLevel != "Open Resume Submission") %>%
  ggplot(aes(x=JobLevel, y=Count)) +
  labs(title="Fig 5b. Types of Source by Candidates from Each Job Level", x="Level of Job", y="Count") +
  geom_bar(stat="identity", position="dodge", aes(fill=TypeCount)) +
  coord_flip()

by_spec_JB %>%
  group_by(Category, TypeCount) %>%
  ggplot(aes(x=Category, y=Count)) +
  labs(title="Fig 6a. Job Boards by Candidates from Each Category", x="Category", y="Count") +
  geom_bar(stat="identity", position="dodge", aes(fill=TypeCount)) +
  coord_flip()

by_lvl_JB %>%
  group_by(JobLevel, TypeCount) %>%
  filter(JobLevel != "Open Resume Submission") %>%
  ggplot(aes(x=JobLevel, y=Count)) +
  labs(title="Fig 6b. Job Boards by Candidates from Each Job Level", x="Level of Job", y="Count") +
  geom_bar(stat="identity", position="dodge", aes(fill=TypeCount)) +
  coord_flip()

#Question 2 ----

#2.1. Applicants per Hire Comparison among Sources ----
#Modeling ----

#Define Applicants per Hire function: x = # for a position, y = # for the whole group, ratio = name of the result
aphratio <- function(ratio, x, y){
  ratio = x/y
  return(ratio)
}

#Grouping data frames for calculation

#All sources = Total # of applicants / Total # of hired
x <- length(which("Hired" == full_data$Status))
y <- length(full_data$Status)
aphratio_all <- aphratio(aphratio_all, x, y)

#Individual sources
ind_source <- full_data %>% filter(Source=='Career Site')
x <- length(which("Hired" == ind_source$Status))
y <- length(ind_source$Status)
CareerSite <- aphratio(CareerSite, x, y)

ind_source <- full_data %>% filter(Source=='Job Board')
x <- length(which("Hired" == ind_source$Status))
y <- length(ind_source$Status)
JobBoard <- aphratio(JobBoard, x, y)

ind_source <- full_data %>% filter(Source=='Employee')
x <- length(which("Hired" == ind_source$Status))
y <- length(ind_source$Status)
Employee <- aphratio(Employee, x, y)

ind_source <- full_data %>% filter(Source=='Agency')
x <- length(which("Hired" == ind_source$Status))
y <- length(ind_source$Status)
Agency <- aphratio(Agency, x, y)

ind_source <- full_data %>% filter(Source=='Campus')
x <- length(which("Hired" == ind_source$Status))
y <- length(ind_source$Status)
Campus <- aphratio(Campus, x, y)

ind_source <- full_data %>% filter(Source=='Hiring Manager')
x <- length(which("Hired" == ind_source$Status))
y <- length(ind_source$Status)
HiringManager <- aphratio(HiringManager, x, y)

ind_source <- full_data %>% filter(Source=='Internal Hire')
x <- length(which("Hired" == ind_source$Status))
y <- length(ind_source$Status)
InternalHire <- aphratio(InternalHire, x, y)

ind_source <- full_data %>% filter(Source=='External Referral')
x <- length(which("Hired" == ind_source$Status))
y <- length(ind_source$Status)
ExternalReferral <- aphratio(ExternalReferral, x, y)

rm(ind_source, x, y)

#Create a data frame for graphing
ind_aphratio <- data.frame(Agency, 
                           Campus, 
                           CareerSite, 
                           Employee, 
                           ExternalReferral, 
                           HiringManager, 
                           InternalHire, 
                           JobBoard)
ind_aphratio_0 <- transpose(ind_aphratio)
colnames(ind_aphratio_0) <- rownames(ind_aphratio)
rownames(ind_aphratio_0) <- colnames(ind_aphratio)
ind_aphratio <- ind_aphratio_0
rm(ind_aphratio_0)

ind_aphratio <- setNames(cbind(rownames(ind_aphratio), ind_aphratio, row.names = NULL), 
         c("Source", "APHratio"))

ind_aphratio <- arrange(ind_aphratio, desc(APHratio))

#Supporting information
data_2$Source <- gsub(" ", "", data_2$Source, fixed = TRUE)
data_7 <- full_join(data_2, ind_aphratio, by=c("Source"="Source"))
data_7 <- data_7 %>% filter(!is.na(APHratio)) %>% arrange(desc(APHratio))

#Graphing ----
# ind_aphratio %>% ggplot(aes(x=reorder(Source, APHratio), y=APHratio)) + 
#   geom_bar(stat="identity", fill="maroon") + 
#   labs(title="Fig 7a. Applicants per Hire ratios by Source", x="Source", y="Applicants per Hire ratio") +
#   coord_flip()

rm(Agency, 
   Campus, 
   CareerSite, 
   Employee, 
   ExternalReferral, 
   HiringManager, 
   InternalHire, 
   JobBoard)

#2.2. Applicants per Hire Comparison among Popular Job Boards ----
#Modeling ----

ind_board <- full_data %>% filter(SourceDetails=='LinkedIn')
x <- length(which("Hired" == ind_board$Status))
y <- length(ind_board$Status)
LinkedIn <- aphratio(LinkedIn, x, y)

ind_board <- full_data %>% filter(SourceDetails=='Indeed')
x <- length(which("Hired" == ind_board$Status))
y <- length(ind_board$Status)
Indeed <- aphratio(Indeed, x, y)

ind_board <- full_data %>% filter(SourceDetails=='Glassdoor')
x <- length(which("Hired" == ind_board$Status))
y <- length(ind_board$Status)
Glassdoor <- aphratio(Glassdoor, x, y)

ind_board <- full_data %>% filter(SourceDetails=='BuiltinNYC')
x <- length(which("Hired" == ind_board$Status))
y <- length(ind_board$Status)
BuiltinNYC <- aphratio(BuiltinNYC, x, y)

rm(ind_board, x, y)

jb_aphratio <- data.frame(LinkedIn,
                          Glassdoor,
                          Indeed,
                          BuiltinNYC)
jb_aphratio_0 <- transpose(jb_aphratio)
colnames(jb_aphratio_0) <- rownames(jb_aphratio)
rownames(jb_aphratio_0) <- colnames(jb_aphratio)
jb_aphratio <- jb_aphratio_0
rm(jb_aphratio_0)

jb_aphratio <- setNames(cbind(rownames(jb_aphratio), jb_aphratio, row.names = NULL), 
                         c("JobBoard", "APHratio"))
#Graphing ----

jb_aphratio %>% ggplot(aes(x=reorder(JobBoard, APHratio), y=APHratio)) + 
  geom_bar(stat="identity", fill="dark blue") + 
  labs(title="Fig 7. Applicants per Hire ratios by Four Popular Job Boards", x="Job Board", y="Applicants per Hire ratio") +
  coord_flip()

rm(LinkedIn,
   Glassdoor,
   Indeed,
   BuiltinNYC)

#Question 3 ----

#Add new column "Hired" as class labels
full_data$Hired <- ifelse(full_data$Status == 'Hired', 'Yes', 'No')
full_data$Hired <- as.factor(full_data$Hired)

#Modeling and Model Execution ----
p_vals <- replicate(
  1000,
  {
    #Sampling 25% of total observations
    train_data <- full_data %>% 
      group_by(Hired) %>% 
      sample_frac(0.75)
    suppressMessages(test_data <- full_data %>% anti_join(train_data))
    
    #Naive Bayes modeling
    model <- naiveBayes(Hired ~ RequisitionTitle+Category+Source+SourceDetails+Origin, train_data)
    
    #Predicting with test_data
    results <- predict(model,test_data)
    
    #Comparing predictions with real data
    test_data <- data.frame(test_data, results)
    test_data$compare <- ifelse(test_data$Hired == test_data$results, T, F)
    
    #Computing probability for whole dataset
    probability <- length(which(T==test_data$compare))/length(test_data$compare)
  }
)

p_vals_positive <- replicate(
  1000,
  {
    #Sampling 25% of total observations
    train_data <- full_data %>% 
      group_by(Hired) %>% 
      sample_frac(0.75)
    suppressMessages(test_data <- full_data %>% anti_join(train_data))
    
    #Naive Bayes modeling
    model <- naiveBayes(Hired ~ RequisitionTitle+Category+Source+SourceDetails+Origin, train_data)
    
    #Predicting with test_data
    results <- predict(model,test_data)
    
    #Comparing predictions with real data
    test_data <- data.frame(test_data, results)
    test_data$compare <- ifelse(test_data$Hired == test_data$results, T, F)
    test_data_1 <- test_data %>% filter(Hired == "Yes")
    
    #Computing probability for Hired candidates
    probability_1 <- length(which(T==test_data_1$compare))/length(test_data_1$compare)
  }
)

#Graphing ----
p_df <- data.frame(p_vals, p_vals_positive)

p_df %>% ggplot(aes(x=p_vals)) +
  geom_histogram(aes(y=..density..), binwidth = 0.0006, fill="light blue", color="dark blue") +
  geom_density(adjust=1, color="maroon") +
  labs(title="Fig 8a. Distribution of Probability \nof Correct Naive Bayes Prediction for all observations", x="probability")

p_df %>% ggplot(aes(x=p_vals_positive)) +
  geom_histogram(aes(y=..density..), binwidth = 0.05, fill="light blue", color="dark blue") +
  geom_density(adjust=2, color="maroon") +
  labs(title="Fig 8b. Distribution of Probability \nof Correct Naive Bayes Prediction for 'Hired' observations", x="probability")