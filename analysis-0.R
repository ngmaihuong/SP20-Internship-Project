#Name: Nguyen, Sierra
#Dickinson College
#Date created: 4/22/2020
#Date last updated: 4/28/2020
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
data_0 <- full_data %>% filter(!is.na(SourceDetails)) %>% group_by(RequisitionTitle) %>% count(RequisitionTitle)%>% arrange(desc(n))
data_1 <- full_data %>% filter(!is.na(SourceDetails)) %>% group_by(Category) %>% count(Category) %>% arrange(desc(n))
data_2 <- full_data %>% filter(!is.na(SourceDetails)) %>% group_by(Source) %>% count(Source) %>% arrange(desc(n))
data_3 <- full_data %>% filter(!is.na(SourceDetails)) %>% group_by(SourceDetails) %>% count(SourceDetails) %>% arrange(desc(n))
data_4 <- full_data %>% filter(!is.na(SourceDetails)) %>% group_by(Status) %>% count(Status) %>% arrange(desc(n))
data_5 <- full_data %>% filter(!is.na(SourceDetails)) %>% group_by(JobLevel) %>% count(JobLevel) %>% arrange(desc(n))
#These are not necessary for ggplot2. ggplot2 will automatically count.

#Examine number of candidates coming from Job Board
full_data_0 <- full_data
#full_data_0$Source <- ifelse(full_data_0$Source == "Job Board", T, F)
#full_data_0 <- full_data_0 %>% filter(Source == T) #11,058 observations
full_data_0$Source <- ifelse(full_data_0$Source == "Job Board", "Job Board", NA)
full_data_0 <- full_data_0  %>% filter(!is.na(Source))
data_3a <- full_data_0 %>% filter(!is.na(SourceDetails)) %>% group_by(SourceDetails) %>% count(SourceDetails) %>% arrange(desc(n))

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

# data_4 %>% mutate(share=n/sum(n)) %>% ungroup() %>% arrange(desc(n)) %>% 
#   mutate(Status=factor(Status, levels=as.character(Status))) %>%
#   ggplot(aes("", y=n, fill=Status)) + geom_bar(stat="identity") + 
#   coord_polar("y", start=0) + theme_void()

# fig3 <- plot_ly(data_4, labels = ~Status, values = ~n, type = 'pie', textposition = 'outside') %>%
#   layout(title = 'Shares of Application Status',
#          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig3a <- data_4 %>% plot_ly(labels = ~Status, values = ~n, textposition = 'outside')
fig3a <- fig3a %>% add_pie(hole = 0.6)
fig3a <- fig3a %>% layout(title = 'Fig 3. Shares of Application Status', showlegend = T,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig3a
#slices <- data_4$n
#data_4_labels <- data_4$Status
#pie3D(slices,labels=data_4_labels,theta=pi/3,border="white",explode=0.75,main="Pie Chart")

#What source receives the highest quality applications?

# full_data %>% 
#   ggplot(aes(x=fct_rev(fct_infreq(JobLevel)))) + 
#   geom_bar(aes(fill = JobLevel)) + 
#   #  scale_fill_manual(values = brewer.pal(nlevels(full_data$JobLevel), name ="Set2")) +
#   labs(title="Fig 4. Distribution of Applications across Job Levels", x="Levels of Job", y="Count") + 
#   coord_flip()
# full_data %>% 
#   filter(JobLevel != "Open Resume Submission") %>% 
#   ggplot(aes(x=fct_rev(fct_infreq(JobLevel)))) + 
#   geom_bar(aes(fill = JobLevel)) + 
#   scale_fill_manual(values = brewer.pal(nlevels(full_data$JobLevel), name ="Set2")) +
#   labs(title="Fig 4a. Distribution of Applications across Job Levels \n(no Open Resume Submission)", 
#        x="Levels of Job", y="Count") + 
#   coord_flip()

full_data %>%
  filter(JobLevel != "Open Resume Submission", Source != "Import", Source != "Career Site") %>% 
  group_by(Source, JobLevel) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=JobLevel, y=count)) +
  #  scale_fill_manual(values = brewer.pal(nlevels(full_data$Source), name ="Set3")) +
  labs(title="Fig 4a. Shares of Types of Source among \nCandidates of Each Job Level", 
       x="Levels of Job", y="Shares of Types of Source") +
  geom_bar(stat="identity", position="fill", color = "white", aes(fill=Source)) + coord_flip()

full_data %>%
  filter(JobLevel != "Open Resume Submission", Source != "Import") %>% 
  group_by(Source, JobLevel) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=Source, y=count)) +
  #  scale_fill_manual(values = brewer.pal(nlevels(full_data$JobLevel), name ="Set2")) +
  labs(title="Fig 4b. Shares of Applications to Different Job Levels \ncoming from Each Type of Source", 
       x="Types of Source", y="Shares of Job Levels") +
  geom_bar(stat="identity", position="fill", color = "white", aes(fill=JobLevel)) + coord_flip()

full_data %>%
  filter(JobLevel != "Open Resume Submission", Source != "Import") %>% 
  group_by(JobLevel, Status) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=JobLevel, y=count)) +
  labs(title="Fig 4c. Shares of Recruiting Stages among Candidates of Each Job Level", 
       x="Levels of Job", y="Shares of Recruiting Stages") +
  geom_bar(stat="identity", position="fill", aes(fill=Status)) + coord_flip()

full_data %>%
  filter(JobLevel != "Open Resume Submission", Source != "Import", Status != "New") %>% 
  group_by(JobLevel, Status) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=JobLevel, y=count)) +
  labs(title="Fig 4c. Shares of Recruiting Stages among Candidates of Each Job Level", 
       x="Levels of Job", y="Shares of Recruiting Stages") +
  geom_bar(stat="identity", position="fill", color = "white", aes(fill=Status)) + coord_flip()

#fig4a <- plot_ly(x = data_5$JobLevel, y = data_5$n, name = "Test Plotly chart", type = "bar")
