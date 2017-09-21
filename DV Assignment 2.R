## Load Libraries
require(ggplot2)
require(dplyr)
require(splitstackshape)

## Set Working Directory
setwd("C:/Users/rahul/Google Drive/RMIT/Semester 2/Data Visualization/Assignment 2")

## Read files
Data2017 = read.csv('data/survey_results_public.csv')

##
## Mean Salary for Developers
##

Salary = Data2017 %>% 
  select(Respondent,Country,DeveloperType,Salary) %>% 
  filter(!is.na(DeveloperType) & !is.na(Salary) & Country %in% c('United States','United Kingdom','Australia'))

Salary_long <- cSplit(Salary, "DeveloperType", sep = ";", direction = "long")

# Create data for Currency Conversion (Average of Jan 2017)
conversionUSD = data.frame(Country = c('United States','United Kingdom','Australia'),
                           exchange = c(1,1.2512,0.7574))

Salary_All = merge(Salary_long,conversionUSD, by ='Country')
Salary_All = Salary_All %>% mutate (SalaryUSD = Salary * exchange)

Salary_Summary = Salary_All %>% 
  group_by(DeveloperType) %>% 
  summarise(count = n(), MeanSalary = mean(SalaryUSD)) %>% 
  filter(DeveloperType != 'Other')

## set the levels 
Salary_Summary$DeveloperType = Salary_Summary$DeveloperType %>% 
  factor(levels = Salary_Summary$DeveloperType[order(Salary_Summary$MeanSalary)]) 

# Adding Color Element
Salary_Summary$colour = ifelse(Salary_Summary$DeveloperType == 'Data scientist' | 
                                 Salary_Summary$DeveloperType == 'Machine learning specialist' |
                                 Salary_Summary$DeveloperType == 'Developer with a statistics or mathematics background',
                               "positive","negative")

ggplot(Salary_Summary,aes(DeveloperType,MeanSalary,label="",fill = colour)) + 
  geom_bar(stat='identity',aes(fill = colour)) + 
  scale_fill_manual(values=c(positive="#2171b5",negative="#6baed6")) +
  coord_flip() +
  guides(fill=FALSE) +
  ggtitle('Average Salary of Software Developer, 2017') +
  xlab('') +
  ylab('Mean Salary in US Dollars') +
  labs(subtitle = 'Salary data from US, UK and Australia',
       caption="Source - Stack Overflow Annual Developer Survey, 2017") +
  theme_minimal() +
  theme(plot.caption = element_text(size = 9,color = '#666666',face = "italic"),
        plot.subtitle = element_text(color = '#333333',face = "italic")) 
##
## Analysis on Languages
##

Languages = Data2017 %>% select(Respondent,Country,HaveWorkedLanguage,DeveloperType) %>% 
  filter(!is.na(HaveWorkedLanguage) & DeveloperType %in% c('Data scientist','Machine learning specialist'))


Languages_long <- cSplit(indt = Languages, 
                         splitCols = c('HaveWorkedLanguage'), sep = ";", direction = "long")

Languages_summary = Languages_long %>% group_by(HaveWorkedLanguage) %>% summarise(Count = n())


## set the levels 
Languages_summary$HaveWorkedLanguage = Languages_summary$HaveWorkedLanguage %>% 
  factor(levels = Languages_summary$HaveWorkedLanguage[order(Languages_summary$Count)]) 
Languages_summary = Languages_summary[Languages_summary$Count > 10,]
ggplot(Languages_summary,aes(HaveWorkedLanguage,Count)) + geom_bar(stat='identity') + coord_flip()

##
## Method of Learning
##

Education = Data2017 %>% select(Respondent,EducationTypes,SelfTaughtTypes,DeveloperType) %>% 
  filter(!is.na(EducationTypes) & !is.na(EducationTypes) & DeveloperType %in% 
           c('Data scientist','Machine learning specialist','Developer with a statistics or mathematics background'))

Education_long <- cSplit(indt = Education, 
                         splitCols = c('EducationTypes','SelfTaughtTypes','DeveloperType'), sep = ";", direction = "long")

x = data.frame(table(Education_long$DeveloperType,Education_long$EducationTypes))
y = data.frame(table(Education_long$EducationTypes,Education_long$SelfTaughtTypes))

defaulter<-rbind(x,y)

# Plotting
library(googleVis)
sk2 <- gvisSankey(defaulter, from='Var1', to='Var2', weight='Freq',
                  options=list(height=400, width=600))
plot(sk2)


##
## Time Series of Growth of Languages
##

Series2017 = Data2017 %>% select(Respondent,HaveWorkedLanguage) %>% 
  filter(!is.na(HaveWorkedLanguage))

Series_long = cSplit(indt = Series2017, splitCols = 'HaveWorkedLanguage', sep = ";", direction = "long")

# Generic Function for all Datasets

LanguageSummary = function(Series_long,InputYear) {
  Summary = Series_long %>% 
    group_by(HaveWorkedLanguage) %>% 
    summarise(Freq = n()) %>%
    mutate(Percentage = round(Freq/sum(Freq)*100,3), Year = InputYear) %>%
    filter(HaveWorkedLanguage %in% c('Python','SQL','Java','R'))
  return(Summary)
}

# Extract the value
Summary2017 = LanguageSummary(Series_long,'2017')


# Data from 2016
Data2016 = read.csv('data/2016 Stack Overflow Survey Results/2016 Stack Overflow Survey Responses.csv')

Series2016 = Data2016 %>% select(X,tech_do) %>% 
  filter(!is.na(tech_do)) %>%
  rename(Respondent = X, HaveWorkedLanguage = tech_do)
Series_long = cSplit(indt = Series2016, splitCols = 'HaveWorkedLanguage', sep = ";", direction = "long")
Summary2016 = LanguageSummary(Series_long,'2016')

# Data from 2015

Data2015 = read.csv('data/2015 Stack Overflow Developer Survey Responses.csv',skip = 1)

Series2015 = Data2015 %>% select(starts_with("Current.Lang"))
x = paste()
  filter(!is.na(tech_do)) %>%
  rename(Respondent = X, HaveWorkedLanguage = tech_do)


Series_long = cSplit(indt = Series2016, splitCols = 'HaveWorkedLanguage', sep = ";", direction = "long")
Summary2016 = LanguageSummary(Series_long,'2016')


# Final Plot
Summary = rbind(Summary2017,Summary2016)

ggplot(Summary,aes(x=Year,y=Percentage)) + geom_line() + facet_wrap(~ HaveWorkedLanguage)
