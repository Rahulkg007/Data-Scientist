## Load Libraries
require(ggplot2)
require(dplyr)

## Set Working Directory
setwd("C:/Users/rahul/Google Drive/RMIT/Semester 2/Data Visualization/Assignment 2")

## Read files
Data2017 = read.csv('data/survey_results_public.csv')
head(Data2017)
str(Data2017)

# Analysis
unique(Data2017$DeveloperType)

x = filter(Data2017, DeveloperType == 'Data scientist')
x = filter(Data2017, grepl('Data scientist', DeveloperType))
#|Machine Learning|Statistics


z = x$HaveWorkedLanguage

x$HaveWorkedLanguage = str_trim(x$HaveWorkedDatabase)
x[1344,]
library(stringr)
a = as.data.frame(str_split_fixed(z[1:20], '\\;', 40))
# a <- a[!is.na(a)]
# max( na.omit(a) )
# print(a, quote=FALSE)
# table(a)


Path = as.data.frame(table(a))

colnames(Path) = c('Language','Frequency')
Path.All = transform(Path, Path = reorder(Language,Frequency))




##
## New Analysis
##

## Read files
Data2017 = read.csv('data/survey_results_public.csv')
head(Data2017)
str(Data2017)

Sal = Data2017 %>% 
  select(Country,Salary) %>% 
  filter(Country %in% c('United States','United Kingdom','Australia') & !is.na(Salary)) 

x = Sal %>% group_by(Country) %>% summarise(Mean = mean(Salary))


sample = Data2017 %>% select(Respondent,Country,DeveloperType,Salary) %>% 
  filter(!is.na(DeveloperType) & !is.na(Salary) & Country %in% c('United States','United Kingdom','Australia'))

# 
# & Country %in% c('United States','United Kingdom','Australia')

library(splitstackshape)
df2 <- cSplit(sample, "DeveloperType", sep = ";", direction = "long")

conversionUSD = data.frame(Country = c('United States','United Kingdom','Australia'),
                           exchange = c(1,1.2512,0.7574))

df3 = merge(df2,conversionUSD, by ='Country')
df3 = df3 %>% mutate (SalaryUSD = Salary * exchange)

df4 = df3 %>% group_by(DeveloperType) %>% summarise(count = n(), MeanSalary = mean(SalaryUSD)) %>% filter(DeveloperType != 'Other')

## set the levels in order we want


df4$DeveloperType = df4$DeveloperType %>% factor(levels = df4$DeveloperType[order(df4$MeanSalary)]) 
ggplot(df4,aes(DeveloperType,MeanSalary)) + geom_bar(stat='identity') + coord_flip()

ggplot(df4,aes(DeveloperType,count)) + geom_bar(stat='identity') + coord_flip()
