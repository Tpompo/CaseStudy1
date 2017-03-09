##Introduction
##The following code examines data for the GDP and Incomes of many countries. I attempt to answer a number of questions
##about possible relaionships within the data. The data used is from the world bank, and specfically are 
##GDP data and educatonal data of many countries.




##We must load the following packages for this study.
library(dplyr)
library(tidyr)
library(ggplot2)

##It is good practice to clear your environment before you start.
rm (list = ls())

#Load the 1st Dataset
GDPData <- read.csv("gdpdata.csv", stringsAsFactors = F, header = F)

#Remove Unnecessary Rows
GDPData2 <- GDPData[6:195,]

#Remove Unnecessary Columns

GDPData3 <- GDPData2[,c(1,2,5)]

#Check Data
str(GDPData3)
head(GDPData3)
tail(GDPData3)
names(GDPData3)

#Load 2nd Data Set
EDData <- read.csv("eddata.csv", stringsAsFactors = F,header=T)

#Remove Unnecessary COlumns
EDData1 <- EDData[,c(1,2,3)]

#Check Data
head(EDData1)
dim(EDData1)
str(EDData1)
names(EDData1)
tail(EDData1)

##We want to start by renaming the columns in our GDP Dataset.
GDPData3 <- rename(GDPData3, CountryCode=V1, GDPrank=V2, Income=V5)
str(GDPData3)

##We then need to tranform GDPrank and Income into numeric variables.
GDPData3$Income <- as.numeric(GDPData3$Income)
GDPData3$GDPrank <- as.numeric(GDPData3$GDPrank)

#After this we merge the data sets using the CountryCode Variable
mergedata1 <- merge(x = GDPData3, y = EDData1, by = "CountryCode", all.x = FALSE, all.y = FALSE)


#Check New Data
names(mergedata1)
str(mergedata1)
head(mergedata1)
tail(mergedata1)

#How many rows are matched? 189 is the answer
nrow(mergedata1)

sortmergedata <- mergedata1[order(-mergedata1$GDPrank) , ]
str(sortmergedata)


sortmergedata[13,]

#The 13th country in reverse GDPRank Order is Kitts and Nevis.


attach(mergedata1)

#Here we sort the countries into quantiles based on their GDPRank
mergedata1$gdpgroups[GDPrank <= 38] <- 1
mergedata1$gdpgroups[GDPrank > 38 & GDPrank <= 74] <- 2
mergedata1$gdpgroups[GDPrank > 74 & GDPrank <= 112] <- 3
mergedata1$gdpgroups[GDPrank > 112 & GDPrank <= 150] <- 4
mergedata1$gdpgroups[GDPrank >= 158] <- 5
summary(mergedata1$gdpgroups)

table(mergedata1$gdpgroups,Income.Group)

#5 countries are lower middle income yet still among highest 38 ranking countries


#Find Average GDP Rankings for High Income OECD vs High Income nonOECD
gdphigh <- subset(x = mergedata1, Income.Group == "High income: OECD")
mean(gdphigh$GDPrank)

#Average for OECD is 32.96667

gdphighnon <- subset(x = mergedata1, Income.Group == "High income: nonOECD")
mean(gdphighnon$GDPrank)


#Average for Non-OECD is 91.91. The NonOECD are ranked significantly lower than the OECD.


#Find Summaries of each individual Income Group
gdphighnon <- subset(x = mergedata1, Income.Group == "High income: nonOECD")
summary(gdphighnon$GDPrank)

gdphigh <- subset(x = mergedata1, Income.Group == "High income: OECD")
summary(gdphigh$GDPrank)

gdpupper <- subset(x = mergedata1, Income.Group == "Upper middle income")
summary(gdpupper$GDPrank)

gdplower <- subset(x = mergedata1, Income.Group == "Lower middle income")
summary(gdplower$GDPrank)

gdplow <- subset(x = mergedata1, Income.Group == "Low income")
summary(gdplow$GDPrank)

#Plot the GDP positions

ggplot(data=mergedata1,aes(Income.Group, GDPrank, color = Income.Group))



#Conclusion
##Countries with OECD and a high income have considerably higher GDPs than those without OECD.
##This is interesting but not nearly enough information to make any kind of definitive statement.
##We should conduct further analysis.
