---
title: "CaseStudy1"
author: "Tommy Pompo"
date: "March 9, 2017"
output: 
  html_document:
    keep_md: true
---

# Introduction
The following code examines data for the GDP and Incomes of many countries. I attempt to answer a number of questions
about possible relaionships within the data. The data used is from the world bank, and specfically are 
GDP data and educatonal data of many countries.




We must load the following packages for this study.
```{r}
library(dplyr)
library(tidyr)
library(ggplot2)
```
It is good practice to clear your environment before you start.
```{r}
rm (list = ls())
```
Load the 1st Dataset
```{r}
GDPData <- read.csv("gdpdata.csv", stringsAsFactors = F, header = F)
```
Remove Unnecessary Rows
```{r}
GDPData2 <- GDPData[6:195,]
```
Remove Unnecessary Columns
```{r}
GDPData3 <- GDPData2[,c(1,2,5)]
```
Check Data
```{r}
str(GDPData3)
head(GDPData3)
tail(GDPData3)
names(GDPData3)
```
Load 2nd Data Set
```{r}
EDData <- read.csv("eddata.csv", stringsAsFactors = F,header=T)
```
Remove Unnecessary COlumns
```{r}
EDData1 <- EDData[,c(1,2,3)]
```
Check Data
```{r}
head(EDData1)
dim(EDData1)
str(EDData1)
names(EDData1)
tail(EDData1)
```
We want to start by renaming the columns in our GDP Dataset.
```{r}
GDPData3 <- rename(GDPData3, CountryCode=V1, GDPrank=V2, Income=V5)
str(GDPData3)
```
We then need to tranform GDPRank into a numeric variable.
```{r}
GDPData3$GDPrank <- as.numeric(GDPData3$GDPrank)
```
After this we merge the data sets using the CountryCode Variable
```{r}
mergedata1 <- merge(x = GDPData3, y = EDData1, by = "CountryCode", all.x = FALSE, all.y = FALSE)
```

Check New Data
```{r}
names(mergedata1)
str(mergedata1)
head(mergedata1)
tail(mergedata1)
```
How many rows are matched? 189 is the answer
```{r}
nrow(mergedata1)
```

```{r}
sortmergedata <- mergedata1[order(-mergedata1$GDPrank) , ]
str(sortmergedata)
sortmergedata[13,]
```
The 13th country in reverse GDPRank Order is Kitts and Nevis.

```{r}
attach(mergedata1)
```
Here we sort the countries into quantiles based on their GDPRank
```{r}
mergedata1$gdpgroups[GDPrank <= 38] <- 1
mergedata1$gdpgroups[GDPrank > 38 & GDPrank <= 74] <- 2
mergedata1$gdpgroups[GDPrank > 74 & GDPrank <= 112] <- 3
mergedata1$gdpgroups[GDPrank > 112 & GDPrank <= 150] <- 4
mergedata1$gdpgroups[GDPrank >= 158] <- 5
summary(mergedata1$gdpgroups)
table(mergedata1$gdpgroups,Income.Group)
```
6 countries are lower middle income yet still among highest 38 ranking countries


Find Average GDP Rankings for High Income OECD vs High Income nonOECD
```{r}
gdphigh <- subset(x = mergedata1, Income.Group == "High income: OECD")
mean(gdphigh$GDPrank)
```
Average for OECD is 32.96667.
```{r}
gdphighnon <- subset(x = mergedata1, Income.Group == "High income: nonOECD")
mean(gdphighnon$GDPrank)
```

Average for Non-OECD is 91.9. The NonOECD are ranked significantly lower than the OECD.


Find Summaries of each individual Income Group
```{r}
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
```
Plot the GDP positions
```{r}
ggplot(data=mergedata1,aes(Income.Group, GDPrank, color = Income.Group))
```


#Conclusion
Countries with OECD and a high income have considerably higher GDPs than those without OECD.
This is interesting but not nearly enough information to make any kind of definitive statement.
We should conduct further analysis.
