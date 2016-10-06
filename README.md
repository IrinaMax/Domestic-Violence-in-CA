# Domestic-Violence-in-CA
 The goal of this script is to get an overview of the accidents in partnership in California to study Domestic Violentce data set. 

##title: "Domestic Violentce accidents"
##author: "IrinaMax"
##date: "30 Sept 2016"

  
#  The goal of this script is to get an overview of the accidents in partnership in California
#to study Domestic Violentce data set. That is how many accidents happened by year, 
#how much with weapons and without weapons, and how this dinamic was chanes during 2005-2014
#I have used the file dv_data_set_2005-2014.csv, which I found online and its open for 
#working with and develop any researchand analysis.

#I found it interesting that the accident with weapon getting less with time. 
#But apperently it is possible for

#I also tried to understand  and make move visual plots easy to understand where 
#in what country accidents happened more and predict some forecast. This code 
#in proggess and i still working on it. 
#If I made mistakes, let me know!
  
##  Load libraries

library(dplyr) # reshaping
library(tidyr) # reshaping
library(ggplot2) # Visualization
library(ggthemes) # Visualization
library(data.table)
##Load the data

studyDV <- read.csv("dv_data_set_2005-2014.csv")
##Get first overview
dim(studyDV)
str(studyDV)

#Get first overview

tbl_df(studyDV)
glimpse(studyDV)
summary(studyDV)

table1 <- data.frame(studyDV$TOTAL_CALLS, studyDV$WEAPONS_INVOLVED, studyDV$NO_WEAPONS,
                     studyDV$SUB_KNIFE, studyDV$SUB_PERSONAL)      
colSums(table1)


#This dataset contains 81866 observations and 11 variables. 
#The first column represents the the year when accident happend.

#Start with summarizing the data

table(studyDV$YEAR, studyDV$COUNTY)
ggplot(studyDV, aes(YEAR)) + geom_bar() # make Rplot1 by year
ggplot(studyDV, aes(COUNTY)) + geom_bar() # make Rplot2 by county
ggplot(studyDV, aes(NCIC_AGENCY_CODE)) + geom_bar()  # make Rplot3 by agency code

m <- mean(studyDV$COUNTY)
WEAPONS_INVOLVED = mean(WEAPONS_INVOLVED))
ddply(studyDV, .(COUNTY), summarize,  TOTAL_CALLS=mean(TOTAL_CALLS), 
      NO_WEAPONS=mean(NO_WEAPONS), WEAPONS_INVOLVED=mean(WEAPONS_INVOLVED))
library(plyr)
ddply(studyDV, .(COUNTY), summarize,  TOTAL_CALLS=mean(TOTAL_CALLS), 
      NO_WEAPONS=mean(NO_WEAPONS), WEAPONS_INVOLVED=mean(WEAPONS_INVOLVED))
pcounty<- ddply(studyDV, .(COUNTY), summarize,  TOTAL_CALLS=mean(TOTAL_CALLS), 
                NO_WEAPONS=mean(NO_WEAPONS), WEAPONS_INVOLVED=mean(WEAPONS_INVOLVED))
ggplot(pcounty,aes(x=COUNTY, y= TOTAL_CALLS))

count(studyDV$NCIC_AGENCY_CODE)
#The dataset contains 58 countys and 745 NCIC agancys, where was detected accident.

op <- par(mfrow = c(2, 2)) ##will make my histogramm smaller
utils::str(hist(studyDV$TOTAL_CALLS, col = "gray", lables = T ))
utils::str(hist(studyDV$NO_WEAPONS, col = "gray", lables = T ))
utils::str(hist(studyDV$WEAPONS_INVOLVED, col = "gray", lables = T ))

hist(sqrt(studyDV$YEAR), breaks = 50, col = "lightblue", border = "pink")
hist(sqrt(studyDV$TOTAL_CALLS), breaks = 50, col = "lightblue", border = "pink")
hist(sqrt(studyDV$NO_WEAPONS), breaks = 50, col = "lightblue", border = "pink") 
hist(sqrt(studyDV$WEAPONS_INVOLVED), breaks = 50, col = "lightblue", border = "pink")

summarise(studyDV, avg=mean(TOTAL_CALLS)) # means
count(studyDV$TOTAL_CALLS/count(studyDV$COUNTY))
summarise(studyDV, sd=sd(TOTAL_CALLS)) # sd
summarise(studyDV, max=max(TOTAL_CALLS))
summarise(studyDV, max=max(WEAPONS_INVOLVED)) # max
summarise(studyDV, max=max(NO_WEAPONS))
colors = c("red", "yellow", "green", "violet", "orange", 
           "blue", "pink", "cyan")
count(studyDV$WEAPONS_INVOLVED)
count(studyDV$TOTAL_CALLS)

#The avarage call by year is 20.22296, with a standard deviation of 69.67994.
#The  maximum calls  2265 and with involving weapons 1779 without weapon 712,\
#and with personal abuse 1392.

table(studyDV$COUNTY)
# make plot

library(ggplot2)
ggplot(studyDV, aes(YEAR, fill = COUNTY)) + geom_bar()

table(studyDV$WEAPONS_INVOLVED)
# make plot
ggplot(studyDV, aes(x=YEAR, fill=COUNTY)) + geom_bar() 


table(studyDV$NCIC_AGENCY_CODE)
ggplot(studyDV, aes(NCIC_AGENCY_CODE, fill=COUNTY)) + geom_bar() # make plot

ggplot(studyDV, aes(NCIC_AGENCY_CODE, fill=WEAPONS_INVOLVED)) + geom_bar() # make plot
table(studyDV$WEAPONS_INVOLVED)

ggplot(meanpyear, aes_all(TOTAL_CALLS)) + geom_bar()
#share improve this answer
#edited Sep 30 '13 at 21:41
