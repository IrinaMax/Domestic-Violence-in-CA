##My work with Domestic Violence-Related Calls for Assistance

 The goal of this script is to get an overview of the accidents in partnership in California to study Domestic Violentce data set. 

##title: "Domestic Violentce accidents"
##author: "IrinaMax"
##date: "30 Sept 2016"

 Domestic violence is defined as “...abuse committed against an adult or a fully emancipated minor who is a spouse, former spouse, cohabitant, former cohabitant, or person with whom the suspect has had a child or is having or has had a dating or engagement relationship.” [13700(b) PC]
 Abuse is defined as “...intentionally or recklessly causing or attempting to cause bodily injury, or placing another person in reasonable apprehension of imminent serious bodily injury to himself or herself, or another.” [13700(a) PC]
 The definition of “domestic violence” is subject to varying interpretations by law enforcement agencies. As a result, different types of domestic violence relationships may be included in the database.
 Included in the data are any cases that resulted in a report being written by the responding law enforcement agency. Therefore, data include both cases where an arrest was made and those where circumstances did not warrant an arrest.
Domestic violence-related calls for assistance that involved the use, or threat to use, of a firearm, knife or cutting instrument or other dangerous weapon are reported according to the type of weapon used regardless of the outcome or injury.
 In 2002, law enforcement agencies were given clarification about reporting personal weapons. This clarification corresponds to a notable decrease in the number of personal weapons reported. The use of a personal weapon such as hands, fists, or feet was reported as a weapon only if the assault was considered an aggravated assault under Uniform Crime Reporting (UCR) guidelines. An aggravated assault is an unlawful attack by one person upon another for the purpose of inflicting severe or aggravated bodily injury, such as broken bones, internal injuries, or cuts requiring stitches.


  
  The goal of this script is to get an overview of the accidents in partnership in California
to study Domestic Violentce data set. That is how many accidents happened by year, 
how much with weapons and without weapons, and how this dinamic was chanes during 2005-2014
I have used the file dv_data_set_2005-2014.csv, which I found online and its open for 
working with and develop any researchand analysis.

##I found it interesting that the accident with weapon getting less with time. 
##But apperently it is possible for

##I also tried to understand  and make move visual plots easy to understand where 
##in what country accidents happened more and predict some forecast. This code 
##in proggess and i still working on it. 
##If I made mistakes, let me know!
  
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

m <- mean(studyDV$TOTAL_CALLS)
m

m_weap = mean(studyDV$WEAPONS_INVOLVED)
m_weap


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
