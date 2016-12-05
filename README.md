##My work with Domestic Violence-Related Calls for Assistance using Exploratory and R code.

 The goal of this script is to get an overview of the incidents in partnership in California to study Domestic Violentce data set. 

##title: "Domestic Violentce accidents"
##author: "IrinaMax"
##date: "30 Sept 2016"

 Domestic violence is defined as “...abuse committed against an adult or a fully emancipated minor who is a spouse, former spouse, cohabitant, former cohabitant, or person with whom the suspect has had a child or is having or has had a dating or engagement relationship.” [13700(b) PC]
 Abuse is defined as “...intentionally or recklessly causing or attempting to cause bodily injury, or placing another person in reasonable apprehension of imminent serious bodily injury to himself or herself, or another.” [13700(a) PC]
 The definition of “domestic violence” is subject to varying interpretations by law enforcement agencies. As a result, different types of domestic violence relationships may be included in the database.
 Included in the data are any cases that resulted in a report being written by the responding law enforcement agency. Therefore, data include both cases where an arrest was made and those where circumstances did not warrant an arrest.
Domestic violence-related calls for assistance that involved the use, or threat to use, of a firearm, knife or cutting instrument or other dangerous weapon are reported according to the type of weapon used regardless of the outcome or injury.
 In 2002, law enforcement agencies were given clarification about reporting personal weapons. This clarification corresponds to a notable decrease in the number of personal weapons reported. The use of a personal weapon such as hands, fists, or feet was reported as a weapon only if the assault was considered an aggravated assault under Uniform Crime Reporting (UCR) guidelines. An aggravated assault is an unlawful attack by one person upon another for the purpose of inflicting severe or aggravated bodily injury, such as broken bones, internal injuries, or cuts requiring stitches.


  
  The goal of this script is to get an overview of the incidents in partnership in California
to study Domestic Violentce data set. That is how many incidents happened by year, 
how much with weapons and without weapons, and how this dynamic was changed during 2005-2014
I have used the file dv_data_set_2005-2014.csv, which I found online and its open for 
working with and develop any research and analysis.
![domesticviolenceca2015](https://cloud.githubusercontent.com/assets/16123495/20820260/c48f15da-b7ee-11e6-8211-2b50c4e3cfd9.png)

Number of total calls in California during 2005-2014 was increase from  from 6595 to 8766.
Highst level of total domestic violence calls during 2005-2014 belong to the Los Angeles county - 11754 registred calls. Orage county with 4775 call on the second place. The third place is shared  by Sun Bernardino with 3700 and Riverside County with 3696 total calls..
![total_calls_ca](https://cloud.githubusercontent.com/assets/16123495/20820458/21c38802-b7f0-11e6-976c-4cd30b7c4f01.png)

I found it interesting that the accident with weapon getting less with time. 
![wepons_involved_ca2005_2014](https://cloud.githubusercontent.com/assets/16123495/20821722/bc3e9008-b7f9-11e6-8da7-3d0afa2c7624.png)
![weapon involved_by countys_ca](https://cloud.githubusercontent.com/assets/16123495/20822485/ec90dabc-b7ff-11e6-8e7c-cb00b235288b.png)
We can see on this plot of total calls in CA with all kind of weponds involved like knifes or cutting instruments,firearms or guns, and other personal( hand, fists or fits in fight).
Victims accessing services from State-funded domestic violence programs report serious threats to
their personal safety: 65% have weapons used against them.

![all_weaponds_involved_ca](https://cloud.githubusercontent.com/assets/16123495/20825131/faa6b07e-b816-11e6-9232-0716b9b92b69.png)
The boxplot shows that San Diego is detected as a leader of the citys where people used weapons.

![weapons_involved_ca](https://cloud.githubusercontent.com/assets/16123495/20820465/2ae45010-b7f0-11e6-869a-8e0062c82aad.png)
But apperently it is possible for

I also tried to understand  and make move visual plots easy to understand where 
in what country incidents happened more and predict some forecast. This code 
in proggess and i still working on it. 

  
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
    
    studyDV.TOTAL_CALLS studyDV.WEAPONS_INVOLVED       studyDV.NO_WEAPONS        studyDV.SUB_KNIFE
                  1655573                   696512                   959061                    32154 
    studyDV.SUB_PERSONAL 
                  551695 


This dataset contains 81866 observations and 11 variables. 
The first column represents the the year when accident happend.

Start with summarizing the data

    table(studyDV$YEAR, studyDV$COUNTY)
    ggplot(studyDV, aes(YEAR)) + geom_bar() # make Rplot1 by year
    ggplot(studyDV, aes(COUNTY)) + geom_bar() # make Rplot2 by county
    ggplot(studyDV, aes(NCIC_AGENCY_CODE)) + geom_bar()  # make Rplot3 by agency code

    m <- mean(studyDV$TOTAL_CALLS)
    m
    [1] 20.22296
Mean of total calls by California is 20.22296.
    
    m_weap = mean(studyDV$WEAPONS_INVOLVED)
    m_weap
    [1] 8.507952
Mean with weapons involved is 8.507952.    


     library(plyr)
     ddply(studyDV, .(COUNTY), summarize,  TOTAL_CALLS=mean(TOTAL_CALLS), 
           NO_WEAPONS=mean(NO_WEAPONS), WEAPONS_INVOLVED=mean(WEAPONS_INVOLVED))
     pcounty<- ddply(studyDV, .(COUNTY), summarize,  TOTAL_CALLS=mean(TOTAL_CALLS), 
                NO_WEAPONS=mean(NO_WEAPONS), WEAPONS_INVOLVED=mean(WEAPONS_INVOLVED))
     ggplot(pcounty,aes(x=COUNTY, y= TOTAL_CALLS))

     count(studyDV$NCIC_AGENCY_CODE)
The dataset contains 58 countys and 745 NCIC agancys, where was detected accident.

     op <- par(mfrow = c(2, 2)) ##will make my histogramm smaller
     utils::str(hist(studyDV$TOTAL_CALLS, col = "gray", lables = T ))
     utils::str(hist(studyDV$NO_WEAPONS, col = "gray", lables = T ))
     utils::str(hist(studyDV$WEAPONS_INVOLVED, col = "gray", lables = T ))
![histograms_dv](https://cloud.githubusercontent.com/assets/16123495/20827044/0dde1b70-b824-11e6-8177-dae31f41126a.png)     

     hist(sqrt(studyDV$YEAR), breaks = 50, col = "lightblue", border = "pink")
     hist(sqrt(studyDV$TOTAL_CALLS), breaks = 50, col = "lightblue", border = "pink")
     hist(sqrt(studyDV$NO_WEAPONS), breaks = 50, col = "lightblue", border = "pink") 
     hist(sqrt(studyDV$WEAPONS_INVOLVED), breaks = 50, col = "lightblue", border = "pink")
![histograms2_freq_dv](https://cloud.githubusercontent.com/assets/16123495/20827047/116d0b98-b824-11e6-94d8-166e3ccaf497.png)     


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

The avarage call by year is 20.22296, with a standard deviation of 69.67994.
The  maximum calls  2265 and with involving weapons 1779 without weapon 712,\
and with personal abuse 1392.

     table(studyDV$COUNTY)
 make plot

     library(ggplot2)
     ggplot(studyDV, aes(YEAR, fill = COUNTY)) + geom_bar()
![ggplot2_dv](https://cloud.githubusercontent.com/assets/16123495/20827078/43555c5a-b824-11e6-9ff8-7740b2f16403.png)     

     table(studyDV$WEAPONS_INVOLVED)
     table(studyDV$NCIC_AGENCY_CODE)
     ggplot(studyDV, aes(NCIC_AGENCY_CODE, fill=COUNTY)) + geom_bar() # make plot
![rplot_agency_code](https://cloud.githubusercontent.com/assets/16123495/20827254/9b517f64-b825-11e6-8ad0-f786db58141b.png)

     ggplot(studyDV, aes(NCIC_AGENCY_CODE, fill=WEAPONS_INVOLVED)) + geom_bar() # make plot
     table(studyDV$WEAPONS_INVOLVED)

     ggplot(meanpyear, aes_all(TOTAL_CALLS)) + geom_bar()

edited Nov 26 '13 at 21:41
