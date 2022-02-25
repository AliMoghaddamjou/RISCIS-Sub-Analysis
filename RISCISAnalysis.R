#################################
####RISCIS  ANALYSIS#############
#######Prepared BY:##############
###########ALI MOGHADDAMJOU######

##SET WD THE FOLDER WITH THE DATAIN


##Loading the data from the CSV file
##Please refer to stata do file in repository for the coes to obtain the file loaded here
baseData<-read.csv('alldata.csv')


## Replacing the race value 99 to missing 
baseData$RACE[baseData$RACE == 99]<- NA


##DEFINING DATA TREATMENT AND OUTCOME

data<-baseData ##data

Treatment <- data$trtgrp  ##unblinded treatment group 

##Outcome. UEM is used in this case. This can be changed to LEMDiff6m or TOTMDiff6m for the other outcomes 
Outcome <- data$UEMDiff6m 