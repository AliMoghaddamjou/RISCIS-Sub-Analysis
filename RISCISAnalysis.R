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