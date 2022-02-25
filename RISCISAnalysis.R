############################################################################
############################################################################
###                                                                      ###
###                           RISCIS ANALYSIS                            ###
###                           ALI MOGHADDAMJOU                           ###
###                                                                      ###
############################################################################
############################################################################

# clear console
cat("\f")

# clear environment
# be careful because it is irreversible!
rm(list = ls())

##Function to check if required packages are installed 
requiredPackages <- c("gplot2", "jtools", "ggstance", "patchwork", "lme4", "splines", "splines", "broom.mixed", "MASS",  "evd", "dplyr", "tidyverse",  "ggpubr")
packagesLoading <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packagesLoading(requiredPackages) ## Displaying if Packages installed


##Loading Package. This is wrapped in invisible to not display all packages loaded by user
invisible(lapply(requiredPackages, library, character.only = TRUE))


##SET WD THE FOLDER WITH THE DATAIN
setwd("C:/Users/Ali/OneDrive/Projects/RISCIS/IntermAnalysisFeb28/gitRepository/data")

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


##Subset Data Based on ASIA

dataASIA <- split(data, f = data$baseASIA)
ASIAA <- dataASIA[["A"]]
ASIAB <-dataASIA[["B"]]
ASIAC <-dataASIA[["C"]]


##########SECONDARY OUTCOME DATA

baseData_SO <-  read.csv('alldataSO.csv')  ## LOADING FILES
#SPLITING Based on ASIA 
dataASIA_SO <- split(baseData_SO, f = baseData_SO$baseASIA)
ASIAA_SO <- dataASIA_SO[["A"]]
ASIAB_SO <-dataASIA_SO[["B"]]
ASIAC_SO <-dataASIA_SO[["C"]]
data_SO <- baseData_SO


#############################################
###############CODE BLOCK 1##################
#############################################
#################Creating Graphs of Endpoints

##Graphs #######################################
##First Create Subset Data in long form 

graphDataAll <- data[c("trtgrp", "UEMDiff6m", "LEMDiff6m", "TOTMDiff6m")] %>%
  pivot_longer(-trtgrp, names_to = "variables", values_to = "value")  

longA <- ASIAA[c("trtgrp", "UEMDiff6m", "LEMDiff6m", "TOTMDiff6m")] %>%
  pivot_longer(-trtgrp, names_to = "variables", values_to = "value")  
longB <- ASIAB[c("trtgrp", "UEMDiff6m", "LEMDiff6m", "TOTMDiff6m")] %>%
  pivot_longer(-trtgrp, names_to = "variables", values_to = "value")  
longC <- ASIAC[c("trtgrp", "UEMDiff6m", "LEMDiff6m", "TOTMDiff6m")] %>%
  pivot_longer(-trtgrp, names_to = "variables", values_to = "value")  


# Create the plot

##All
AllGraph <- ggboxplot(
  graphDataAll, x = "trtgrp", y = "value",
  fill = "trtgrp", palette = "npg", legend = "none", 
  ggtheme = theme_pubr(border = TRUE)
)+
  facet_wrap(~variables) +  ggtitle("All Patients") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Treatment") +ylab(NULL)

##A
AGraph <- ggboxplot(
  longA, x = "trtgrp", y = "value",
  fill = "trtgrp", palette = "npg", legend = "none", 
  ggtheme = theme_pubr(border = TRUE)
)+
  facet_wrap(~variables) +  ggtitle("ASIA A") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Treatment")+ylab(NULL)

##B
BGraph <- ggboxplot(
  longB, x = "trtgrp", y = "value",
  fill = "trtgrp", palette = "npg", legend = "none", 
  ggtheme = theme_pubr(border = TRUE)
)+
  facet_wrap(~variables) +  ggtitle("ASIA B") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Treatment")+ylab(NULL)
##C
CGraph <- ggboxplot(
  longC, x = "trtgrp", y = "value",
  fill = "trtgrp", palette = "npg", legend = "none", 
  ggtheme = theme_pubr(border = TRUE)
)+
  facet_wrap(~variables) +  ggtitle("ASIA C") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Treatment")+ylab(NULL)


AllGraph +AGraph +BGraph +CGraph


#############################################
###############CODE BLOCK 2##################
#############################################
##################Primary Statistical Testing 


lapply(list(data$UEMDiff6m ,data$LEMDiff6m ,data$TOTMDiff6m),  ## Defining list of outcomes
       function(outcomeList) {
ttest<- t.test(outcomeList~Treatment, mu = 0, alternative = "greater")  ## Two sample one-way T test

})


#############################################
###############CODE BLOCK 3##################
#############################################
##############################ASIA DIFFERENCE

##Replacing 9 with missing 

data$ASIA_6[data$ASIA_6 == 9]<- NA
data$ASIA_1[data$ASIA_1 == 9]<- NA

##Defining the difference variable
ASIADiff6m <- data$ASIA_6 - data$ASIA_1

##Creating Binary Data
ASIADiff6m_greater0 <-ifelse(ASIADiff6m<1,"Negative","Positive")

##TwoBy2 Table 
ASIADIFF_2b2<-twoby2(Treatment, ASIADiff6m_greater0)

##MOSAIC PLOT
tableASIA <- table( Treatment, ASIADiff6m_greater0 )
ASIADIFF_mplot<- mosaicplot(tableASIA, color = c("red", "green"), main = "ASIA Change >0",
                   xlab = "Treatment", ylab = "Outcome  ")


#############################################
###############CODE BLOCK 4##################
#############################################
######Changes in Neurological Level of Injury

ttest_NLI<- t.test(data$NLIDiff6m~Treatment, mu = 0, alternative = "greater")  ## Two sample one-way T test
ttest_NLI

##Creating Binary Data
NLIADiff6m_greater0 <-ifelse(ASIADiff6m<1,"Negative","Positive")

##TwoBy2 Table 
NLIDIFF_2b2<-twoby2(Treatment, NLIADiff6m_greater0)

##MOSAIC PLOT

tableNLI <- table( Treatment, NLIADiff6m_greater0 )
NLIDIFF_mplot<- mosaicplot(tableNLI, color = c("red", "green"), main = "NLI Change >0",
                            xlab = "Treatment", ylab = "Outcome  ")

#############################################
###############CODE BLOCK 5##################
#############################################
######################################SF36
par(mfrow=c(2,2))
lapply(list(data_SO,ASIAA_SO,ASIAB_SO, ASIAC_SO ),
       function(w) {
         
         par(mfrow=c(2,2))
         sf36_data <- w[c("treatment",  "SF36Physical6m",  "SF36Mental6m")]
         mydata <- sf36_data
         mydata.long <- mydata %>%
           pivot_longer(-treatment, names_to = "variables", values_to = "value")  
         
         # Create the plot
         myplot <- ggboxplot(
           mydata.long, x = "treatment", y = "value",
           fill = "treatment", palette = "npg", legend = "none",
           ggtheme = theme_pubr(border = TRUE)
         )+
           facet_wrap(~variables)
         
         myplot})


#############################################
###############CODE BLOCK 6##################
#############################################
######################################SCIM


FL<- lapply(list(data_SO,ASIAA_SO,ASIAB_SO, ASIAC_SO ),
            function(w) {
              box_plot <- ggplot(data = w, aes(x = treatment, y = w$TotalSCIM6m 
              ))
              box_plot +
                ggtitle("Boxplot")+
                geom_boxplot() +
                
                geom_point(shape = 5,
                           color = "steelblue") +
                stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
                stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
                             vjust = 3, aes(label = paste("Mean:", round(..y.., digits = 1))))+
                stat_summary(fun = median, geom = "point", col = "blue") +  # Add points to plot
                stat_summary(fun = median, geom = "text", col = "blue",     # Add text to plot
                             vjust = -2.5, aes(label = paste("Median:", round(..y.., digits = 1))))+
                
                theme_classic()    +
                theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank() 
                )
            })

p1<- FL[[1]]+ ggtitle ("All Patients")
p2<- FL[[2]]+ ggtitle ("ASIA A")
p3<- FL[[3]]+ ggtitle ("ASIA B")
p4<- FL[[4]]+ ggtitle ("ASIA C")


par(mfrow=c(2,2))
p1+p2+p3+p4

#############################################
###############CODE BLOCK 7##################
#############################################
######################################VAS


FL_VAS<- lapply(list(data_SO,ASIAA_SO,ASIAB_SO, ASIAC_SO ),
            function(w) {
              box_plot <- ggplot(data = w, aes(x = treatment, y = w$EQ5DVAS6m  
              ))
              box_plot +
                ggtitle("Boxplot")+
                geom_boxplot() +
                
                geom_point(shape = 5,
                           color = "steelblue") +
                stat_summary(fun = mean, geom = "point", col = "red") +  # Add points to plot
                stat_summary(fun = mean, geom = "text", col = "red",     # Add text to plot
                             vjust = 1.5, aes(label = paste("Mean:", round(..y.., digits = 1))))+
                stat_summary(fun = median, geom = "point", col = "blue") +  # Add points to plot
                stat_summary(fun = median, geom = "text", col = "blue",     # Add text to plot
                             vjust = -2.0, aes(label = paste("Median:", round(..y.., digits = 1))))+
                
                theme_classic()    +
                theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank() 
                )
            })

p1<- FL_VAS[[1]]+ ggtitle ("All Patients")
p2<- FL_VAS[[2]]+ ggtitle ("ASIA A")
p3<- FL_VAS[[3]]+ ggtitle ("ASIA B")
p4<- FL_VAS[[4]]+ ggtitle ("ASIA C")


par(mfrow=c(2,2))
p1+p2+p3+p4
