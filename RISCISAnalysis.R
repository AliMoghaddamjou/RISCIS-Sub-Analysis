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

#############################################
###############CODE BLOCK 8##################
#############################################
#####################Linear MIXED EFFECT MODEL


##Defining the Co-variates
Age<- data$AGE
Race <- data$RACE 
NeurologicalLevel <- data$NLI_N_1 
BaselineASIA <- data$baseASIA 
CharlsonComorbidityIndex <-data$CCI
Gender <- data$SEX
Treatment <- data$trtgrp  ##unblinded treatment group 

##Defining Outcomes
OutcomeUEMALL <- data$UEMDiff6m
OutcomeLEMALL <- data$LEMDiff6m
OutcomeTOTMALL <- data$TOTMDiff6m



## Fitting Models 
MixedUEM<- lmer(OutcomeUEMALL ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex + (1|BaselineASIA) +(1|NeurologicalLevel), data = data)
MixedLEM<- lmer(OutcomeLEMALL ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex + (1|BaselineASIA) +(1|NeurologicalLevel), data = data)
MixedTOTM<- lmer(OutcomeTOTMALL ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex + (1|BaselineASIA) +(1|NeurologicalLevel), data = data)

##PLOTTING THE MODELS


mixedPlotAll<- plot_summs(MixedUEM,MixedLEM,MixedTOTM, scale = TRUE, inner_ci_level = .9, 
                          model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"),
                          legend.title = "Outcome Measure")


##CREATING THEME FOR THE GRAPHS
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Helvetica'),
        legend.title=element_blank(), 
        axis.text=element_text(size=25),
        axis.title=element_text(size=15),
        legend.text = element_text(size = 20),
        legend.position = "none")

mixedPlotAll +apatheme

##Summary table 

SumMIXED<- export_summs(MixedUEM,MixedLEM, MixedTOTM,error_format = "[{conf.low}, {conf.high}]",  model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"))
SumMIXED



#############################################
###############CODE BLOCK 9##################
#############################################
################################Linear Models 

#######ALL PATIENTS 

lfitAllUEM <- glm(OutcomeUEMALL ~ Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel+ Treatment, data = data )
lfitAllLEM <- glm(OutcomeLEMALL ~ Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel+ Treatment, data = data )
lfitAllTOTM <- glm(OutcomeTOTMALL ~ Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel+ Treatment, data = data )

plotlfALL<- plot_summs(lfitAllUEM, lfitAllLEM,lfitAllTOTM, scale = TRUE, inner_ci_level = .9,
                       model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"),
                       legend.title = "Outcome Measure")


export_summs(lfitAllUEM,lfitAllLEM, lfitAllTOTM,error_format = "[{conf.low}, {conf.high}]",  model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"))

plot1<- plotlfALL + apatheme
plot1

## ASIA A
Age<- ASIAA$AGE
Race <- ASIAA$RACE 
NeurologicalLevel <- ASIAA$NLI_N_1 
BaselineASIA <- ASIAA$baseASIA 
CharlsonComorbidityIndex <-ASIAA$CCI
Gender <- ASIAA$SEX
Riluzole <-ASIAA$trtgrp  
Treatment <- ASIAA$trtgrp 
OutcomeUEMA <- ASIAA$UEMDiff6m
OutcomeLEMA <- ASIAA$LEMDiff6m
OutcomeTOTMA <- ASIAA$TOTMDiff6m

lfitAllUEM <- glm(OutcomeUEMA ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAA )
lfitAllLEM <- glm(OutcomeLEMA ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAA )
lfitAllTOTM <- glm(OutcomeTOTMA ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAA )

plotlfASIAA<- plot_summs(lfitAllUEM, lfitAllLEM,lfitAllTOTM, scale = TRUE, inner_ci_level = .9,
                         model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"),
                         legend.title = "Outcome Measure")


plot2<- plotlfASIAA + apatheme
plot2

export_summs(lfitAllUEM,lfitAllLEM, lfitAllTOTM,error_format = "[{conf.low}, {conf.high}]",  model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"))


## ASIA B
OutcomeUEMB <- ASIAB$UEMDiff6m
OutcomeLEMB <- ASIAB$LEMDiff6m
OutcomeTOTMB <- ASIAB$TOTMDiff6m
Treatment <- ASIAB$trtgrp 
Age<- ASIAB$AGE
Race <- ASIAB$RACE 
NeurologicalLevel <- ASIAB$NLI_N_1 
BaselineASIA <- ASIAB$baseASIA 
CharlsonComorbidityIndex <-ASIAB$CCI
Gender <- ASIAB$SEX
Riluzole <-ASIAB$trtgrp  

lfitAllUEMB <- glm(OutcomeUEMB ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAB )
lfitAllLEMB <- glm(OutcomeLEMB ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAB )
lfitAllTOTMB <- glm(OutcomeTOTMB ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAB )

plotlfASIAB<- plot_summs(lfitAllUEMB, lfitAllLEMB,lfitAllTOTMB, scale = TRUE,  inner_ci_level = .9,
                         model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"),
                         legend.title = "Outcome Measure")


plot3<- plotlfASIAB +  apatheme
plot3
export_summs(lfitAllUEM,lfitAllLEM, lfitAllTOTM,error_format = "[{conf.low}, {conf.high}]",  model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"))


## ASIA C
OutcomeUEMC <- ASIAC$UEMDiff6m
OutcomeLEMC <- ASIAC$LEMDiff6m
OutcomeTOTMC <- ASIAC$TOTMDiff6m
Treatment <- ASIAC$trtgrp 
Age<- ASIAC$AGE
Race <- ASIAC$RACE 
NeurologicalLevel <- ASIAC$NLI_N_1 
BaselineASIA <- ASIAC$baseASIA 
CharlsonComorbidityIndex <-ASIAC$CCI
Gender <- ASIAC$SEX
Riluzole <-ASIAC$trtgrp  

lfitAllUEMC <- glm(OutcomeUEMC ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAC )
lfitAllLEMC <- glm(OutcomeLEMC ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAC )
lfitAllTOTMC <- glm(OutcomeTOTMC ~ Treatment+Age+Race+Gender+CharlsonComorbidityIndex+NeurologicalLevel, data = ASIAC )

plotlfASIAC<- plot_summs(lfitAllUEMC, lfitAllLEMC,lfitAllTOTMC, scale = TRUE, inner_ci_level = .9, 
                         model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"),
                         legend.title = "Outcome Measure", legend.position="none")

plot4<- plotlfASIAC + apatheme

summ(lfitAllLEMC)

plot4
export_summs(lfitAllUEMC,lfitAllLEMC, lfitAllTOTMC,error_format = "[{conf.low}, {conf.high}]",  model.names = c("Upper Extremity Motor Diff. (6m)", "Lower Extremity Motor Diff. (6m)", "Total  Motor Diff. (6m)"))

#############################################
###############CODE BLOCK 10##################
#############################################
################################MCID/Binary 


##UEM
par(mfrow=c(2,2))
#All
Outcome <- data$UEMDiff6m 
Treatment <- data$trtgrp 
CutOff<- 2
UEM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, UEM_MCID )
UEMALL<- mosaicplot(table1, color = c("red", "green"), main = "ALL Patients",
                    xlab = "Treatment", ylab = "Outcome  ")

##A
Outcome <- ASIAA$UEMDiff6m 
Treatment <- ASIAA$trtgrp 
CutOff<- 2
UEM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, UEM_MCID )
UEMA<- mosaicplot(table1, color = c("red", "green"), main = "ASIA A",
                  xlab = "Treatment", ylab = "Outcome  ")

##B
Outcome <- ASIAB$UEMDiff6m 
Treatment <- ASIAB$trtgrp 
CutOff<- 2
UEM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, UEM_MCID )
UEMB<- mosaicplot(table1, color = c("red", "green"), main = "ASIA B",
                  xlab = "Treatment", ylab = "Outcome  ")
##C
Outcome <- ASIAC$UEMDiff6m 
Treatment <- ASIAC$trtgrp 
CutOff<- 2
UEM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, UEM_MCID )
UEMC<- mosaicplot(table1, color = c("red", "green"), main = "ASIA C",
                  xlab = "Treatment", ylab = "Outcome  ")


##LEM
par(mfrow=c(2,2))
#All
Outcome <- data$LEMDiff6m 
Treatment <- data$trtgrp 
CutOff<- 2
LEM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, LEM_MCID )
LEMALL<- mosaicplot(table1, color = c("red", "green"), main = "ALL Patients",
                    xlab = "Treatment", ylab = "Outcome  ")

##A
Outcome <- ASIAA$LEMDiff6m 
Treatment <- ASIAA$trtgrp 
CutOff<- 2
LEM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, LEM_MCID )
LEMA<- mosaicplot(table1, color = c("red", "green"), main = "ASIA A",
                  xlab = "Treatment", ylab = "Outcome  ")

##B
Outcome <- ASIAB$LEMDiff6m 
Treatment <- ASIAB$trtgrp 
CutOff<- 2
LEM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, LEM_MCID )
LEMB<- mosaicplot(table1, color = c("red", "green"), main = "ASIA B",
                  xlab = "Treatment", ylab = "Outcome  ")
##C
Outcome <- ASIAC$LEMDiff6m 
Treatment <- ASIAC$trtgrp 
CutOff<- 2
LEM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, LEM_MCID )
LEMC<- mosaicplot(table1, color = c("red", "green"), main = "ASIA C",
                  xlab = "Treatment", ylab = "Outcome  ")



##TOTM
par(mfrow=c(2,2))
#All
Outcome <- data$TOTMDiff6m 
Treatment <- data$trtgrp 
CutOff<- 5
TOTM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, TOTM_MCID )
TOTMALL<- mosaicplot(table1, color = c("red", "green"), main = "ALL Patients",
                     xlab = "Treatment", ylab = "Outcome  ")

##A
Outcome <- ASIAA$TOTMDiff6m 
Treatment <- ASIAA$trtgrp 
CutOff<- 5
TOTM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, TOTM_MCID )
TOTMA<- mosaicplot(table1, color = c("red", "green"), main = "ASIA A",
                   xlab = "Treatment", ylab = "Outcome  ")

##B
Outcome <- ASIAB$TOTMDiff6m 
Treatment <- ASIAB$trtgrp 
CutOff<- 5
TOTM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, TOTM_MCID )
TOTMB<- mosaicplot(table1, color = c("red", "green"), main = "ASIA B",
                   xlab = "Treatment", ylab = "Outcome  ")
##C
Outcome <- ASIAC$TOTMDiff6m 
Treatment <- ASIAC$trtgrp 
CutOff<- 5
TOTM_MCID <-ifelse(Outcome<CutOff,"Negative","Positive")
table1 <- table( Treatment, TOTM_MCID )
TOTMC<- mosaicplot(table1, color = c("red", "green"), main = "ASIA C",
                   xlab = "Treatment", ylab = "Outcome  ")


