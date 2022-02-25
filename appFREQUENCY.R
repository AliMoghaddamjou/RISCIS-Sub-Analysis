#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggpubr)
library(ggplot2)
library(naniar)
library(Epi)
library(fragility)

## Defining Choices
dataChoices <- c("Raw Data", "Regression Imputation (6m)", "Carry Forward")
OutcomeChoices<- c("UEM 6Months", "LEM 6Months", "Total Motor 6Months", "NLI 6Months", "UEM 1year", "LEM 1year", "Total Motor 1year", "NLI 1year", "UEM 84days", "LEM 84days", "Total Motor 84days", "NLI 84days", "ASIA Difference 6M")
ASIAChoices<- c("All", "A", "B", "C")
VariableChoices <- c("AGE", "CCI", "SEX", "RACE", "NLI_N_1")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    
    # Application title
    titlePanel("RISCIS Traditional Analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            ## Selecting the Data
            selectInput("dataType", "Which Data Type:", dataChoices) ,
            ## Selecting ASIA TYPE
            selectInput("ASIAT", "Which ASIA Choice:", ASIAChoices) ,
            ## Selecting Outcome Measure
            selectInput("OutcomeM", "Which Outcome Measure:", OutcomeChoices),
            ## Multivariate Inputs
            checkboxGroupInput("modelVars", "Select other variables to include:", VariableChoices ),
            ## Selecting Binary Cutoff
            numericInput("cutoffInput", "Select Cutoff Value for the Binary Outcome  ", 2),
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
            verbatimTextOutput("warning"),
            verbatimTextOutput("sumTable"),
            verbatimTextOutput("missing"),
            plotOutput("summary"),
            plotOutput("ttestPlot"),
            verbatimTextOutput("ttest"),
            plotOutput("modelGraph"),
            verbatimTextOutput("modelR"),
            plotOutput("BinaryPlot"),
            verbatimTextOutput("BinaryTable"),
            verbatimTextOutput("Fragility")
        )
    )
)





# Server Logic
server <- function(input, output) {

    library(ggplot2)
    library(dplyr)
    library(see)
    library(jtools)


    
    
    ## Loading base data
    base_data <- reactive({
        if(input$dataType =="Raw Data"){
            baseData<-read.csv('alldata.csv') 
        } else if (input$dataType =="Regression Imputation (6m)"){
            baseData<-read.csv('alldataRI.csv') 
        } else if (input$dataType == "Carry Forward") {
          baseData<-read.csv('alldataCF.csv') 
        }
        
    })
    
    ##ACtive Data based on ASIA 
    active_data <- reactive({
        
        baseData <-   base_data()
        dataASIA <- split(baseData, f = baseData$baseASIA)
        if(input$ASIAT == "All"){
            data <-baseData
        } else if ( input$ASIAT == "A"){
            data <-dataASIA[["A"]]
        } else if (input$ASIAT == "B"){
            data <-dataASIA[["B"]]
        } else if ( input$ASIAT == "C"){
            data <-dataASIA[["C"]]
        }
    })
    
    active_outcome <- reactive ({
        data <- active_data ()
        data$ASIA_6[data$ASIA_6 == 9]<- NA
        data$ASIA_1[data$ASIA_1 == 9]<- NA
        ASIADiff6m <- data$ASIA_6 - data$ASIA_1
        if(input$OutcomeM == "UEM 6Months"){
            Outcome <- data$UEMDiff6m
            
        } else if (input$OutcomeM == "LEM 6Months"){
            Outcome <- data$LEMDiff6m
            
        } else if (input$OutcomeM == "Total Motor 6Months"){
            Outcome <- data$TOTMDiff6m
            
        } else if (input$OutcomeM == "NLI 6Months"){
            Outcome <- data$NLIDiff6m
            
        } else if (input$OutcomeM == "UEM 1year"){
            Outcome <- data$UEMDiff12m
            
        } else if (input$OutcomeM == "LEM 1year"){
            Outcome <- data$LEMDiff12m
            
        } else if (input$OutcomeM == "Total Motor 1year"){
            Outcome <- data$TOTMDiff12m
            
        } else if (input$OutcomeM == "NLI 1year"){
            Outcome <- data$NLIDiff12m
            
        } else if (input$OutcomeM == "UEM 84days"){
            Outcome <- data$LEMDiff3m
            
        } else if (input$OutcomeM == "LEM 84days"){
            Outcome <- data$LEMDiff3m
            
        } else if (input$OutcomeM == "Total Motor 84days"){
            Outcome <- data$TOTMDiff3m
            
        } else if (input$OutcomeM == "NLI 84days"){
            Outcome <- data$NLIDiff3m
            
        } else if (input$OutcomeM =="ASIA Difference 6M")
          Outcome <- ASIADiff6m
   
        } 

    )
    
    active_Vars <- reactive({
        modelVars <- input$modelVars
    })
    
    active_cutoff <- reactive({
      CutOff <- input$cutoffInput
    })
    
  ## Warning Msg
    
    output$warning <-renderPrint({
      if (input$dataType =="Regression Imputation (6m)") {
        print("Note for Regression Imputation ONLY the 6 month UEM, TOTM and LEM have imputed data")
      }
    })
  
    ## Summary table
    
    output$sumTable <- renderPrint({
      data <-active_data()
      Outcome <- active_outcome ()  
      Treatment<- data$trtgrp 
      
      table1 <- table( Treatment, Outcome )
      rowSums(table1)

    })
    
    ## Missing Values
    output$missing <- renderPrint({
      data <-active_data()
      Outcome <- active_outcome ()  
      Treatment<- data$trtgrp 
      MissingOutcome <-sum(is.na (Outcome))
      paste("Number of missing values:", MissingOutcome)

    })
    ## Summary PLot
    output$summary <- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        
        OutcomeNA <- na.omit(Outcome)
        dataNA<- data[!is.na(Outcome), ]
        TreatmentNA <- dataNA$trtgrp 
        
        
        dataNA %>%
            ggplot(aes(x = TreatmentNA, y = OutcomeNA, fill = TreatmentNA)) +
            geom_violindot(fill_dots = "black", size_dots = 1) +
            scale_fill_material() +
            ggtitle("Summary Plot")+
            xlab("Treatment") +
            ylab (input$OutcomeM) +
            annotate("text",
                     x = 1:length(table(TreatmentNA)),
                     y = aggregate(OutcomeNA ~ TreatmentNA, dataNA, median)[ , 2],
                     label = table(TreatmentNA),
                     col = "red",
                     vjust = - 20)+
            theme_modern()   
    })

    ##Ttest Plot
    output$ttestPlot<- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()  
        Treatment<- data$trtgrp 
        
        box_plot <- ggplot(data, aes(x = Treatment, y = Outcome))
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
                       vjust = 2, aes(label = paste("Median:", round(..y.., digits = 1))))+

            theme_classic()
    })
## T-Test
    output$ttest <- renderPrint({
        data <-active_data()
        Outcome <- active_outcome ()  
        Treatment<- data$trtgrp 
       ttest<- t.test(Outcome~Treatment, mu = 0, alternative = "greater")
       print(ttest)
    })
## Visualizing Linear Reg
    
    output$modelGraph <-renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$trtgrp 
        model <- reactive({
            if (is.null(input$modelVars) == TRUE){
                glm(Outcome ~ Treatment , data = data)
            } else if (is.null(input$modelVars) == FALSE){
                vars <- as.matrix(data[, input$modelVars])
                glm(Outcome ~ Treatment + vars, data = data) 
            }
        })
      
        par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
        plot(model())
        par(mfrow=c(1,1)) # Change back to 1 x 1
    })
    ##Linear Regresionn
    
    output$modelR <- renderPrint({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$trtgrp 
        model <- reactive({
            if (is.null(input$modelVars) == TRUE){
                glm(Outcome ~ Treatment , data = data)
            } else if (is.null(input$modelVars) == FALSE){
                vars <- as.matrix(data[, input$modelVars])
                glm(Outcome ~ Treatment + vars, data = data) 
            }
        })
        summary(model())
     
    })
    
    ## Binary Mosaic
    output$BinaryPlot <- renderPlot({
      data <-active_data()
      Outcome <- active_outcome ()
      Treatment <- data$trtgrp 
      CutOff<- active_cutoff ()
      
      OutcomeBinary <-ifelse(Outcome<CutOff,"Negative","Positive")
      table1 <- table( Treatment, OutcomeBinary )
      ##PLotting
      mosaicplot(table1, color = c("red", "green"), main = "Mosaic Plot for Binary Outcome",
                 xlab = "Treatment", ylab = "Outcome  ")

    })
    
    ## Two by 2
    output$BinaryTable <- renderPrint({
      
      data <-active_data()
      Outcome <- active_outcome ()
      Treatment <- data$trtgrp 
      CutOff<- active_cutoff ()
      
      OutcomeBinary <-ifelse(Outcome<CutOff,"Negative","Positive")
      table2<-twoby2(Treatment, OutcomeBinary)
      table2
    })
    
    ## Fragility Index
    
    output$Fragility <- renderPrint({
      data <-active_data()
      Outcome <- active_outcome ()
      Treatment <- data$trtgrp 
      CutOff<- active_cutoff ()
      
      OutcomeBinary <-ifelse(Outcome<CutOff,"Negative","Positive")
      table1 <- table( Treatment, OutcomeBinary )
      
      n0 <- table1["Placebo", "Negative"] + table1["Placebo", "Positive"]
      n1 <- table1["Riluzole", "Negative"] + table1["Riluzole", "Positive"]
      
      e0 <- table1["Placebo", "Positive"]
      e1 <- table1["Riluzole", "Positive"]
      
      Frag1<- frag.study(e0, n0, e1, n1)
      Frag1
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
