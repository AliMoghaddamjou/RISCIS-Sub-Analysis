#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



## Defining Choices
dataChoices <- c("Raw Data", "Regression Imputation (6m)", "Carry Forward")
OutcomeChoices<- c("UEM 6Months", "LEM 6Months", "Total Motor 6Months", "NLI 6Months", "UEM 1year", "LEM 1year", "Total Motor 1year", "NLI 1year", "UEM 84days", "LEM 84days", "Total Motor 84days", "NLI 84days" )
ASIAChoices<- c("All", "A", "B", "C")
VariableChoices <- c("AGE", "CCI", "SEX", "RACE", "NLI_N_1")

# Define UI for application that draws a histogram
ui <- fluidPage(

  

    # Application title
    titlePanel("RISCIS Baysean Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            ## Selecting the Data
             selectInput("dataType", "Which Data Type:", dataChoices) ,
             ## Selecting ASIA TYPE
             selectInput("ASIAT", "Which ASIA Choice:", ASIAChoices) ,
            ## Selecting Outcome Measure
            selectInput("OutcomeM", "Which Outcome Measure:", OutcomeChoices),
            ## Selecting ROPE MIN
            numericInput("ropeMin", "Minimum of Region of Practical Equivalence ", -2),
            ## Selecting ROPE MAX
            numericInput("ropeMax", "Maximum of Region of Practical Equivalence ", 2),
            ## Multivariate Inputs
            checkboxGroupInput("modelVars", "Select other variables to include:", VariableChoices )
         
        ),
        
  

        # Show a plot of the generated distribution
        mainPanel(
          verbatimTextOutput("warning"),
          verbatimTextOutput("sumTable"),
          verbatimTextOutput("missing"),
            plotOutput("summary"),
            plotOutput("posterior"),
            tableOutput("bayStats"),
            verbatimTextOutput("baysCor"),
            plotOutput("baysCorGraphs"),
            plotOutput("ropeGraph"),
            tableOutput("ropeTable"),
            plotOutput("pdGraph"),
            tableOutput("pdTable"),
            tableOutput("diagnostic")
     
  
        )
    )
)





# Server Logic
server <- function(input, output) {
    library(rstanarm)
    library(bayestestR)
    library(insight)
    library(ggplot2)
    library(logspline)
    library(BayesFactor)
    library(dplyr)
    library(see)
    library(modelbased)
    library(performance)
    
    
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
            
        } 

    })
    active_ropeMin <-reactive ({
        minRope <- input$ropeMin
    })
    active_ropeMax <-reactive ({
        maxRope <- input$ropeMax
    })
    active_Vars <- reactive({
      modelVars <- input$modelVars
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

 
## Posterior Distribution Plot
    output$posterior <- renderPlot({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$trtgrp 
        model <- reactive({
          if (is.null(input$modelVars) == TRUE){
            stan_glm(Outcome ~ Treatment , data = data)
          } else if (is.null(input$modelVars) == FALSE){
            vars <- as.matrix(data[, input$modelVars])
            stan_glm(Outcome ~ Treatment + vars, data = data) 
          }
        })
        
        
        ##Extracting Posterior
        
        posteriors <- insight::get_parameters(model())
        
        ## Graph with mean median and mode
        ggplot(posteriors, aes(x = TreatmentRiluzole)) +
            ggtitle("Posterior Plot")+
            geom_density(fill = "orange") +
            # The mean in blue
            geom_vline(xintercept = mean(posteriors$TreatmentRiluzole), color = "blue", size = 1) +
            # The median in red
            geom_vline(xintercept = median(posteriors$TreatmentRiluzole), color = "red", size = 1) +
            # The MAP in purple
            geom_vline(xintercept = map_estimate(posteriors$TreatmentRiluzole), color = "purple", size = 1)
    })

## Important Baysean Statistics
    output$bayStats <- renderTable({
        data <-active_data()
        Outcome <- active_outcome ()
        Treatment <- data$trtgrp 
        model <- reactive({
          if (is.null(input$modelVars) == TRUE){
            stan_glm(Outcome ~ Treatment , data = data)
          } else if (is.null(input$modelVars) == FALSE){
            vars <- as.matrix(data[, input$modelVars])
            stan_glm(Outcome ~ Treatment + vars, data = data) 
          }
        })
        posteriors <- insight::get_parameters(model())
        
        describe_posterior(model(), test = c("p_direction", "rope", "bayesfactor"))
    })
    
## Baysean Correlation
     output$baysCor <- renderPrint({
         data <-active_data()
         Outcome <- active_outcome ()
         Treatment <- data$trtgrp 
         Outcome<- as.numeric(Outcome)
         TreatmentN <- ifelse(Treatment == "Placebo", 0, 1)
         result <- correlationBF(Outcome, TreatmentN)
         describe_posterior(result)
         bayesfactor_models(result)
         
     })
## Visualizing Baysean Correlation 
     output$baysCorGraphs<- renderPlot({
         data <-active_data()
         Outcome <- active_outcome ()
         Treatment <- data$trtgrp 
         Outcome<- as.numeric(Outcome)
         TreatmentN <- ifelse(Treatment == "Placebo", 0, 1)
         result <- correlationBF(Outcome, TreatmentN)
         plot(bayesfactor_models(result)) +
             ggtitle("Bayes Factor")+
             scale_fill_pizza()
     })
     
## CUstom ROPE
     output$ropeGraph <-renderPlot({
         data <-active_data()
         Outcome <- active_outcome ()
         Treatment <- data$trtgrp 
         maxRope <-  active_ropeMax ()
         minRope <- active_ropeMin ()
         model <- reactive({
           if (is.null(input$modelVars) == TRUE){
             stan_glm(Outcome ~ Treatment , data = data)
           } else if (is.null(input$modelVars) == FALSE){
             vars <- as.matrix(data[, input$modelVars])
             stan_glm(Outcome ~ Treatment + vars, data = data) 
           }
         })
         posteriors <- insight::get_parameters(model())       
         percentage_in_rope <- rope(posteriors$TreatmentRiluzole, range = c(minRope, maxRope), ci = 0.89)
         plot(percentage_in_rope) 
     })
     
## Table Custom ROPE
     output$ropeTable <- renderTable({
         data <-active_data()
         Outcome <- active_outcome ()
         Treatment <- data$trtgrp 
         maxRope <-  active_ropeMax ()
         minRope <- active_ropeMin ()
         model <- reactive({
           if (is.null(input$modelVars) == TRUE){
             stan_glm(Outcome ~ Treatment , data = data)
           } else if (is.null(input$modelVars) == FALSE){
             vars <- as.matrix(data[, input$modelVars])
             stan_glm(Outcome ~ Treatment + vars, data = data) 
           }
         })
         posteriors <- insight::get_parameters(model())       
         percentage_in_rope <- rope(posteriors$TreatmentRiluzole, range = c(minRope, maxRope), ci = 0.89)
         percentage_in_rope
         
     })
## Probability Direction
     output$pdGraph <- renderPlot ({
         data <-active_data()
         Outcome <- active_outcome ()
         Treatment <- data$trtgrp 
         model <- reactive({
           if (is.null(input$modelVars) == TRUE){
             stan_glm(Outcome ~ Treatment , data = data)
           } else if (is.null(input$modelVars) == FALSE){
             vars <- as.matrix(data[, input$modelVars])
             stan_glm(Outcome ~ Treatment + vars, data = data) 
           }
         })
         posteriors <- insight::get_parameters(model())       
         pd <- p_direction(model())
         plot(pd) 
     })
## Table PD
     output$pdTable <- renderTable ({
         data <-active_data()
         Outcome <- active_outcome ()
         Treatment <- data$trtgrp 
         model <- reactive({
           if (is.null(input$modelVars) == TRUE){
             stan_glm(Outcome ~ Treatment , data = data)
           } else if (is.null(input$modelVars) == FALSE){
             vars <- as.matrix(data[, input$modelVars])
             stan_glm(Outcome ~ Treatment + vars, data = data) 
           }
         })
    
         pd <- p_direction(model())
         pd
     })
## Model Diagnostics
     output$diagnostic<- renderTable({
         data <-active_data()
         Outcome <- active_outcome ()
         Treatment <- data$trtgrp 
         model <- reactive({
           if (is.null(input$modelVars) == TRUE){
             stan_glm(Outcome ~ Treatment , data = data)
           } else if (is.null(input$modelVars) == FALSE){
             vars <- as.matrix(data[, input$modelVars])
             stan_glm(Outcome ~ Treatment + vars, data = data) 
           }
         })
         model_performance(model())
         
     })
}


# Run the application 
shinyApp(ui = ui, server = server)
