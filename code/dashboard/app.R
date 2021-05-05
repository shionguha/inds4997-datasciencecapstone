library(bs4Dash)
library(ggplot2)
library(plotly)
library(MASS)
library(tidyverse)
library(dplyr)
library(lubridate)
library(caret)
library(fairness)
library(fairmodels)
library(DALEX)
library(ranger)
library(gbm)
library(jtools)

dataTrain <- read.csv('./data/compas-scores-updated-training.csv')
dataTest <- read.csv('./data/compas-scores-updated-testing.csv')
recData <- read.csv('./data/compas-scores-updated.csv')

outputClassification <- function(number) {
    result <- if (number>0.7) { 
        "Low"
    } else if (number<0.4){
        "High"
    } else {
        "Medium"
    }

    return(result)
}

dataPreparation <- function(data) {
    # Remove unnecessary columns
    data <- select(data, c(race, age, sex, juv_fel_count, juv_misd_count, juv_other_count, priors_count, c_charge_degree, c_charge_violent, c_time_in_jail, score_text))
    
    # Remove N/A's found in score_text
    # Can't us drop_na() as the values are labeled N/A, so had to filter
    data <- data %>% 
        filter(!score_text == "N/A")
    
    # Order Score Text for graph processing later
    data$score_text <- factor(data$score_text, 
                                 order = TRUE, 
                                 levels = c("Low", "Medium", "High"))
    
    # Remove rows without a time for jail
    data <- data[!(is.na(data$c_time_in_jail) | data$c_time_in_jail==""), ]

    # Change time spent in jail's data type to be a number
    data <- transform(data, c_time_in_jail = as.numeric(c_time_in_jail))
    
    data$c_charge_degree <- as.factor(data$c_charge_degree)
    data$c_charge_violent <- as.factor(data$c_charge_violent)
    
    return(data)
}

shinyApp(
    ui = dashboardPage(
        title = "Compas Debiasing",
        fullscreen = TRUE,
        header = dashboardHeader(
            title = dashboardBrand(
                title = "Compas Debiasing",
                color = "primary",
                image = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e8/Marquette_Golden_Eagles_logo.svg/1200px-Marquette_Golden_Eagles_logo.svg.png"
            ),
            skin = "light",
            status = "white",
            border = TRUE,
            sidebarIcon = icon("bars"),
            controlbarIcon = icon("th"),
            fixed = FALSE
        ),
        sidebar = dashboardSidebar(
            skin = "light",
            status = "primary",
            elevation = 3,
            sidebarMenu(
                menuItem(
                    "Home",
                    tabName = "homepage",
                    icon = icon("home")
                ),
                menuItem(
                    "Model Playground",
                    tabName = "modelPlayground",
                    icon = icon("balance-scale")
                )
            )
        ),
        body = dashboardBody(
            tabItems(
                tabItem(tabName = "homepage",
                        jumbotron(
                            title = "Welcome!",
                            lead = "Our model reduces bias by approximately 43% compared to the predictive sentencing algorithm presented by Compas.",
                            "Learn more by reading our paper on it.",
                            btnName = "See the Paper",
                            href="https://www.google.com/",
                            status = "primary"
                        ),
                    fluidRow(
                        infoBox(
                            title = "Total Data Points",
                            value = nrow(currData),
                            icon = icon("hashtag"),
                            color = "success",
                            fill = TRUE,
                            gradient = TRUE
                        ),
                        infoBox(
                            title = "Entries Under Age 30",
                            value = paste(round((sum(currData$age < 30) / nrow(currData)) * 100, 2), "%"),
                            icon = icon("percent"),
                            color = "primary",
                            fill = TRUE,
                            gradient = TRUE
                        ),
                        infoBox(
                            title = "Entries scored Low",
                            value = paste(round((sum(currData$score_text == "Low") / nrow(currData)) * 100, 2), "%"),
                            icon = icon("percent"),
                            color = "warning",
                            fill = TRUE,
                            gradient = TRUE
                        ),
                    ),
                    fluidRow(
                        bs4Card(
                            title = "Dataset Makeup (Race)",
                            maximizable = TRUE,
                            width = 4,
                            plotlyOutput(outputId = "raceGraph")
                        ),
                        bs4Card(
                            title = "Dataset Makeup (Age)",
                            maximizable = TRUE,
                            width = 4,
                            plotlyOutput(outputId = "ageGraph")
                        ),
                        bs4Card(
                            title = "Dataset Makeup (Score)",
                            maximizable = TRUE,
                            width = 4,
                            plotlyOutput(outputId = "scoreGraph")
                        )
                    )
                ),
                tabItem(tabName = "modelPlayground",
                    fluidRow(
                        bs4Card(
                            title = "Model Parameters",
                            maximizable = FALSE,
                            collapsible = FALSE,
                            width = 6,
                            textOutput("instructions"),
                            selectInput(
                                inputId = "mRace", 
                                label = "Race", 
                                choices = c("", unique(recData$race)), 
                                selected = NULL,
                                multiple = FALSE
                            ),
                            numericInput("age", "Age", NULL, min = 0, max = 100),
                            selectInput(
                                inputId = "sex", 
                                label = "Sex", 
                                choices = c("", unique(recData$sex)), 
                                selected = NULL,
                                multiple = FALSE
                            ),
                            numericInput("juvFelCount", "Juvenile Felony Count", NULL, min = 0, max = 20),
                            numericInput("juvMisdCount", "Juvenile Misdemeanor Count", NULL, min = 0, max = 20),
                            numericInput("juvOtherCount", "Juvenile Other Charges Count", NULL, min = 0, max = 20),
                            numericInput("priorsCount", "Prior Charges Count", NULL, min = 0, max = 20),
                            selectInput(
                                inputId = "cChargeDegree", 
                                label = "Charge Degree", 
                                choices = c("", unique(recData$c_charge_degree)), 
                                selected = NULL,
                                multiple = FALSE
                            ),
                            selectInput(
                                inputId="cChargeViolent",
                                label="Charge Violent?",
                                choices = c("", unique(recData$c_charge_violent)),
                                selected = NULL,
                                multiple = FALSE
                            ),
                            numericInput("cTimeInJail", "Time In Jail", NULL, min = 0, max = 10000),
                            actionButton(
                                "runModel", "Run Model", 
                                status = "primary", 
                                outline = TRUE,
                                size = "lg"
                            )
                        ),
                        column(6,
                               fluidRow(
                                    bs4Card(
                                        title = "Model Output",
                                        maximizable = TRUE,
                                        width = 12,
                                        verbatimTextOutput(outputId = "baseModelOutput"),
                                        verbatimTextOutput(outputId = "debiasedModelOutput")
                                    )
                               ),
                               fluidRow(
                                   bs4Card(
                                       title = "Fairness Check",
                                       maximizable = TRUE,
                                       width = 12,
                                       plotOutput("fairnessPlot")
                                   ) 
                               )
                        )
                    )
                )
            )
        )
    ),
    server = function(input, output, ...) {
        
        currData <- dataPreparation(recData)
        trainingData <- dataPreparation(dataTrain)
        testingData <- dataPreparation(dataTest)
        
        output$raceGraph <- renderPlotly({
            plot_ly(currData, x = ~race) %>%
                layout(xaxis=list(title="Race"),
                       yaxis=list(title="Count")) %>%
                add_histogram()
        })
        
        output$ageGraph <- renderPlotly({
            plot_ly(currData, x = ~age) %>%
                layout(xaxis=list(title="Age"),
                       yaxis=list(title="Count")) %>%
                add_histogram()
        })
        
        output$scoreGraph <- renderPlotly({
            plot_ly(currData, x = ~score_text) %>%
                layout(xaxis=list(title="Risk of Recidivism Score"),
                       yaxis=list(title="Count")) %>%
                add_histogram()
        })
        
        
        output$instructions <- renderText({
            "Fill in any attributes you would like to run the model with."
        })
        
        observeEvent(input$runModel, {
            
            predictors <- list()
            
            if(input$mRace!="") {
                predictors['race'] = input$mRace
            }
                
            if(!is.na(input$age)) {
                predictors['age'] = input$age
            }
            
            if(input$sex!="") {
                predictors['sex'] = input$sex
            }
            
            if(!is.na(input$juvFelCount)) {
                predictors['juv_fel_count'] = input$juvFelCount
            }
            
            if(!is.na(input$juvMisdCount)) {
                predictors['juv_misd_count'] = input$juvMisdCount
            }
            
            if(!is.na(input$juvOtherCount)) {
                predictors['juv_other_count'] = input$juvOtherCount
            }
            
            if(!is.na(input$priorsCount)) {
                predictors['priors_count'] = input$priorsCount
            }
            
            if(input$cChargeDegree!="") {
                predictors['c_charge_degree'] = input$cChargeDegree
            }
            
            if(input$cChargeViolent!="") {
                predictors['c_charge_violent'] = input$cChargeViolent
            }
            
            if(!is.na(input$cTimeInJail)) {
                predictors['c_time_in_jail'] = input$cTimeInJail
            }
            
            validate(
                need(length(predictors) != 0, "Please select at least one attribute")
            )
            
            form <- sprintf("%s~%s","score_text",paste0(names(predictors),collapse="+"))
            
            baseOrdRegModel <- polr(as.formula(form), data = currData, Hess = TRUE)
            debiasedOrdRegmodel <- polr(as.formula(form), data = trainingData, Hess = TRUE) #should this use training data or curr?
            
            # Creating gbm model and applying reweighing
            trainingData <- trainingData %>% 
                filter(!score_text == "Medium")
            
            #Low prob of recidivism is the positive outcome so given 1, high risk given 0
            trainingData$score <- ifelse(trainingData$score_text=="Low",1,0)
            
            debiased_predictors <- within(predictors, rm(race, sex))
            debiased_form <- sprintf("%s~%s","score",paste0(names(debiased_predictors),collapse="+"))
            
            #Race is our protected class
            protected <- as.factor(trainingData$race)
            
            # making model
            set.seed(1)
            gbm_model <- gbm(as.formula(debiased_form) , data = trainingData, distribution = "bernoulli")
            
            gbm_explainer <- explain(gbm_model,
                                     data = trainingData,
                                     y = trainingData$score,
                                     label = "original",
                                     colorize = FALSE)
            
            fobject <- fairness_check(gbm_explainer,
                                      protected  = protected,
                                      privileged = "Caucasian",
                                      colorize = FALSE)
            
            weights <- reweight(protected = protected, y = trainingData$score)
            
            gbm_weighted <-gbm(as.formula(debiased_form) , data = trainingData, weights = weights, distribution = "bernoulli")
            
            gbm_explainer_w <- explain(gbm_weighted,
                                       data = trainingData,
                                       y = trainingData$score,
                                       label = "reweighed",
                                       verbose = FALSE)
            
            debiasedPrediction <- as.data.frame(bind_rows(setNames(debiased_predictors, names(debiased_predictors))))
            
            prediction <- predict(gbm_explainer_w, debiasedPrediction)
            
            debiasedScore <- outputClassification(prediction)
            
            fobject <- fairness_check(fobject, gbm_explainer_w, verbose = FALSE)
            
            basePrediction <- as.data.frame(bind_rows(setNames(predictors, names(predictors))))
            
            base_tested_model <- round(predict(baseOrdRegModel,basePrediction,type = "p"), 3)
            
            largest <- 0
            
            for(i in 1:length(base_tested_model)) {
                if (base_tested_model[i] > largest) {
                    largest <- base_tested_model[i]
                    index <- i
                } else {
                    next
                }
            }
            
            base_score_cat <- switch (index,
                "Low",
                "Medium",
                "High"
            )

            output$baseModelOutput <- renderPrint({
                print("Using your parameters, the COMPAS model predicts:")
                print(base_score_cat)
            })
            
            output$debiasedModelOutput <- renderPrint({
                print("Using your parameters, our Debiased model predicts:")
                print(debiasedScore)
            })

            output$fairnessPlot <- renderPlot({
                plot(fobject)
            })
        })
        
    }
)
