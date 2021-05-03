library(bs4Dash)
library(ggplot2)
library(plotly)
library(MASS)
library(tidyverse)
library(dplyr)
library(lubridate)
library(caret)

recData <- read.csv('./data/compas-scores-updated.csv')

dataCleaning <- function(data) {
    # Remove unnecessary columns
    data <- select(data, c(id, sex, age, age_cat, race, juv_fel_count, juv_misd_count, juv_other_count, priors_count, days_b_screening_arrest, c_days_from_compas, c_jail_in, c_jail_out, c_charge_degree, c_charge_desc, is_recid, decile_score, score_text))
    
    # Remove N/A's found in score_text
    # Can't us drop_na() as the values are labeled N/A, so had to filter
    data <- data %>% 
        filter(!score_text == "N/A")
    
    # Order Score Text for graph processing later
    data$score_text <- factor(data$score_text, 
                                 order = TRUE, 
                                 levels = c("Low", "Medium", "High"))
    
    # Store counts of race in new table
    race_count <- data %>%
        count(race)
    
    # Convert into datetime type
    data$c_jail_in <- ymd_hms(data$c_jail_in)
    data$c_jail_out <- ymd_hms(data$c_jail_out)
    
    # Add column that represents how long crime had person in jail in days
    data <- data %>% rowwise() %>%
        mutate(c_time_in_jail = difftime(c_jail_out, c_jail_in, units = "days"))
    
    # Remove rows without a time for jail
    data <- data[!(is.na(data$c_time_in_jail) | data$c_time_in_jail==""), ]
    
    # Change time spent in jail's data type to be a number
    data <- transform(data, c_time_in_jail = as.numeric(c_time_in_jail))
    
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
                    tabName = "orgregmodel",
                    icon = icon("layer-group")
                )
            )
        ),
        body = dashboardBody(
            tabItems(
                tabItem(tabName = "homepage",
                        jumbotron(
                            title = "Welcome!",
                            lead = "Our model reduces bias by 30% compared to the predictive sentencing algorithm presented by Compas.",
                            status = "primary"
                        ),
                    fluidRow(
                        bs4Card(
                            title = "Ordinal Regression Graph",
                            maximizable = TRUE,
                            width = 6,
                            selectizeInput(
                                inputId = "race1", 
                                label = "Select a race", 
                                choices = unique(recData$race), 
                                selected = "Caucasian",
                                multiple = FALSE
                            ),
                            plotlyOutput(outputId = "plot1")
                        ),
                        bs4Card(
                            title = "Ordinal Regression Graph",
                            maximizable = TRUE,
                            width = 6,
                            selectizeInput(
                                inputId = "race2", 
                                label = "Select a race", 
                                choices = unique(recData$race), 
                                selected = "Asian",
                                multiple = FALSE
                            ),
                            plotlyOutput(outputId = "plot2")
                        )
                    )
                ),
                tabItem(tabName = "selectmodel",
                        fluidRow(
                            bs4Card(
                                title = "Selectable Attributes",
                                maximizable = FALSE,
                                collapsible = FALSE,
                                width = 3,
                                checkboxGroupInput("attr", "Predictors", 
                                                   choiceNames = 
                                                       c("Race",
                                                        "Age",
                                                        "Sex",
                                                        "Juvenile Felony Count",
                                                        "Juvenile Misdemeanor Count",
                                                        "Juvenile Other Charges Count",
                                                        "Prior Charges Count",
                                                        "Days Before Screening Arrest",
                                                        "Days From Compas",
                                                        "Charge Degree",
                                                        "Time in Jail"), 
                                                   choiceValues = 
                                                       c("race",
                                                         "age",
                                                         "sex",
                                                         "juv_fel_count",
                                                         "juv_misd_count",
                                                         "juv_other_count",
                                                         "priors_count",
                                                         "days_b_screening_arrest",
                                                         "c_days_from_compas",
                                                         "c_charge_degree",
                                                         "c_time_in_jail")
                                                   ),
                                
                                actionButton(
                                    "runCustomModel", "Run Model", 
                                    status = "primary", 
                                    outline = TRUE,
                                    size = "lg"
                                )
                            ),
                            bs4Card(
                                title = "Output",
                                maximizable = TRUE,
                                width = 9,
                                verbatimTextOutput(outputId = "customModelResult")
                            )
                        )
                ),
                tabItem(tabName = "ordregmodel",
                    fluidRow(
                    bs4Card(
                        title = "Ordinal Regression Model",
                        maximizable = FALSE,
                        collapsible = FALSE,
                        width = 6,
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
                        numericInput("cTimeInJail", "Time In Jail", NULL, min = 0, max = 10000),
                        actionButton(
                            "runModel", "Run Model", 
                            status = "primary", 
                            outline = TRUE,
                            size = "lg"
                        )
                    ),
                    bs4Card(
                        title = "Model Output",
                        maximizable = TRUE,
                        width = 6,
                        verbatimTextOutput(outputId = "inputtedModelResult")
                    )
                    )
                )
            )
        )
    ),
    server = function(input, output, ...) {
        
        currData <- dataCleaning(recData)
        
        output$plot1 <- renderPlotly({
            plot_ly(currData, x = ~score_text, y = ~age, colors='Dark2') %>%
                filter(race %in% input$race1) %>%
                group_by(race) %>%
                layout(title = "Ordinal Regression based on Race", 
                       xaxis=list(title="Risk Categorization"),
                       yaxis=list(title="Age")) %>%
                add_boxplot(color=~score_text, 
                            line = list(color = '#000000'),
                            marker = list(color = '#000000'))
                    
        })
        
        observeEvent(input$runCustomModel, {
            
            validate(
                need(input$attr != 0, "Please select at least one attribute")
            )
            
            form <- sprintf("%s~%s","score_text",paste0(input$attr,collapse="+"))
            ordReg <- polr(as.formula(form), data = currData, Hess = TRUE)
            output$customModelResult <- renderPrint({
                
                print(form)
                summary(ordReg)
                
            })
            
        })
        
        output$plot2 <- renderPlotly({
            plot_ly(currData, x = ~score_text, y = ~age, colors='Dark2') %>%
                filter(race %in% input$race2) %>%
                group_by(race) %>%
                layout(title = "Ordinal Regression based on Race", 
                       xaxis=list(title="Risk Categorization"),
                       yaxis=list(title="Age")) %>%
                add_boxplot(color=~score_text,
                            line = list(color = '#000000'),
                            marker = list(color = '#000000'))
            
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
            
            if(!is.na(input$cTimeInJail)) {
                predictors['c_time_in_jail'] = input$cTimeInJail
            }
            
            validate(
                need(length(predictors) != 0, "Please select at least one attribute")
            )
            
            form <- sprintf("%s~%s","score_text",paste0(names(predictors),collapse="+"))
            ordReg <- polr(as.formula(form), data = currData, Hess = TRUE)
            
            prediction <- as.data.frame(bind_rows(setNames(predictors, names(predictors))))
            
            tested_model <- round(predict(ordReg,prediction,type = "p"), 3)
            
            output$inputtedModelResult <- renderPrint({
                print("Using your parameters:")
                print(tested_model)
            })
        })
        
    }
)
