library(bs4Dash)
library(ggplot2)
library(plotly)
library(MASS)
library(tidyverse)
library(dplyr)
library(lubridate)

recData <- read.csv('../../data/compas-scores.csv')

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

ordinalRegression <- function(raceIn, 
                              ageIn, 
                              sexIn, 
                              juvFelCountIn, 
                              juvMisdCountIn, 
                              juvOtherCountIn,
                              priorsCountIn,
                              daysBScreeningArrestIn, 
                              cDaysFromCompasIn,
                              cChargeDegreeIn,
                              cTimeInJailIn) {
    
    ## Clean data ##
    cleanedData <- dataCleaning(recData)
    
    ## Data Processing / Analysis ##
    
    # Create Ordinal Logistic Model
    model_fit <- polr(score_text ~ race + age + sex + juv_fel_count + juv_misd_count + juv_other_count + priors_count + days_b_screening_arrest + c_days_from_compas + c_charge_degree + c_time_in_jail, data = cleanedData, Hess = TRUE)
    
    # Test data against model
    prediction <- data.frame("race"= raceIn, 
                             "age" = ageIn, 
                             "sex"=sexIn, 
                             "juv_fel_count" = juvFelCountIn, 
                             "juv_misd_count" = juvMisdCountIn, 
                             "juv_other_count" = juvOtherCountIn, 
                             "priors_count" = priorsCountIn, 
                             "days_b_screening_arrest" = daysBScreeningArrestIn, 
                             "c_days_from_compas" = cDaysFromCompasIn, 
                             "c_charge_degree" = cChargeDegreeIn, 
                             "c_time_in_jail" = cTimeInJailIn)
    output <- round(predict(model_fit,prediction,type = "p"), 3)
    
    return(output)
    
}

shinyApp(
    ui = dashboardPage(
        title = "INDS 4997 Dashboard",
        fullscreen = TRUE,
        header = dashboardHeader(
            title = dashboardBrand(
                title = "INDS 4997",
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
                    "Ordinal Regression",
                    tabName = "ordreg",
                    icon = icon("chart-bar")
                )
            )
        ),
        body = dashboardBody(
            bs4Card(
                title = "Ordinal Regression Graph",
                maximizable = TRUE,
                width = 12,
                selectizeInput(
                    inputId = "races", 
                    label = "Select a race", 
                    choices = unique(recData$race), 
                    selected = "Caucasian",
                    multiple = FALSE
                ),
                plotlyOutput(outputId = "plot1")
            ),
            bs4Card(
                title = "Ordinal Regression Model",
                maximizable = TRUE,
                width = 12,
                selectizeInput(
                    inputId = "races", 
                    label = "Race", 
                    choices = unique(recData$race), 
                    selected = "Caucasian",
                    multiple = FALSE
                ),
                numericInput("age", "Age", 25, min = 0, max = 100),
                selectizeInput(
                    inputId = "sex", 
                    label = "Sex", 
                    choices = unique(recData$sex), 
                    selected = "Male",
                    multiple = FALSE
                ),
                numericInput("juvFelCount", "Juvinile Felony Count", 0, min = 0, max = 20),
                numericInput("juvMisdCount", "Juvinile Misdemeanor Count", 0, min = 0, max = 20),
                numericInput("juvOtherCount", "Juvinile Other Charges Count", 0, min = 0, max = 20),
                numericInput("priorsCount", "Prior Charges Count", 0, min = 0, max = 20),
                numericInput("daysBScreeningArrest", "Days Before Screening Arrest", 90, min = 0, max = 365),
                numericInput("cDaysFromCompas", "Days From Compas", 30, min = 0, max = 365),
                
                numericInput("cTimeInJail", "Time In Jail", 60, min = 0, max = 10000)
                
            )
        )
    ),
    server = function(input, output, ...) {
        
        currData <- dataCleaning(recData)
        
        output$plot1 <- renderPlotly({
            
            p1 <- plot_ly(currData, x = ~score_text, y = ~age, colors='Reds') %>%
                filter(currData$sex == 'Male') %>%
                filter(race %in% input$races) %>%
                group_by(race) %>%
                add_boxplot(color=~score_text)
            
            p2 <- plot_ly(currData, x = ~score_text, y = ~age, colors='Reds') %>%
                filter(currData$sex == 'Female') %>%
                filter(race %in% input$races) %>%
                group_by(race) %>%
                add_boxplot(color=~score_text)
            
            subplot(p1, p2, shareX = FALSE) %>% 
                layout(xaxis=list(title='Compas Score'), yaxis=list(title='Age'))
                    
        })
    }
)
