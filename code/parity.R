library(ggplot2)
library(MASS)
library(dplyr)
library(lubridate)
library(fairness)

setwd('~/GitHub/inds4997-datasciencecapstone')
data <- read.csv('./data/compas-scores-predictions.csv')


aadata <- data %>% 
  filter(race == "African-American")
otdata <- data %>% 
  filter(!race == "African-American")

#Statistical Parity for logistic regression model
#Rate at which characterized "High Risk:"
(nrow(otdata[otdata$model =="High", ]) / nrow(otdata)) - (nrow(aadata[aadata$model =="High", ]) / nrow(aadata))
#Rate at which characterized "Low Risk:"
(nrow(otdata[otdata$model =="Low", ]) / nrow(otdata)) - (nrow(aadata[aadata$model =="Low", ]) / nrow(aadata))

#Statistical Parity for Compas model
#Rate at which characterized "High Risk:"
(nrow(otdata[otdata$score_text =="High", ]) / nrow(otdata)) - (nrow(aadata[aadata$score_text =="High", ]) / nrow(aadata))
#Rate at which characterized "Low Risk:"
(nrow(otdata[otdata$score_text =="Low", ]) / nrow(otdata)) - (nrow(aadata[aadata$score_text =="Low", ]) / nrow(aadata))




#Everything below here is still a work in progress. This is currently a "proportional parity" test from the fairness
#package, but we can adjust to some other metric comparing true and false positives

#Removing medium outcomes because they won't help compute true/false positives
data <- data %>% 
  filter(!model == "Medium")

#Create binary prediction modpred
data$modpred<- ifelse(data$model=="Low",0,1)
  

# data: data.frame containing the input data and model predictions
# group: column name indicating the sensitive group (factor variable)
# base: base level of the sensitive group for fairness metrics calculation
# outcome: column name indicating the binary outcome variable
# outcome_base: base level of the outcome variable (i.e., negative class) for fairness metrics calculation

prop_parity(data    = data,
            outcome = 'is_recid',
            group   = 'race',
            preds   = 'modpred',
            cutoff  = 0.5,
            base    = 'Caucasian')


#Now repeating process for compas predictions in dataset
data <- read.csv('./data/compas-scores-predictions.csv')


#Removing medium outcomes because they won't help compute true/false positives
data <- data %>% 
  filter(!score_text == "Medium")

#Create binary prediction comppred
data$comppred<- ifelse(data$score_text=="Low",0,1)

prop_parity(data    = data,
            outcome = 'is_recid',
            group   = 'race',
            preds   = 'comppred',
            cutoff  = 0.5,
            base    = 'Caucasian')