library(ggplot2)
library(MASS)
library(dplyr)
library(lubridate)
library(fairness)
library(fairmodels)
library(DALEX)
library(ranger)
library(gbm)

setwd('~/GitHub/inds4997-datasciencecapstone')
dataTrain <- read.csv('./data/compas-scores-updated-training.csv')
dataTest <- read.csv('./data/compas-scores-updated-testing.csv')

# Order Score Text for graph processing later
dataTrain$score_text <- factor(dataTrain$score_text, 
                               order = TRUE, 
                               levels = c("Low", "Medium", "High"))

# Order Score Text for graph processing later
dataTest$score_text <- factor(dataTest$score_text, 
                              order = TRUE, 
                              levels = c("Low", "Medium", "High"))

# Create Ordinal Logistic Model
#Removed days_b_screening_arrest & c_days_from_compas because they did not contribute to algorithm and had NA values
model_fit <- polr(score_text ~ race + age + sex + juv_fel_count + juv_misd_count + juv_other_count + priors_count + c_charge_degree + c_charge_violent + c_time_in_jail, data = dataTrain, Hess = TRUE)


#Creating vector that will hold model predictions for each record in dataset
n<- nrow(dataTest)
temp=c()
i=1
while(i<n+1){
  temp[i]<- if (predict(model_fit,dataTest[i,] , type = "p")[1] > predict(model_fit,dataTest[i,] ,type = "p")[2] & predict(model_fit,dataTest[i,] ,type = "p")[1] > predict(model_fit,dataTest[i,] ,type = "p")[3]) { 
    "Low"
  } else if (predict(model_fit,dataTest[i,] ,type = "p")[2] > predict(model_fit,dataTest[i,] ,type = "p")[1] & predict(model_fit,dataTest[i,] ,type = "p")[2] > predict(model_fit,dataTest[i,] ,type = "p")[3]) {
    "Medium"
  } else {
    "High"
  }
  i=i+1
}

#Appending predictions to df
dataTest$model<-temp


#####################################################

# Creating gbm model and applying reweighing

dataTrain <- read.csv('./data/compas-scores-updated-training.csv')
dataTrain <- dataTrain %>% 
  filter(!score_text == "Medium")

#Low prob of recidivism is the positive outcome so given 1, high risk given 0
dataTrain$score <- ifelse(dataTrain$score_text=="Low",1,0)

#Race is our protected class
protected     <- as.factor(dataTrain$race)

dataTrain$c_charge_degree   <- as.factor(dataTrain$c_charge_degree)
dataTrain$c_charge_violent   <- as.factor(dataTrain$c_charge_violent)

# making model
set.seed(1)
gbm_model <-gbm(score ~ age + juv_fel_count + juv_misd_count + juv_other_count + priors_count + c_charge_degree + c_charge_violent + c_time_in_jail , data = dataTrain, distribution = "bernoulli")

# making explainer
gbm_explainer <- explain(gbm_model,
                         data = dataTrain,
                         y = dataTrain$score,
                         label = "original",
                         colorize = FALSE)
model_performance(gbm_explainer)

fobject <- fairness_check(gbm_explainer,
                          protected  = protected,
                          privileged = "Caucasian",
                          colorize = FALSE)

weights <- reweight(protected = protected, y = dataTrain$score)

set.seed(1)

gbm_weighted <-gbm(score ~ age + juv_fel_count + juv_misd_count + juv_other_count + priors_count + c_charge_degree + c_charge_violent + c_time_in_jail , data = dataTrain, weights = weights, distribution = "bernoulli")


gbm_explainer_w <- explain(gbm_weighted,
                           data = dataTrain,
                           y = dataTrain$score,
                           label = "reweighed",
                           verbose = FALSE)

fobject <- fairness_check(fobject, gbm_explainer_w, verbose = FALSE)
plot(fobject)


dataTest$c_charge_degree   <- as.factor(dataTest$c_charge_degree)
dataTest$c_charge_violent   <- as.factor(dataTest$c_charge_violent)

#Creating vector that will hold model predictions for each record in dataset
n<- nrow(dataTest)
out=c()
out2=c()
temp=c()
temp2=c()
i=1
while(i<n+1){
  out[i]<- predict(gbm_explainer,dataTest[i,])
  temp[i]<- if (predict(gbm_explainer,dataTest[i,])>0.7) { 
    "Low"
  } else if (predict(gbm_explainer,dataTest[i,])<0.4){
    "High"
  } else {
    "Medium"
  }
  out2[i]<- predict(gbm_explainer_w,dataTest[i,])
  temp2[i]<- if (predict(gbm_explainer_w,dataTest[i,])>0.7) { 
    "Low"
  } else if (predict(gbm_explainer_w,dataTest[i,])<0.4){
    "High"
  } else {
    "Medium"
  }
  i=i+1
}

#Appending predictions to df
dataTest$gbmRaw<-out
dataTest$weightedRaw<-out2
dataTest$gbm<-temp
dataTest$weighted<-temp2

#Saving to new CSV
write.csv(dataTest,'./data/compas-scores-predictions.csv', row.names = FALSE)

