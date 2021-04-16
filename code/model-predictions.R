library(ggplot2)
library(MASS)
library(dplyr)
library(lubridate)
library(fairness)


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

#Saving to new CSV
write.csv(dataTest,'./data/compas-scores-predictions.csv', row.names = FALSE)

#####################################################

