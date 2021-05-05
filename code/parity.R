library(ggplot2)
library(MASS)
library(dplyr)
library(lubridate)
library(fairness)

setwd('~/GitHub/inds4997-datasciencecapstone')
data <- read.csv('./data/compas-scores-predictions.csv')


aadata <- data %>% 
  filter(race == "African-American")
cdata <- data %>% 
  filter(race == "Caucasian")
otdata <- data %>% 
  filter(!race == "African-American")

# --Statistical Parity for baseline model--------------------
#Rate at which characterized "High Risk:"
(nrow(otdata[otdata$model =="High", ]) / nrow(otdata)) - (nrow(aadata[aadata$model =="High", ]) / nrow(aadata))
#Rate at which characterized "Low Risk:"
(nrow(otdata[otdata$model =="Low", ]) / nrow(otdata)) - (nrow(aadata[aadata$model =="Low", ]) / nrow(aadata))

# --Statistical Parity for Compas model------------------------------
#Rate at which characterized "High Risk:"
(nrow(otdata[otdata$score_text =="High", ]) / nrow(otdata)) - (nrow(aadata[aadata$score_text =="High", ]) / nrow(aadata))
#Rate at which characterized "Low Risk:"
(nrow(otdata[otdata$score_text =="Low", ]) / nrow(otdata)) - (nrow(aadata[aadata$score_text =="Low", ]) / nrow(aadata))

# GLM Model----------------------------------------- 
# Before Reweighing
# Rate at which characterized "High Risk:"
(nrow(otdata[otdata$gbm =="High", ]) / nrow(otdata)) - (nrow(aadata[aadata$gbm =="High", ]) / nrow(aadata))
#Rate at which characterized "Low Risk:"
(nrow(otdata[otdata$gbm =="Low", ]) / nrow(otdata)) - (nrow(aadata[aadata$gbm =="Low", ]) / nrow(aadata))

#After Reweighing---
#Rate at which characterized "High Risk:"
(nrow(otdata[otdata$weighted =="High", ]) / nrow(otdata)) - (nrow(aadata[aadata$weighted =="High", ]) / nrow(aadata))
#Rate at which characterized "Low Risk:"
(nrow(otdata[otdata$weighted =="Low", ]) / nrow(otdata)) - (nrow(aadata[aadata$weighted =="Low", ]) / nrow(aadata))

#Rate at which characterized "High Risk:"
(nrow(otdata[otdata$roc =="High", ]) / nrow(otdata)) - (nrow(aadata[aadata$roc =="High", ]) / nrow(aadata))
#Rate at which characterized "Low Risk:"
(nrow(otdata[otdata$roc =="Low", ]) / nrow(otdata)) - (nrow(aadata[aadata$roc =="Low", ]) / nrow(aadata))


compHi<- nrow(data[data$score_text =="High" & data$is_recid ==1, ]) / nrow(data[data$score_text =="High", ])
ordHi<- nrow(data[data$model =="High" & data$is_recid ==1, ]) / nrow(data[data$model =="High", ])
gbmHi<- nrow(data[data$gbm =="High" & data$is_recid ==1, ]) / nrow(data[data$gbm =="High", ])
weightHi<- nrow(data[data$weighted =="High" & data$is_recid ==1, ]) / nrow(data[data$weighted =="High", ])

compLo<- nrow(data[data$score_text =="Low" & data$is_recid ==0, ]) / nrow(data[data$score_text =="Low", ])
ordLo<- nrow(data[data$model =="Low" & data$is_recid ==0, ]) / nrow(data[data$model =="Low", ])
gbmLo<- nrow(data[data$gbm =="Low" & data$is_recid ==0, ]) / nrow(data[data$gbm =="Low", ])
weightLo<- nrow(data[data$weighted =="Low" & data$is_recid ==0, ]) / nrow(data[data$weighted =="Low", ])

compMed<- nrow(data[data$score_text =="Medium" & data$is_recid ==1, ]) / nrow(data[data$score_text =="Medium", ])
ordMed<- nrow(data[data$model =="Medium" & data$is_recid ==1, ]) / nrow(data[data$model =="Medium", ])
gbmMed<- nrow(data[data$gbm =="Medium" & data$is_recid ==1, ]) / nrow(data[data$gbm =="Medium", ])
weightMed<- nrow(data[data$weighted =="Medium" & data$is_recid ==1, ]) / nrow(data[data$weighted =="Medium", ])

# Accuracy Measure comparing the percentages of people who score "High" and recidivised and "low" without recidivising for each model 
cat(" Compas High", compHi,"\n","Baseline High", ordHi,"\n","GBM High", gbmHi,"\n","Weighted High", weightHi,"\n\n","Compas Low", compLo,"\n","Baseline Low", ordLo,"\n","GBM Low", gbmLo,"\n","Weighted Low", weightLo,"\n")





# For computing z-test 

nrow(aadata[aadata$model =="High", ])
nrow(aadata)
nrow(aadata[aadata$model =="High", ]) / nrow(aadata)

nrow(cdata[cdata$model =="High", ])
nrow(cdata)
nrow(cdata[cdata$model =="High", ]) / nrow(cdata)



