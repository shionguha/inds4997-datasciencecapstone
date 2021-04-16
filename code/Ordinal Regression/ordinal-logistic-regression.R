library(ggplot2)
library(MASS)

setwd('~/GitHub/inds4997-datasciencecapstone')
data <- read.csv('./data/compas-scores-updated-training.csv')

# Order Score Text for graph processing later
data$score_text <- factor(data$score_text, 
                          order = TRUE, 
                          levels = c("Low", "Medium", "High"))

## Data Processing / Analysis ##

# Create Ordinal Logistic Model
model_fit <- polr(score_text ~ race + age + sex + juv_fel_count + juv_misd_count + juv_other_count + priors_count + c_charge_degree + c_charge_violent + c_time_in_jail, data = data, Hess = TRUE)
summary(model_fit)

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

# Create Ordinal Logistic Model without race
raceless_model_fit <- polr(score_text ~ age + sex + juv_fel_count + juv_misd_count + juv_other_count + priors_count + c_charge_degree + c_charge_violent + c_time_in_jail, data = data, Hess = TRUE)
summary(raceless_model_fit)

summary_raceless_table <- coef(summary(raceless_model_fit))
pval <- pnorm(abs(summary_raceless_table[, "t value"]),lower.tail = FALSE)* 2
summary_raceless_table <- cbind(summary_raceless_table, "p value" = round(pval,3))
summary_raceless_table

# Test data against various models #

# Test data based on Caucasian and African-American

c_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "c_charge_degree" = "F", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "African-American", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "c_charge_degree" = "F", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test a Caucasian Felony vs an African-American misdemeanor

c_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "c_charge_degree" = "F", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "African-American", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test a Caucasian with a prior vs African-American with no record

c_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "African-American", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test days between screening and arrest (5 vs 90)
c_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test days between charge and compas screening (5 vs 50)
c_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test days spent in jail (1 month vs 2 years)
c_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 30)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 720)
round(predict(model_fit,aa_data,type = "p"), 3)