library(ggplot2)
library(MASS)

setwd('~/GitHub/inds4997-datasciencecapstone')
data <- read.csv('./data/compas-scores-updated.csv')

# Store counts of race in new table
race_count <- data %>%
  count(race)

# Order Score Text for graph processing later
data$score_text <- factor(data$score_text, 
                          order = TRUE, 
                          levels = c("Low", "Medium", "High"))

## Data Processing / Analysis ##

# Create plot with the distribution of race, age, and gender
ggplot(data, aes(x = score_text, y = age, fill = score_text)) +
  xlab('Compas Score') + 
  ylab('Age') + labs(fill= 'Compas Score') + 
  geom_boxplot(size = .75) +
  facet_grid(race ~ sex, margins = FALSE) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_brewer(palette = "Reds")

# Create Ordinal Logistic Model
model_fit <- polr(score_text ~ race + age + sex + juv_fel_count + juv_misd_count + juv_other_count + priors_count + days_b_screening_arrest + c_days_from_compas + c_charge_degree + c_charge_violent + c_time_in_jail, data = data, Hess = TRUE)
summary(model_fit)

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

# Create Ordinal Logistic Model without race
raceless_model_fit <- polr(score_text ~ age + sex + juv_fel_count + juv_misd_count + juv_other_count + priors_count + days_b_screening_arrest + c_days_from_compas + c_charge_degree + c_charge_violent + c_time_in_jail, data = data, Hess = TRUE)
summary(raceless_model_fit)

summary_raceless_table <- coef(summary(raceless_model_fit))
pval <- pnorm(abs(summary_raceless_table[, "t value"]),lower.tail = FALSE)* 2
summary_raceless_table <- cbind(summary_raceless_table, "p value" = round(pval,3))
summary_raceless_table

# Test data against various models #

# Test data based on Caucasian and African-American

c_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "F", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "African-American", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "F", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test a Caucasian Felony vs an African-American misdemeanor

c_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "F", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "African-American", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test a Caucasian with a prior vs African-American with no record

c_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "African-American", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test days between screening and arrest (5 vs 90)
c_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "days_b_screening_arrest" = 5, "c_days_from_compas" = 30, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test days between charge and compas screening (5 vs 50)
c_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "days_b_screening_arrest" = 5, "c_days_from_compas" = 5, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "days_b_screening_arrest" = 5, "c_days_from_compas" = 50, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)

# Test days spent in jail (1 month vs 2 years)
c_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "days_b_screening_arrest" = 5, "c_days_from_compas" = 5, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 30)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "Caucasian", "age" = 33, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 1, "days_b_screening_arrest" = 5, "c_days_from_compas" = 5, "c_charge_degree" = "M", "c_charge_violent" = 'V', "c_time_in_jail" = 720)
round(predict(model_fit,aa_data,type = "p"), 3)