library(ggplot2)
library(MASS)
library(tidyverse)
library(dplyr)
library(lubridate)

setwd('~/GitHub/inds4997-datasciencecapstone')
data <- read.csv('./data/compas-scores.csv')

## Clean data ##

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
          
## Data Processing / Analysis ##

# Create plot of data
ggplot(data, aes(x = score_text, y = age, fill = score_text)) +
  xlab('Compas Score') + 
  ylab('Age') + labs(fill= 'Compas Score') + 
  geom_boxplot(size = .75) +
  facet_grid(race ~ sex, margins = FALSE) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_brewer(palette = "Reds")

# Create Ordinal Logistic Model
model_fit <- polr(score_text ~ race + age + sex + juv_fel_count + juv_misd_count + juv_other_count + priors_count + days_b_screening_arrest + c_days_from_compas + c_charge_degree + c_time_in_jail, data = data, Hess = TRUE)
summary(model_fit)

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

# Test data against model
c_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "F", "c_time_in_jail" = 60)
round(predict(model_fit,c_data,type = "p"), 3)

aa_data <- data.frame("race"= "African-American", "age" = 25, "sex"="Male", "juv_fel_count" = 0, "juv_misd_count" = 0, "juv_other_count" = 0, "priors_count" = 0, "days_b_screening_arrest" = 90, "c_days_from_compas" = 30, "c_charge_degree" = "F", "c_time_in_jail" = 60)
round(predict(model_fit,aa_data,type = "p"), 3)