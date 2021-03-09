library(ggplot2)
library(MASS)
library(tidyverse)

setwd('~/GitHub/inds4997-datasciencecapstone')
data <- read.csv('./data/compas-scores.csv')

## Clean data ##

# Remove unnecessary columns
data <- select(data, c(id, sex, age, age_cat, race, is_recid, decile_score, score_text))

# Remove N/A's found in score_text
# Can't us drop_na() as the values are labeled N/A, so had to filter
data <- data %>% 
  filter(!score_text == "N/A")

# Store counts of race in new table
race_count <- data %>%
  count(race)

data$score_text <- factor(data$score_text, order = TRUE, 
                                   levels = c("Low", "Medium", "High"))
ggplot(data, aes(x = score_text, y = age, fill = score_text)) + xlab('Compas Score') + ylab('Age') + labs(fill= 'Compas Score') + geom_boxplot(size = .75) +   facet_grid(race ~ sex, margins = FALSE) +   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

model_fit <- polr(score_text ~ race + age + sex, data = data, Hess = TRUE)
summary(model_fit)

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

new_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male")
round(predict(model_fit,new_data,type = "p"), 3)