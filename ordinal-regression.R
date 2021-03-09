library(carData)
library(MASS)
library("ggplot2")

summary(compas.scores)

compas.scores$score_text <- factor(compas.scores$score_text, order = TRUE, 
                                    levels = c("Low", "Medium", "High"))
ggplot(compas.scores, aes(x = score_text, y = age, fill = score_text)) + xlab('Compas Score') + ylab('Age') + labs(fill= 'Compas Score') + geom_boxplot(size = .75) +   facet_grid(race ~ sex, margins = FALSE) +   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

model_fit <- polr(score_text ~ race + age + sex, data = compas.scores, Hess = TRUE)
summary(model_fit)

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

new_data <- data.frame("race"= "Caucasian", "age" = 25, "sex"="Male")
round(predict(model_fit,new_data,type = "p"), 3)