library(tidyr)
library(gtsummary)
libraray(gt)
library(flextable)
library(dplyr)
#Logistic regression for knowledge
data1 <- data[, c(1:9,46)]  
colnames(data1)
data1$`Total Knowledge Score` <- factor(data$`Total Knowledge Score`, levels = c("Poor", "Moderate", "Good"), ordered = TRUE)

tbl_uvreg_practices1 <- 
  data1 |>
  tbl_uvregression(
    method = polr,  
    y = Total_knowledge_score,  
    exponentiate = TRUE  
  ) |>
  add_global_p() |>
  bold_p(t = 0.10) |>
  bold_labels() |>
  italicize_levels()|>
  as_gt() |>
  gtsave("table/Table2_logistic_regression_knowledge.docx")

#logistic regression for attitude
data2 <- data[, c(1:9,47)] 
data2$Total attitude score <- factor(data2$Total attitude score, levels = c("Negative", "Uncertain", "Positive"), ordered = TRUE)
tbl_uvreg_practices2 <- 
  data2 |>
  tbl_uvregression(
    method = polr,  
    y = attitude_level, 
    exponentiate = TRUE, 
    pvalue_fun = label_style_pvalue(digits = 2),
  ) |>
  add_global_p() |>
  bold_p(t = 0.10) |>
  bold_labels() |>
  italicize_levels()|>
  as_gt() |>
  gtsave("table/Table3_logistic_regression_attitude.docx")  

#logistic regression for practice
data$practice_status <- factor(data$practice_level, 
                               levels = c("POOR", "Good"), 
                               labels = c(0, 1))
data3 <- data[, c(1:9,46,47,49)]
tbl_uvreg_practices3 <- 
  data3 |>
  tbl_uvregression(
    method = glm, 
    y = practice_status,  
    method.args = list(family = binomial(link = "logit")),  
    exponentiate = TRUE  
  ) |>
  as_gt() |>
  gtsave("table/table4_logistic_regression_practice.docx")  


