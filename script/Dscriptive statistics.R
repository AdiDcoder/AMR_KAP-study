library(tidyr)
library(gtsummary)
library(flextable)
library(dplyr)
library(ggplot2)

#read data
data<- readxl::read_excel("clean data/AMR_KAP_Data.xlsx", sheet = 2)

#summary of demographic characteristics
data %>% 
  select(1:11) %>% 
  tbl_summary() %>% 
  as_gt() %>% 
  gtsave("table/table1_demographic.docx")

#Leveling knowledge
data$`Total Knowledge Score` <- cut(data$`Knowledge PCT`,
                            breaks = c(-Inf, 39, 59, Inf),
                            labels = c("Poor", "Moderate", "Good"),
                            right = FALSE)

#Leveling attitude
data$`Total attitude score` <- cut(data$`Attitude PCT`,
                           breaks = c(-Inf, 49, 79, Inf),
                           labels = c("Negative","Uncertain", "Positive"),
                           right = TRUE)

#Leveling practice
data$`Total Practice Score` <- cut(data$`Practice PCT`,
                           breaks = c(-Inf, 79, Inf),
                           labels = c("Misuse", "Good"),
                           right = TRUE)

#Distribution of knowledge level
figdata=read_excel("clean_data/AMR_KAP_Data.xlsx",sheet=1)
colnames(figdata)
fig1=figdata[,12:23]
colnames(fig1)
data_long <- fig1 %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")
data_summary <- data_long %>%
  group_by(Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()
ggplot(data_summary, aes(x = Question, y = percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3) + 
  coord_flip() +
  labs(title = "Antibiotic Knowledge Survey Responses (with Percentages)",
       x = "Question",
       y = "Percentage of Responses") +
  scale_fill_manual(values = c("Yes" = "Dark Cyan", "No" = "White Smoke", "Don't Know" = "Wheat")) +
  theme_minimal()
ggsave("figures/Distribution of knowledge.jpeg", width = 12, height = 10)

#Distribution of attitude
fig2=figdata[,24:33]
colnames(figdata)
data_long1 <- fig2 %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")
data_summary1 <- data_long1 %>%
  group_by(Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()
ggplot(data_summary1, aes(x = Question, y = percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3) + # Add percentages to the bars
  coord_flip() +
  labs(title = "Antibiotic Attitude Survey Responses (with Percentages)",
       x = "Question",
       y = "Percentage of Responses") +
  scale_fill_manual(values = c("Neutral" = "Dark Cyan", "Disagree" = "White Smoke", "Agree" = "Wheat")) +
  theme_minimal()
ggsave("figures//Distribution of attitude.jpeg", width = 12, height = 8)

#Distribution of practice
fig3=figdata[,34:39]
data_long2 <- fig3 %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Response")
data_summary2 <- data_long2 %>%
  group_by(Question, Response) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()
ggplot(data_summary2, aes(x = Question, y = percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3) + # Add percentages to the bars
  coord_flip() +
  labs(title = "Practices among parents of school-going children regarding antibiotic resistance",
       x = "Question",
       y = "Percentage of Responses") +
  scale_fill_manual(values = c("Yes" = "Dark Cyan","No" = "Wheat")) +
  theme_minimal()
ggsave("figures//Distribution of practice.jpeg", width = 12, height = 8)


