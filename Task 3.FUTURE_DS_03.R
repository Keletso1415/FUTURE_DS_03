# Install if needed
install.packages(c("tidyverse", "janitor"))
install.packages("readxl")

# Load libraries
library(tidyverse)
library(janitor)
library(readxl)

getwd()

list.files()

df <- read.csv("bank.csv", sep = ";")

str(df)
head(df)
summary(df)

df$y_binary <- ifelse(df$y == "yes", 1, 0)

conversion_rate <- mean(df$y_binary) * 100
conversion_rate

channel_conversion <- df %>%
  group_by(contact) %>%
  summarise(conversion_rate = mean(y_binary) * 100)

channel_conversion

ggplot(channel_conversion, aes(x = contact, y = conversion_rate)) +
  geom_bar(stat = "identity") +
  labs(title = "Conversion Rate by Contact Channel",
       x = "Channel", y = "Conversion (%)")

total_leads <- nrow(df)
converted <- sum(df$y_binary)

drop_off <- total_leads - converted

total_leads
converted
drop_off

campaign_analysis <- df %>%
  group_by(campaign) %>%
  summarise(conversion_rate = mean(y_binary) * 100)

head(campaign_analysis)

ggplot(campaign_analysis, aes(x = campaign, y = conversion_rate)) +
  geom_line() +
  labs(title = "Conversion Rate by Number of Contacts",
       x = "Number of Contacts", y = "Conversion (%)")

job_conversion <- df %>%
  group_by(job) %>%
  summarise(conversion_rate = mean(y_binary) * 100) %>%
  arrange(desc(conversion_rate))

job_conversion

ggplot(df, aes(x = balance, fill = y)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "Balance Distribution by Conversion")

funnel <- data.frame(
  stage = c("Leads", "Converted"),
  count = c(total_leads, converted)
)

funnel

model <- glm(y_binary ~ age + balance + duration + campaign,
             data = df,
             family = "binomial")

summary(model)

df$predicted_prob <- predict(model, type = "response")

head(df$predicted_prob)

write.csv(df, "clean_bank.csv", row.names = FALSE)
