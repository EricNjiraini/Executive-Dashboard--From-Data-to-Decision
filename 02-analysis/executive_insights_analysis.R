# 📦 Load Libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# 📂 Load Data
customers <- read_csv("../01-data/customers.csv")      # customer_id, signup_date, age, location
transactions <- read_csv("../01-data/transactions.csv") # transaction_id, customer_id, date, amount

# 🧹 Clean Data
transactions <- transactions %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# 🧮 1. RFM Analysis (Recency, Frequency, Monetary)

# Last date in data (snapshot date)
snapshot_date <- max(transactions$date)

rfm <- transactions %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.numeric(snapshot_date - max(date)),
    frequency = n(),
    monetary = sum(amount)
  ) %>%
  ungroup()

# RFM Summary Table
summary(rfm)

# 📊 Plot: Frequency vs Monetary
ggplot(rfm, aes(x = frequency, y = monetary)) +
  geom_point(alpha = 0.5, color = "#2c3e50") +
  labs(title = "💰 High-Value Customers", x = "Frequency", y = "Monetary Value ($)") +
  theme_minimal()

# 🧪 Segment: High Value Customers
high_value <- rfm %>%
  filter(recency <= 30 & frequency >= 5 & monetary > 500)

write_csv(high_value, "../05-insights-delivery/high_value_customers.csv")

# 📈 2. Cohort Analysis (by Signup Month)

customers <- customers %>%
  mutate(signup_month = floor_date(as.Date(signup_date), "month"))

transactions <- transactions %>%
  left_join(customers, by = "customer_id") %>%
  mutate(order_month = floor_date(date, "month"),
         cohort_month = floor_date(as.Date(signup_date), "month"),
         months_since_signup = interval(cohort_month, order_month) %/% months(1))

# Cohort Counts
cohort_counts <- transactions %>%
  group_by(cohort_month, months_since_signup) %>%
  summarise(users = n_distinct(customer_id), .groups = "drop")

# 📊 Plot: Cohort Retention
ggplot(cohort_counts, aes(x = months_since_signup, y = users, group = cohort_month, color = as.factor(cohort_month))) +
  geom_line() +
  scale_y_continuous(labels = comma) +
  labs(title = "📉 Customer Retention by Cohort", x = "Months Since Signup", y = "Active Users") +
  theme_minimal() +
  theme(legend.position = "none")

# 📥 Save cohort data
write_csv(cohort_counts, "../05-insights-delivery/cohort_retention.csv")

# 🧾 Summary Insights (basic)
summary_insights <- list(
  total_customers = n_distinct(customers$customer_id),
  total_transactions = nrow(transactions),
  high_value_customers = nrow(high_value),
  top_region = customers %>% count(location, sort = TRUE) %>% slice(1)
)

print(summary_insights)
