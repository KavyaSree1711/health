install.packages("plotly")

library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

df <- read_csv("C:/Users/test/Desktop/Health/Data/public_health_dataset.csv")
df$date <- as.Date(df$date)

summary_kpis <- df %>%
  summarise(
    total_cases = sum(cases),
    total_deaths = sum(deaths),
    total_hospitalizations = sum(hospitalizations)
  )
print(summary_kpis)

#Cases Over Time by Disease
cases_over_time <- df %>%
  group_by(date, disease) %>%
  summarise(cases = sum(cases), .groups = "drop")

fig_cases <- ggplot(cases_over_time, aes(x = date, y = cases, color = disease)) +
  geom_line(size = 1) +
  labs(title = "Cases Over Time", x = "Date", y = "Cases") +
  theme_minimal()

print("Cases Over Time:")
ggplotly(fig_cases)

# Top 10 Locations by Deaths
top_deaths <- df %>%
  group_by(location) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  arrange(desc(deaths)) %>%
  slice_max(deaths, n = 10)

fig_deaths <- ggplot(top_deaths, aes(x = reorder(location, deaths), y = deaths)) +
  geom_col(fill = "salmon") +
  coord_flip() +
  labs(title = "Top 10 Locations by Deaths", x = "Location", y = "Deaths") +
  theme_minimal()

print("Top Locations by Deaths:")
ggplotly(fig_deaths)

# Hospitalizations by Age Group (Pie)
hosp_by_age <- df %>%
  group_by(age_group) %>%
  summarise(hospitalizations = sum(hospitalizations), .groups = "drop")

fig_pie <- plot_ly(hosp_by_age, labels = ~age_group, values = ~hospitalizations, type = 'pie') %>%
  layout(title = "Hospitalizations by Age Group")

print("Hospitalizations by Age Group:")
fig_pie

# ⚥ Gender-Disease Breakdown
gender_cases <- df %>%
  group_by(gender, disease) %>%
  summarise(cases = sum(cases), .groups = "drop")

fig_gender <- ggplot(gender_cases, aes(x = disease, y = cases, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cases by Gender and Disease", x = "Disease", y = "Cases") +
  theme_minimal()

print("Cases by Gender and Disease:")
ggplotly(fig_gender)
