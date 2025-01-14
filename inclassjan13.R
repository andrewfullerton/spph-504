library(tidyverse)
covid_data <- read_csv("bccdc_covid_data_2021_09_02.csv")

view(covid_data)

# 1) Compare between health authorities
covid_data %>% 
  group_by(HA) %>%
  summarise(
    cases = n()
    )
  
# 2) In each HA which age group comprised the highest number of cases?
covid_data %>%
  group_by(HA, age_group) %>%
  summarise(cases = n(), .groups = "drop") %>%
  group_by(HA) %>%
  slice_max(cases, n = 1)

# 3) Which columns contain missing values?
covid_data %>%
  summarise(across(everything(), ~ any(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "has_missing") %>%
  filter(has_missing) %>%
  pull(column)

# 4) Distribution of sex in different health authorities
covid_data %>%
  group_by(HA) %>%
  summarise(
    prop_men = sum(sex == "M")/n(),
    prop_wom = sum(sex == "F")/n(),
    prop_uni = sum(sex == "U")/n()
  )

# 5) Which year/month combination has the highest count of covid cases?
month_year_counts <- covid_data %>%
  mutate(year_month = format(reported_date, "%Y-%m")) %>%
  count(year_month) %>%
  arrange(desc(n)) 

print(month_year_counts)
# 2021-04 is the highest count

# 6) Hospitalization is more common in which age group? 
covid_data %>% group_by(age_group) %>%
  summarise(
    hospitalizations = sum(hospitalization),
    hospitalization_prop = hospitalizations/n(),
    n = n()
    )

# 7) Death is more common in which age group? 
covid_data %>% group_by(age_group) %>%
  summarise(
    deaths = sum(death),
    deaths_prop = deaths/n(),
    n = n()
  )

# 8) What proportion of COVID deaths were not hospitalizations?
# ...

# BONUS QUESTION
covid_data %>%
  group_by(HA, age_group) %>%
  summarise(
    deaths = sum(death == "1"), .groups = "drop") %>%
  group_by(HA) %>%
  slice_max(deaths, n = 1)





