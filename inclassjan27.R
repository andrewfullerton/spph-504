library(tidyverse)

data <- read_csv("bccdc_covid_data_2021_09_02.csv")
head(data, n = 15)

# 1
data %>%
  group_by(reported_date, sex) %>%
  summarise(cases = n()) %>%
  filter(sex != "U") %>%
  ggplot(aes(x = reported_date, y = cases)) +
    geom_line() +
    facet_wrap(~factor(sex)) +
    theme_minimal()

# 2 + 3
ggplot(data, aes(x = reported_date, fill = factor(hospitalization))) +
  geom_bar() +
  theme_minimal()

# 4
data_v2 <- data %>%
  arrange(reported_date) %>%
  mutate(
    epi_year = epiyear(reported_date),
    epi_week = epiweek(reported_date)
    ) %>%
  mutate(epi_year_week = paste0(epi_year, " - ", epi_week)) %>%
  mutate(epi_year_week = factor(epi_year_week, levels = unique(.$epi_year_week)))

ggplot(data_v2, aes(x = epi_year, fill = factor(hospitalization))) +
  geom_bar()

ggplot(data_v2, aes(x = epi_week, fill = factor(hospitalization))) +
  geom_bar()

# 5 
ggplot(data_v2, aes(x = epi_year, fill = factor(death))) +
  geom_bar()

ggplot(data_v2, aes(x = epi_week, fill = factor(death))) +
  geom_bar()

# 6
ggplot(data_v2, aes(x = epi_year, fill = factor(hospitalization))) +
  geom_bar() +
  facet_wrap(~factor(HA))

ggplot(data_v2, aes(x = epi_week, fill = factor(hospitalization))) +
  geom_bar() +
  facet_wrap(~factor(HA))

# 7
# ...

# 8
cycle_thru <- data_v2 %>%
  select(epi_week, epi_year) %>%
  names()

for (cycle in cycle_thru) {
  p <- data_v2 %>%
    group_by(.data[[cycle]], sex) %>%
    summarise(cases = n(), .groups = "drop") %>%
    filter(sex != "U") %>%
    ggplot(aes(x = factor(.data[[cycle]]), y = cases)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste("Cases by", cycle))
  
  print(p)  # Explicitly print each plot
}




