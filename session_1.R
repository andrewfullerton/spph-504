# Script information ------------------------------------------------------

# Purpose: Run through the various steps of exploring unfamiliar data.

# Load libraries and Data -------------------------------------------------

# If you have never used these packages in the past, un-comment and use the commands below to install them.
# install.packages("dplyr")
# install.packages("readr")

# Load the packages.
library(tidyverse)
covid_data <- read_csv("bccdc_covid_data_2021_09_02.csv")

# Basic data info ---------------------------------------------------------

# Find the number of rows.
nrow(covid_data)

# Find the number of columns.
ncol(covid_data)

# What are the column names?
colnames(covid_data)

# The str() function looks at the overall structure of any object or dataset.
str(covid_data)

# In practice, glimpse() is more commonly used.
# glimpse() gives a clean display of variable classes and a data preview.
glimpse(covid_data)


# Side Note: Variable Types -----------------------------------------------

# There exists 6 basic data structures in R.
# Only 4/6 of these data structures are relevant to everyday work/analysis.

# 1) Logical
class(TRUE)
class(FALSE)

# 2) Integer
class(504L)
# In data or when printed, integers look like numeric variables.
print(504L)

# 3) Numeric, also called Double
class(504)
class(504.2)

# 4) Character, also called String
class("hello world")

# What about factors?
# Technically factors are stored as integers.
# However, R gives factors their own "factors" class and allows us to pre-define variable levels.
# We will dive deeper into the use case of factors in the next few sessions.

# Single variable exploration ---------------------------------------------

# Use the dollar sign to access a column from the data.
# Do not use attach().

# For columns that are categorical, what are the frequencies?
table(covid_data$age_group)
# the extra argument useNA displays any NA values in the data.
table(covid_data$age_group, useNA = "ifany")

# When there are no NA values in the data, it does not matter whether useNA is used.
# However, is the "U" sex value possibly something we want to deal with?
table(covid_data$sex)
table(covid_data$sex, useNA = "ifany")

# Always good practice to keep the useNA argument as you never know what you're dealing with.
table(covid_data$HA, useNA = "ifany")
table(covid_data$classification_reported, useNA = "ifany")
table(covid_data$hospitalization, useNA = "ifany")
table(covid_data$death, useNA = "ifany")

# If you don't care about frequencies and only want to know the unique values.
# unique() also automatically shows NA values if any are present.
unique(covid_data$age_group)

# A quick way to check for NA values
colSums(is.na(covid_data))

# For columns that are dates, what is the range? This works for numeric columns as well.
# If na.rm is not included, and any NA values exist in the column, these functions will return a NA.
# Always good practice to included na.rm.
range(covid_data$reported_date, na.rm = TRUE)
# Most recent date?
max(covid_data$reported_date, na.rm = TRUE)
# Earliest date?
min(covid_data$reported_date, na.rm = TRUE)
# Mean date?
mean(covid_data$reported_date, na.rm = TRUE)
# Median date?
median(covid_data$reported_date, na.rm = TRUE)

# Can we look at the distribution of dates?
# The freq argument sets the y-axis to counts.
#pdf("HistExample.pdf"). ## you can use pdf() to create a pdf version of the figure. Need dev.off() command after the plotting statements
hist(covid_data$reported_date, breaks = "days", freq = TRUE)
#dev.off()

# What about by week?
hist(covid_data$reported_date, breaks = "weeks", freq = TRUE)

# What if you want to look at the probability distribution?
hist(covid_data$reported_date, breaks = "days", freq = FALSE)
# freq = FALSE is actually the default when looking at dates!
hist(covid_data$reported_date, breaks = "days")
# But when working with normal numeric values we get frequencies and not the probability distribution.
hist(covid_data$hospitalization)
# This is why we should always check code documentation.
?hist()

# Multiple variable exploration -------------------------------------------

# What if we want to do a cross tabulation between Health Authority and Death?
table(covid_data$HA, covid_data$death, useNA = "ifany")
# Set captions for the rows and columns here to avoid confusion.
table("Health Authority" = covid_data$HA, 
      "Experienced Death" = covid_data$death, useNA = "ifany")

# Let's now put the table into an object for further use.
# Assign the table argument to a object. Can be named anything, we chose "cross_tab_ha_by_death".
cross_tab_ha_by_death <- table("Health Authority" = covid_data$HA, 
                               "Experienced Death" = covid_data$death, useNA = "ifany")

# addmargins() gives the row, column and total sum of the tabulations.
addmargins(cross_tab_ha_by_death)

# What if we want the proportional table of Health Authority and Death?
# Without any arguments, the sum of all proportions = 1.
prop.table(cross_tab_ha_by_death)

# What if we want row proportions? Here the sum of all row proportions = 1.
prop.table(cross_tab_ha_by_death, 1)
# What if we want column proportions? Here the sum of all column proportions = 1.
prop.table(cross_tab_ha_by_death, 2)

# Again let's assign it an object as it's easier to work with.
prop_table_ha_by_death <- prop.table(cross_tab_ha_by_death, 2)

# We can clean up the proportional table slightly.
# Multiply the entire table by 100, and use the custom round function to get percentages.
prop_table_ha_by_death_cleaned <- round(prop_table_ha_by_death*100, digits = 2)
# What does it look like?
prop_table_ha_by_death_cleaned

# At this point in the script, everything below is not greatly relevant to the course or further learning.
# The code above are what are used the most in day to day data exploration.
# The code below is just to show how we can put these outputs onto an excel document.

# Put this cross tabulation into a dataframe.
data_frame_ha_death <- as.data.frame.matrix(prop_table_ha_by_death_cleaned)

# What does it look like?
View(data_frame_ha_death)

# Need to bring out the rownames as the first column.
# Side note: here we required the function rownames_to_column() from the tibble package.
# Instead of loading the entire package we just wrote tibble:: to access this one function.
# This requires the tibble package to be installed but not loaded.
data_frame_ha_death <- tibble::rownames_to_column(data_frame_ha_death)
# Standardize the column names.
colnames(data_frame_ha_death) <- c("variable", "outcome_1", "outcome_2")
# Add a row that defines this cross tabulation.
data_frame_ha_death <- add_row(data_frame_ha_death, 
                               variable = "Health Authority", 
                               outcome_1 = 0, 
                               outcome_2 = 1, 
                               .before = 1)

# What does it look like now?
View(data_frame_ha_death)
# Great the column names have been added and the column names have been standardized!

# Combining multiple outputs ----------------------------------------------

# What if we want the same cross tabulation for sex and death?
prop_table_sex_by_death_cleaned <- round(
  prop.table(
    table("Sex" = covid_data$sex, 
          "Experienced Death" = covid_data$death, 
          useNA = "ifany"), 
    2) * 100, 
  digits = 2)

# Create a table
data_frame_sex_death <- as.data.frame.matrix(prop_table_sex_by_death_cleaned)
# Bring out row names.
data_frame_sex_death <- tibble::rownames_to_column(data_frame_sex_death)
# Standardize the column names.
colnames(data_frame_sex_death) <- c("variable", "outcome_1", "outcome_2")
# Add a row that defines this cross tabulation.
data_frame_sex_death <- add_row(data_frame_sex_death, 
                                variable = "Sex",
                                outcome_1 = 0, 
                                outcome_2 = 1, 
                                .before = 1)

# Finally combine the two data frames
cross_tab_output <- bind_rows(data_frame_ha_death, data_frame_sex_death)

# Export ------------------------------------------------------------------

write_csv(cross_tab_output, 
          # Specify location and name of output file.
          "~/Dropbox (Personal)/SPPH504/MODULE2/cross_tabs_for_death.csv",
          # Allows the column names to be written in.
          col_names = TRUE)
