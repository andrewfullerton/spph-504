library(tidyverse)
library(haven)

mhacs <- read_sas("data/mhacs_2022_pumf_v2.sas7bdat")
cchs <- read_sav("data/cchs2012_mh.sav")

pooled_df <- full_join(mhacs, cchs, by = intersect(names(mhacs), names(cchs)))

pooled_df <- pooled_df %>%
  filter(DHHGAGE > 4)

pooled_df %>%
  select(CEXDNUM) %>%
  filter(CEXDNUM > 1) %>%
  nrow()

pooled_df_subset <- pooled_df %>%
  select(
    CEXDNUM, MHPFYM, MHPFYA, MHPFYSA, 
    CCC_101, CCC_121, CCC_131, 
    CCC_031, CCC_051, CCC_171A
  )

# Add binary flags
psychiatric <- c("MHPFYM", "MHPFYA")
physical <- c("CCC_101", "CCC_121", "CCC_131", 
              "CCC_031", "CCC_051", "CCC_171A")

pooled_df_flags <- pooled_df_subset %>%
  mutate(
    has_physical = apply(across(all_of(physical)), 1, function(x) any(x == 1)),
    has_psychiatric = apply(across(all_of(psychiatric)), 1, function(x) any(x == 1))
  ) %>%
  mutate(
    has_physical = ifelse(is.na(has_physical), FALSE, has_physical),
    has_psychiatric = ifelse(is.na(has_psychiatric), FALSE, has_psychiatric)
  )

head(pooled_df_flags)

# View class balance
pooled_df_flags %>%
  summarise(
    none = sum(!has_physical & !has_psychiatric),
    physical = sum(has_physical & !has_psychiatric),
    psych = sum(!has_physical & has_psychiatric),
    both = sum(has_physical & has_psychiatric)
  )

# Add CEXDNUM output
pooled_df_flags %>%
  mutate(
    has_physical = ifelse(is.na(has_physical), FALSE, has_physical),
    has_psychiatric = ifelse(is.na(has_psychiatric), FALSE, has_psychiatric)
  ) %>%
  summarise(
    none = sum(!has_physical & !has_psychiatric),  
    physical = sum(has_physical & !has_psychiatric),  
    psych = sum(!has_physical & has_psychiatric),  
    both = sum(has_physical & has_psychiatric),  
    avg_cexdnum_none = mean(CEXDNUM[!has_physical & !has_psychiatric], na.rm = TRUE),  # Average for 'none'
    avg_cexdnum_physical = mean(CEXDNUM[has_physical & !has_psychiatric], na.rm = TRUE),  # Average for 'physical'
    avg_cexdnum_psych = mean(CEXDNUM[!has_physical & has_psychiatric], na.rm = TRUE),  # Average for 'psych'
    avg_cexdnum_both = mean(CEXDNUM[has_physical & has_psychiatric], na.rm = TRUE)  # Average for 'both'
  )
  
