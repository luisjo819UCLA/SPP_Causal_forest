# Load required libraries (tidyverse includes dplyr and ggplot2)

library(grf)
library(haven)
library(tidyverse)
library(GGally)
library(naniar)
library(rdrobust)

# 1. Load Data --------------------------------------------------------------
# Load the saved RDS files
df_2013 <- readRDS("df_2013_processed.rds")
df_combined <- readRDS("df_combined.rds")

adress4 <- "Plots/Data Analysis"

# 2. Create 2014 Dataset Based on Eligibility --------------------------------
# For 2014, filter using icfes_per and eligible_saber11
df_2014 <- df_combined %>%
  filter(icfes_per == 20142) %>%
  # filter(eligible_spp == 1) %>%
  select(-icfes_student)

#Lets check the % of students above 21 years
df_2014$age_gt21 <- ifelse(df_2014$icfes_age > 20, 1, 0)

#Lets do a table with the percentage of age
age_table <- df_2014 %>%
  group_by(icfes_age) %>%
  summarise(percentage = n() / nrow(df_2014) * 100)  %>% 
  arrange(desc(percentage))
print("Proportion of students by age in 2014:")
print(age_table)
#Lets plot this


summary(df_2014$running_saber11)
print("Number of data rows from elegible 2014:")
print(nrow(df_2014))

# 3. Convert Variables to Numeric (if needed) -------------------------------
# Convert variables as needed for modeling
df_robust <- df_2014 %>%
  mutate(
    beneficiary_spp = as.numeric(as.character(beneficiary_spp)),
    running_saber11 = as.numeric(running_saber11),
    running_sisben = as.numeric(running_sisben)
  )


# 4. Data Filtering for Counterfactual (2013) and Treated (2014) -------------
# For 2013, filter by icfes_per, z_eligibility, and eligible_sisben
df_2013 <- df_combined %>%
  filter(icfes_per == 20132) %>%
  filter(z_eligible == TRUE) %>%
  filter(eligible_sisben == 1) %>%
  select(-icfes_student)

# 5. Combine Datasets -----------------------------------------------------
# Add a 'year' indicator for each dataset
df_2013 <- df_2013 %>% mutate(year = 2013)
df_2014 <- df_2014 %>% mutate(year = 2014)

print("Number of data rows from elegible 2013:")
print(nrow(df_2013))

# Combine treated (2014) and counterfactual (2013) data
df_combined_mod <- rbind(df_2014, df_2013)


# 7. Drop Missing Values and Visualize---------------------------------------
# Create a subset with selected covariates and outcomes
selected_df <- df_combined_mod %>%
  # filter(year == 2014) %>%
  select(
    # Covariates:
    puntaje_global_s11_final, # Test score
    sisben_score, # SISBEN score (raw)
    icfes_female, # Gender
    icfes_age, # Age
    icfes_stratum, # Socioeconomic stratum
    family_size, # Family size
    icfes_works, # Employment status
    icfes_privatehs, # School type: public/private
    sisben_area, # SISBEN area
    icfes_schoolsch1, # School schedule indicator
    ethnminority, # Ethnic minority
    father_educ, # Father's education
    mother_educ, # Mother's education
    school_schedule, # Combined school schedule (factor)
    school_calendar, # Combined school calendar (factor)
    floor_material, # Combined floor materials (factor)
    family_internet,
    family_laptop,
    family_car,
    # family_cell_phone,
    # student_resides_urban,
    school_urban,
    year,
    age_gt21,
    # Outcomes:
    access_0,
    access_0_hq,
    access_0_hq_pri,
    graduation_exam_pro,
    graduation_exam_pro_hq,
    graduation_exam_pro_hq_pri,
    mw_9,
    # ln_wage_9,
    wage0_9
  )

# Visualize overall missingness:

#gg_miss_var(selected_df)

# Lets plot the values (and missings) of father_educ and mother_educ
# Identify top 10 variables with the most missing values
missing_vars <- selected_df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
    names_to = "variable", values_to = "missing_count"
  ) %>%
  arrange(desc(missing_count)) %>%
  slice(1:10)

gg_miss_var(selected_df[c(missing_vars$variable, "year")], facet = year)



# Lets see the proportion of missing values in father_educ
cat(
  "Proportion of missing values in father_educ:",
  mean(is.na(selected_df$father_educ)), "\n"
)



print("The top 10 variables with the most missing values:")
print(missing_vars)

# NOW BY YEAR
print("Number of rows by year:")
print(table(selected_df$year))
print("Number of missings by year and father_educ:")
print(table(selected_df$year, is.na(selected_df$father_educ)))



# drop the missings
nrow(df_combined_mod)
df_combined_mod <- df_combined_mod %>%
  drop_na(father_educ)
nrow(df_combined_mod)
# gg_miss_var(df_combined_mod[c(missing_vars$variable, "year")], facet = year)
df_combined_mod <- df_combined_mod %>%
  drop_na(icfes_female, family_car, mother_educ, icfes_works)

#Lets save df_combined_mod as rds
saveRDS(df_combined_mod, "df_combined_mod.rds")

#Lets to hist of saber 11 for 2014

p2 <- ggplot(df_combined_mod, aes(x = puntaje_global_s11_final)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Saber 11 Scores (2014)",
    x = "Saber 11 Score",
    y = "Count"
  ) +
  theme_minimal() +
  facet_wrap(~year)
#Same for sisben
p3 <- ggplot(df_combined_mod, aes(x = sisben_score)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of SISBEN Scores (2014)",
    x = "SISBEN Score",
    y = "Count"
  ) +
  theme_minimal() +
  facet_wrap(~year)

address_tot <- "Plots/Causal ML 3/Total Outcomes"
# Save the plots
ggsave(
  filename = file.path(address_tot, "saber11_histogram.png"),
  plot = p2,
  width = 8,
  height = 6
)
ggsave(
  filename = file.path(address_tot, "sisben_histogram.png"),
  plot = p3,
  width = 8,
  height = 6
)

rm(list=ls())