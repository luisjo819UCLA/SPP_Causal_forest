# Load required libraries (tidyverse includes dplyr and ggplot2)

library(grf)
library(haven)
library(tidyverse)
library(GGally)
library(naniar)
library(rdrobust)

# 1. Load Data --------------------------------------------------------------
# Load the saved RDS files
df_combined <- readRDS("df_combined.rds")  %>%
  filter(year == 2015)


adress4 <- "Plots/Data Analysis"

# 2. Modify 2015 Dataset--------------------------------

df_2015 <- df_combined %>%
  select(-icfes_student) %>%
  mutate(family_size = as.numeric(family_size))


#Lets check the % of students above 21 years
df_2015$age_gt21 <- ifelse(df_2015$icfes_age > 20, 1, 0)
#Lets check the % of students above 21 years

#Lets do a table with the percentage of age
age_table <- df_2015 %>%
  group_by(icfes_age) %>%
  summarise(percentage = n() / nrow(df_2015) * 100)  %>% 
  arrange(desc(percentage))
print("Proportion of students by age in 2015:")
print(age_table)
#Lets plot this


print("Number of data rows from elegible 2015:")
print(nrow(df_2015))

# 3. Convert Variables to Numeric (if needed) -------------------------------
# Convert variables as needed for modeling
df_robust <- df_2015 %>%
  mutate(
    beneficiary_spp = as.numeric(as.character(beneficiary_spp)),
    running_saber11 = as.numeric(running_saber11),
    running_sisben = as.numeric(running_sisben)
  )

colSums(is.na(df_robust["running_sisben"]))

df_combined_mod <- df_2015


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
    mw_8,
    # ln_wage_9,
    wage0_8
  )

# Visualize overall missingness:

gg_miss_var(selected_df)

# Lets plot the values (and missings) of father_educ and mother_educ
# Identify top 10 variables with the most missing values
missing_vars <- selected_df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
    names_to = "variable", values_to = "missing_count"
  ) %>%
  arrange(desc(missing_count)) %>%
  slice(1:10)

gg_miss_var(selected_df[c(missing_vars$variable, "year")])

# Lets see the proportion of missing values in father_educ
cat(
  "Proportion of missing values in sisben_area:",
  mean(is.na(selected_df$father_educ)), "\n"
)

print("The top 10 variables with the most missing values:")
print(missing_vars)

# Additional visualizations
library(visdat)

# drop the missings
nrow(df_combined_mod)
df_combined_mod <- df_combined_mod %>%
  drop_na(father_educ)
nrow(df_combined_mod)

df_combined_mod <- df_combined_mod %>%
  drop_na(school_schedule, icfes_works, mother_educ,
          sisben_area, icfes_age, icfes_female)


#Lets save df_combined_mod as rds
saveRDS(df_combined_mod, "df_combined_mod_2015.rds")
