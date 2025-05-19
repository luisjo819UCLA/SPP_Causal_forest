library(dplyr)
library(rdrobust)
library(ggplot2)
library(haven)
library(tidyverse)
library(GGally)
# Este script unifica los datos de 2014 y 2015,
# los combina con la base de valor agregado,
# y filtra el estimation sample final usado en los modelos de tratamiento.

# 1. Load Data --------------------------------------------------------------
# Load the value-added dataset (va_prog) from Stata.
va_prog <- read_dta("Code/va_prog.dta")

# Read the filtered data for 2013 and 2014
# (without missings for saber11 and sisben scores)
data2 <- read_rds("Code/filtered_data.rds") %>%
  rename(codigo_prog = codigo_programa)
#Loading 2015 year data
data2015 <- read_rds("Code/Data_2015.rds") %>%
  rename(codigo_prog = codigo_programa)

#Lets combine data2 and data2015
data2 <- bind_rows(data2, data2015)

# Merge the main dataset (data2) with the va_prog data on the key variable.
# This performs a many-to-one merge: each 'codigo_prog'
# in df will join with its corresponding row in va_prog.
df2 <- merge(data2, va_prog, by = "codigo_prog", all.x = TRUE)
rm(data2, va_prog)
# Exclude individuals with m == 1 (already in higher
# education at the time of Saber 11)
# Also, exclude individuals with missing values for sisben_score and
# puntaje_global_s11_final
# (the latter is the final score for Saber 11)
df <- df2 %>%
  filter(m != 1) %>%
  filter(!is.na(sisben_score)) %>%
  filter(!is.na(puntaje_global_s11_final))
rm(df2)
#Find the column names that finish on c_va

# Identify all columns ending with "_c_va"  except "mw_9_c_va"
cols_to_drop <- setdiff(grep("_c_va$", names(df), value = TRUE), "mw_9_c_va")

# Drop columns that would need merging
# e.g., variables ending with "_c_va" or merge keys, except for mw_9_c_va
df <- df %>%
  select(-all_of(cols_to_drop), -codigo_prog) 

# Extract and save variable labels for reference
var_labels <- sapply(df, function(x) attr(x, "label"))
label_lines <- paste(names(var_labels), ":", var_labels)
writeLines(label_lines, "var_labels.txt")
### 2. Process Parental Education Variables -------------------------------
# For father_educ: use the binary dummies for primary,
# secondary, technical, university
df <- df %>%
  mutate(
    father_educ = case_when(
      icfes_educp1 == 1 ~ "primary",
      icfes_educp2 == 1 ~ "secondary",
      icfes_educp3 == 1 ~ "technical",
      icfes_educp4 == 1 ~ "university",
      TRUE ~ NA_character_
    ),
    mother_educ = case_when(
      icfes_educm1 == 1 ~ "primary",
      icfes_educm2 == 1 ~ "secondary",
      icfes_educm3 == 1 ~ "technical",
      icfes_educm4 == 1 ~ "university",
      TRUE ~ NA_character_
    )
  )

# Convert parental education into factors (if desired)
df$father_educ <- factor(df$father_educ,
                         levels = c("primary", "secondary", "technical",
                                    "university"), ordered = FALSE)
df$mother_educ <- factor(df$mother_educ,
                         levels = c("primary", "secondary", "technical",
                                    "university"), ordered = FALSE)
# numeric codes for modeling
df$father_educ_num <- as.numeric(df$father_educ)
df$mother_educ_num <- as.numeric(df$mother_educ)



# 3. Other Key Variables to Factors -------------------------------
df <- df %>%
  mutate(
         icfes_stratum = factor(icfes_stratum,
                                levels = sort(unique(icfes_stratum)),
                                ordered = FALSE)) %>%
  mutate(
    # Combine school schedule dummies into one categorical variable.
    # According to the descriptions:
    # icfes_schoolsch1: jornada completa/única (full day)
    # icfes_schoolsch2: jornada de la mañana (morning)
    # icfes_schoolsch3: jornada de la noche (night)
    # icfes_schoolsch4: jornada de la tarde (afternoon)
    # icfes_schoolsch5: jornada de fines de semana (weekends)
    school_schedule = case_when(
      icfes_schoolsch1 == 1 ~ "completa",
      icfes_schoolsch2 == 1 ~ "mañana",
      icfes_schoolsch3 == 1 ~ "noche",
      icfes_schoolsch4 == 1 ~ "tarde",
      icfes_schoolsch5 == 1 ~ "fines_de_semana",
      TRUE ~ NA_character_
    ),
    school_schedule = factor(school_schedule,
                             levels = c("completa",
                                        "mañana", "tarde", "noche",
                                        "fines_de_semana"),
                             ordered = FALSE),

    # Combine school calendar dummies into one categorical variable.
    # school_schedule_a: Calendario A
    # school_schedule_b: Calendario B
    # school_schedule_other: Otro calendario
    school_calendar = case_when(
      school_schedule_a == 1 ~ "A",
      school_schedule_b == 1 ~ "B",
      school_schedule_other == 1 ~ "otro",
      TRUE ~ NA_character_
    ),
    school_calendar = factor(school_calendar,
                             levels = c("A", "B", "otro"),
                             ordered = FALSE),

    # Combine floor material dummies into one categorical variable.
    # floors_1: Cemento, ladrillo.
    # floors_2: Madera (baja calidad), tablones.
    # floors_3: Madera pulida (alta calidad), mármol.
    # floors_4: Tierra, arena.
    floor_material = case_when(
      floors_1 == 1 ~ "cemento_ladrillo",
      floors_2 == 1 ~ "madera_baja",
      floors_3 == 1 ~ "madera_alta_marmol",
      floors_4 == 1 ~ "tierra_arena",
      TRUE ~ NA_character_
    ),
    floor_material = factor(floor_material,
                            levels = c("cemento_ladrillo",
                                       "madera_baja", "madera_alta_marmol",
                                       "tierra_arena"),
                            ordered = FALSE)
  )


df <- df %>%
  mutate(school_calendar = fct_collapse(school_calendar, 
                                        "B_y_otro" = c("B", "otro")))

df <- df %>%
  mutate(age_gt21 = ifelse(icfes_age > 20, 1, 0))

#Set Sin Estrato from icfes_stratum as NAs
df$icfes_stratum[df$icfes_stratum == "Sin Estrato"] <- NA
#Lets remove the level  "Sin Estrato" from icfes_stratum
df$icfes_stratum <- droplevels(df$icfes_stratum)
# Optional: Check the new variables
summary(df$school_schedule)
summary(df$school_calendar)
summary(df$floor_material)
summary(df$mw_9_c_va)

# 4. Filter Data for 2013 and 2014 -------------------------------------------
# icfes_per contains year and semester info: 20132 for 2013 and 20142 for 2014
df_2013 <- df %>% filter(icfes_per == 20132)
df_2014 <- df %>% filter(icfes_per == 20142)
df_2015 <- df %>% filter(icfes_per == 20152)

# Assign a 'year' column
df_2013$year <- 2013
df_2014$year <- 2014
df_2015$year <- 2015
# Combine both years for later analysis if needed

# 5. Compute SABER 11 Z-scores -----------------------------------------------
# Use the 2014 data as reference for SABER 11 scores
mean_2014 <- mean(df_2014$puntaje_global_s11_final, na.rm = TRUE)
sd_2014   <- sd(df_2014$puntaje_global_s11_final, na.rm = TRUE)

# For 2014:
df_2014 <- df_2014 %>%
  mutate(z_saber = (puntaje_global_s11_final  - mean_2014) / sd_2014)

# For 2013, we have a corresponding score variable
# (e.g., puntaje_global_s11_final)
df_2013 <- df_2013 %>%
  mutate(z_saber = (puntaje_global_s11_final - mean_2014) / sd_2014)
# Use 2014 mean & sd for comparability
df_2015 <- df_2015 %>%
  mutate(z_saber = NA)
# Compute the Z-score cutoff based on the 2014 cutoff (e.g., 310)
cutoff_2014 <- 310
z_cutoff <- (cutoff_2014 - mean_2014) / sd_2014
cat("Z-score cutoff based on 310 in 2014 scale:", round(z_cutoff, 2), "\n")

# Mark eligibility based on z-scores
df_2014 <- df_2014 %>% mutate(z_eligible = z_saber >= z_cutoff)
df_2013 <- df_2013 %>% mutate(z_eligible = z_saber >= z_cutoff)
df_2015 <- df_2015 %>% mutate(z_eligible = NA)


# Recombine after adding the outcome variable
df_combined_2 <- rbind(df_2013, df_2014)  %>%
  rbind(df_2015)  %>%
  filter(!is.na(sisben_score))

# 7. Plot Distributions and Check Proportions -------------------------------
ggplot(df_combined_2, aes(x = z_saber, fill = factor(year))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Z-scores de Saber 11",
       x = "Z-score",
       y = "Densidad",
       fill = "Año") +
  geom_vline(xintercept = z_cutoff, linetype = "dashed", color = "green") +
  theme_minimal()

ggplot(df_combined_2, aes(x = puntaje_global_s11_final, fill = factor(year))) +
  geom_density(alpha = 0.5) +
  labs(title = "Saber 11 density distribution",
       x = "Saber 11 Score",
       y = "Density",
       fill = "Year") +
  geom_vline(xintercept = 310, linetype = "dashed", color = "green") + 
  geom_vline(xintercept = 318, linetype = "dashed", color = "#ff00f7") +
  theme_minimal()
# Remove NA from sisben_area just for plotting
df_plot <- df_combined_2 %>%
  filter(!is.na(sisben_area)) %>%
  mutate(sisben_area = factor(sisben_area, 
                              levels = c(1, 2, 3),
                              labels = c("Area 1: Major Cities",
                                         "Area 2: Other Urban",
                                         "Area 3: Rural")))

# Create the plot
ggplot(df_plot, aes(x = sisben_score, fill = factor(year))) +
  geom_density(alpha = 0.5) +
  labs(title = "Sisben Score Density Distribution",
       x = "SISBEN Score",
       y = "Density",
       fill = "Year") +
  geom_vline(xintercept = 57, linetype = "dashed", color = "green") + 
  geom_vline(xintercept = 41, linetype = "dashed", color = "#ff00f7") +
  theme_minimal() +
  facet_wrap(~ sisben_area, ncol = 1)

rm(df_plot)

# Check the proportion above the cutoff for each year
step1_2013 <- df_2013 %>% filter(!is.na(sisben_score))
step2_2013 <- step1_2013 %>% filter(z_saber >= z_cutoff)
prop_2013 <- nrow(step2_2013) / nrow(step1_2013)

step1_2014 <- df_2014 %>% filter(!is.na(sisben_score))
step2_2014 <- step1_2014 %>% filter(z_saber >= z_cutoff)
prop_2014 <- nrow(step2_2014) / nrow(step1_2014)

step1_2015 <- df_2015 %>% filter(!is.na(sisben_score))
step2_2015 <- step1_2015 %>% filter(puntaje_global_s11_final >= 318)
prop_2015 <- nrow(step2_2015) / nrow(step1_2015)


cat("Proporción en 2013 por encima del corte Saber 11:",
    round(prop_2013 * 100, 2), "%\n")
cat("Proporción en 2014 por encima del corte Saber 11:",
    round(prop_2014 * 100, 2), "%\n")
cat("Proporción en 2015 por encima del corte Saber 11:",
    round(prop_2015 * 100, 2), "%\n")

df_combined <- rbind(df_2013, df_2014) %>%
  rbind(df_2015)
# 8. Save Processed Data -----------------------------------------------------
saveRDS(df_2013, "df_2013_processed.rds")
saveRDS(df_2014, "df_2014_processed.rds")
saveRDS(df_combined, "df_combined.rds")
saveRDS(df_2015, "df_2015_processed.rds")
rm(list = ls()) # Clear workspace to free memory
