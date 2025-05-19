#Guardar modelos para el año 2015
# 1. Carga y preprocesamiento
# 2. Estimación de modelos
# 3. Guardado de resultados

library(grf)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(e1071)  # for skewness function
library(iml)
library(patchwork)
library(haven)

# In case you are using a Mac and Visual Studio Code,
# uncomment the following line to set the path to Pandoc
# Sys.setenv(RSTUDIO_PANDOC =
#            '/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64')


# --- A. Setup and Data Loading ---
#load the data  (obtained via the script "02_Cleaning_2015.R")
df_2015 <- readRDS("df_combined_mod_2015.rds") %>%
  mutate(across(
    where(~ is.factor(.) && all(levels(.) %in% c("0", "1"))),
    ~ as.numeric(as.character(.))
  ))  %>%
  filter(icfes_age < 21)  %>% #keep only those under 21
  mutate(sisben_area = haven::as_factor(sisben_area))  %>%
  mutate(icfes_privatehs = haven::zap_label(icfes_privatehs) %>% as.double())
#Lets find the colnames that start with icfes_private


# --- C. Define Outcomes and Labels ---
# List of outcome variable names (as strings)
outcomes <- c(
  "access_0",
  "access_0_hq",
  "access_0_hq_pri",
  "graduation_exam_pro",
  "graduation_exam_pro_hq",
  "graduation_exam_pro_hq_pri",
  "mw_8",
  "wage0_8",
  "ln_wage_8",
  "mw_9_c_va",
  "work_8"
)

# Create a data frame with outcome names and descriptive labels

outcome_info <- data.frame(
  Outcome = outcomes,
  Description = c(
    "Immediate access to any college",
    "Immediate access to HQA college",
    "Immediate access to private HQA college",
    "Graduation from any college",
    "Graduation from any HQA college",
    "Graduation from any private HQA college",
    "Salary at eight years (in minimum wages)",
    "Salary at eight years (in constant Colombian pesos)",
    "Log of salary at eight years (in constant Colombian pesos)",
    "Program value-added",
    "Working status at eight years (1 = working, 0 = not working)"
  )
)

#Setting the address for the plots
address_ind <- "Plots/Causal ML 2015/Individual Outcomes"
address_tot <- "Plots/Causal ML 2015/Total Outcomes"
address_params <- "Plots/Causal ML 2015/Params"

# --- D. Define Covariates ---
# Define the list of covariate variables (using a character vector)
# If you have already prepared a design matrix, skip this step.
x_vars <- c(
  "puntaje_global_s11_final",  # test score
  "sisben_score",              # SISBEN score (raw)
  "icfes_female",              # Gender
  "icfes_age",                 # Age
  "father_educ",               # Father's education
  "mother_educ",               # Mother's education
  "icfes_stratum",             # Socioeconomic stratum
  "family_size",               # Family size
  "icfes_works",               # Employment status
  "icfes_privatehs",           # School type: public/private
  "sisben_area",               # SISBEN area
  # "icfes_schoolsch1",          # School schedule indicator
  "ethnminority",              # Additional control: ethnic minority
  "school_schedule",           # Combined school schedule (factor)
  "school_calendar",           # Combined school calendar (factor)
  "floor_material",            # Combined floor materials (factor)
  "family_internet",
  "family_laptop",
  "family_car",
  # "family_cell_phone",
  "school_urban"
)

# Create a named vector for pretty variable labels
pretty_labels <-c(
  "puntaje_global_s11_final" = "SABER 11 Test Score",
  "sisben_score"             = "SISBEN Poverty Score",
  "icfes_female"             = "Gender (Female)",
  "icfes_age"                = "Age",
  "father_educ"              = "Father's Education Level",
  "mother_educ"              = "Mother's Education Level",
  "icfes_stratum"            = "Socioeconomic Stratum",
  "family_size"              = "Family Size",
  "icfes_works"              = "Employment Status",
  "icfes_privatehs"          = "Private High School",
  "sisben_area"              = "SISBEN Area",
  "icfes_schoolsch1"         = "School Schedule Indicator",
  "ethnminority"             = "Ethnic Minority Status",
  "school_schedule"          = "School Schedule",
  "school_calendar"          = "School Calendar",
  "floor_material"           = "Home Floor Material",
  "family_internet"          = "Internet Access at Home",
  "family_laptop"            = "Laptop Ownership",
  "family_car"               = "Car Ownership",
  "family_cell_phone"        = "Cellphone Ownership",
  "school_urban"             = "Urban School",
  "year"                     = "Year of Graduation",
  "father_educ_primary" = "Father: Primary",
  "father_educ_secondary" = "Father: Secondary",
  "father_educ_technical" = "Father: Technical",
  "father_educ_university" = "Father: University",
  "mother_educ_primary" = "Mother: Primary",
  "mother_educ_secondary" = "Mother: Secondary",
  "mother_educ_technical" = "Mother: Technical",
  "mother_educ_university" = "Mother: University",
  "icfes_stratum_Estrato 1" = "Stratum 1",
  "icfes_stratum_Estrato 2" = "Stratum 2",
  "icfes_stratum_Estrato 3" = "Stratum 3",
  "icfes_stratum_Estrato 4" = "Stratum 4",
  "icfes_stratum_Estrato 5" = "Stratum 5",
  "icfes_stratum_Estrato 6" = "Stratum 6",
  "sisben_area_1" = "SISBEN Area 1 - Main metro area",
  "sisben_area_Main metro area" = "SISBEN Area 1 - Main metro area",
  "sisben_area_Other urban area" = "SISBEN Area 2 - Other urban area",
  "sisben_area_Rural area" = "SISBEN Area 3 - Rural area",
  "school_schedule_completa" = "School: Full Day",
  "school_schedule_mañana" = "School: Morning",
  "school_schedule_tarde" = "School: Afternoon",
  "school_schedule_noche" = "School: Night",
  "school_schedule_fines_de_semana" = "School: Weekend",
  "school_calendar_A"  = "Calendar A",
  "school_calendar_B_y_otro" = "Calendar B or Other",
  "floor_material_cemento_ladrillo" = "Floor: Cement/Brick",
  "floor_material_madera_baja" = "Floor: Low-quality wood",
  "floor_material_madera_alta_marmol" = "Floor: High-quality wood/marble",
  "floor_material_tierra_arena" = "Floor: Dirt/Sand"
)


# Function to apply these labels easily in plots
pretty_label <- function(varname) {
  pretty_labels[varname]
}

bandwiths <- tibble(
  outcome = outcomes,
  bw_saber = NA,
  bw_sisben = NA
)

library(rdrobust)

for (outcome in outcomes) {
  # Select the covariates and outcomes for the model
  df_2015_model <- df_2015 %>%
    select(all_of(x_vars), beneficiary_spp, outcome,
           running_saber11, running_sisben)  %>%
    drop_na()
  #Observe the distribution of the running variable saber 11
  rd_result_saber <- rdrobust(
    df_2015_model[[outcome]],
    df_2015_model$running_saber11,
    c = 0,
    fuzzy = df_2015_model$beneficiary_spp
  )
  # Store the bandwidth in the data frame
  bandwiths$bw_saber[bandwiths$outcome == outcome] <- rd_result_saber$bws["h", "left"]

  #Now we do the same for sisben
  rd_result_sisben <- rdrobust(
    df_2015_model[[outcome]],
    df_2015_model$running_sisben,
    c = 0,
    fuzzy = df_2015_model$beneficiary_spp
  )
  # Store the bandwidth in the data frame
  bandwiths$bw_sisben[bandwiths$outcome == outcome] <- rd_result_sisben$bws["h", "left"]

}
rm(df_2014_model, rd_result_saber, rd_result_sisben)

# save the bandwidths as rds in params
saveRDS(bandwiths,
        file = paste(address_params, "bandwidths.rds", sep = "/"))

bandwiths <- readRDS(paste(address_params, "bandwidths.rds", sep = "/"))

# --- F. Fit Causal Forest and Analyze HTEs ---
# For each outcome, we estimate HTEs using Causal Forests,
# and plot: histogram of HTEs, variable importance, PDPs, and 3D visualization.
# Initialize empty lists to store output plots
plots_list <- list()
varimp_plots <- list()
pdp_plots <- list()
CI_plots <- list()

# Loop Over Outcomes ----------------------------------------------------
for (outcome in outcomes) {
  path <- file.path(address_ind, outcome)
  # Filter the modeling dataset for the covariates,
  # treatment indicator, and outcomes, dropping missing rows
  df_2015_model <- df_2015 %>%
    select(all_of(x_vars), beneficiary_spp, outcome, eligible_spp,
           running_saber11, running_sisben) %>%
    drop_na()  %>%
    mutate(across(
      where(~ is.factor(.) && all(levels(.) %in% c("0", "1"))),
      ~ as.numeric(as.character(.))
    )) %>%
    filter(running_saber11 > -bandwiths$bw_saber[bandwiths$outcome == outcome]) %>%
    filter(running_sisben > -bandwiths$bw_sisben[bandwiths$outcome == outcome]) %>%
    select(-running_saber11, -running_sisben)

  # Create design matrix (X) for the covariates (remove intercept with -1)
  x_2015 <- model.matrix(~ . - 1,
    data = df_2015_model %>% select(all_of(x_vars)),
    contrasts.arg = lapply(df_2015_model %>% select(where(is.factor)),
      function(x) {
        contrasts(x, contrasts = FALSE)
      }
    )
  )
  # Adjust column names to add an underscore
  # between variable name and level for clarity.
  colnames(x_2015) <- gsub("(mother_educ|school_calendar|sisben_area|father_educ|icfes_stratum|floor_material|school_schedule)(.*)$",
                         "\\1_\\2", colnames(x_2015))
  w_2015 <- as.numeric(df_2015_model$beneficiary_spp)

  cat("\nFitting model for outcome:", outcome, "...\n")

  # Step 1: Fit causal forest model to estimate conditional
  # average treatment effects
  # (a) Define the outcome vector (Y) for the current outcome
  y_2015_raw <- df_2015_model[[outcome]]

  # Ensure y_2014 is a numeric vector
  # If it's factor, convert to numeric codes (0/1 for binary)
  if (is.factor(y_2015_raw)) {
    # If it's binary factor with levels c("0","1"),
    # as.numeric() might yield c(1,2).
    # Subtract 1 to make it c(0,1). Adjust logic if your factor levels differ.
    y_2015 <- as.numeric(y_2015_raw) - 1
  } else {
    y_2015 <- as.numeric(y_2015_raw)
  }
  #Check that y, x and w are the same length
  length(y_2015)
  nrow(x_2015)
  print(length(w_2015))

  # b. Fit the causal forest
  cf_model <- causal_forest(x_2015, y_2015, w_2015,
    num.trees          = 3000,
    tune.parameters    = "all",
    seed               = 123
  )

  # c. Save the model
  saveRDS(cf_model,
          file = paste(address_params, paste0("cf_model_",
                                              outcome, "_2015.rds"),
                       sep = "/"))

  # cf_model <- readRDS(paste(address_params, paste0("cf_model_",
  #                                             outcome, "_2015.rds"),
  #                      sep = "/"))

  df_2015_model <- df_2015_model %>%
    filter(eligible_spp == 1)
  # # Crear la matriz de diseño (X) para las covariables
  x_2015 <- model.matrix(~ . - 1,
    data = df_2015_model %>% select(all_of(x_vars)),
    contrasts.arg = lapply(df_2015_model %>% select(where(is.factor)),
      function(x) {
        contrasts(x, contrasts = FALSE)
      }
    )
  )
  colnames(x_2015) <- gsub("(mother_educ|school_calendar|sisben_area|father_educ|icfes_stratum|floor_material|school_schedule)(.*)$",
                         "\\1_\\2", colnames(x_2015))

  # # # Step 3: Analyze variable importance
  varimp_df <- variable_importance(cf_model) %>%
    as.data.frame() %>%
    mutate(variable = colnames(cf_model$X.orig)) %>%
    arrange(desc(V1))

  varimp_df$pretty_name <- pretty_label(varimp_df$variable)

  # # Predict HTEs with variance
  pred <- predict(cf_model, estimate.variance = TRUE, newdata = x_2015)
  tau_hat <- pred$predictions
  
  # # 6.1. Get the top 2 variables

  top_vars <- head(varimp_df$variable, 2)
  for (i in 1:length(top_vars)) {
    if(!(top_vars[i] %in% names(df_2015_model))) {
      # Remove trailing digits
      base_name <- gsub("[0-9]+$", "", top_vars[i])
      matched <- grep(paste0("^", base_name, "$"),
                      names(df_2015_model), value = TRUE)
      if(length(matched) >= 1) {
        top_vars[i] <- matched[1]
      } else {
        base_name <- str_extract(top_vars[i], "^[^_]+_[^_]+")  
        # works for "father_educ_university" → "father_educ"
        matched <- grep(paste0("^", base_name, "$"),
                        names(df_2015_model), value = TRUE)

        if (length(matched) >= 1) {
          top_vars[i] <- matched[1]
        } else {
          warning(paste("No matching base column found for:", top_vars[i]))
        }
      }
    }
  }

  if (length(unique(top_vars)) < 2) {
    label1 <- head(varimp_df$variable, 1)[1]
    additional_var <- head(varimp_df$variable, 3)[3]
    if (!(additional_var %in% names(df_2015_model))) {
      # Remove trailing digits
      base_name <- gsub("[0-9]+$", "", additional_var)
      matched <- grep(paste0("^", base_name, "$"),
                      names(df_2015_model), value = TRUE)
      if (length(matched) >= 1) {
        additional_var <- matched[1]
      } else {
        warning(paste("No matching column found for", additional_var))
      }
    }
    if (!is.na(additional_var)) {
      top_vars[2] <- additional_var
      print(paste("The first variable name was", top_vars[1]))
    } else {
      stop("No additional variable available to replace duplicate in top_vars.")
    }
  }


  cat("Top 2 variables for", outcome, ":",
      paste(top_vars, collapse = ", "), "\n")

  # Partial Dependence Plot for the Top 2 Important Variables
  # We wrap it with Predictor from iml
  predict_function <- function(model, newdata) {
    predict(model, newdata)$predictions
  }

  y_2015_raw <- df_2015_model[[outcome]]

  # Ensure y_2014 is a numeric vector
  # If it's factor, convert to numeric codes (0/1 for binary)
  if (is.factor(y_2015_raw)) {
    # If it's binary factor with levels c("0","1"),
    # as.numeric() might yield c(1,2).
    # Subtract 1 to make it c(0,1). Adjust logic if your factor levels differ.
    y_2015 <- as.numeric(y_2015_raw) - 1
  } else {
    y_2015 <- as.numeric(y_2015_raw)
  }
  print(length(y_2015))
  predictor <- Predictor$new(cf_model, data = data.frame(x_2015), y = y_2015,
                             predict.function = predict_function)

  # Map each variable in top_vars to the first matching column
  # name in predictor$data$feature.names
  top_vars_mapped <- sapply(top_vars, function(var) {
    matches <- grep(paste0("^", var),
                    head(varimp_df$variable, 3), value = TRUE)
    if (length(matches) == 0) {
      stop("No matching column found for ", var)
    } else {
      return(matches[1])
    }
  })
  cat("Mapped top_vars: ", paste(top_vars_mapped, collapse = ", "), "\n")
  grid_sizes <- sapply(top_vars_mapped, function(var) {
    vec   <- predictor$data$X[[var]]
    n_uniq <- length(unique(vec))
    # treat any 2‑level variable (factor or numeric) as a dummy
    if (is.factor(vec) || (is.numeric(vec) && n_uniq == 2)) {
      return(n_uniq)      # yields 2
    } else {
      return(15)          # or however many points you want for continuous
    }
  })
  top_vars_mapped <- gsub(" ", ".", top_vars_mapped, fixed = TRUE)
  # Suppose top_vars has the top two variable names,
  # e.g. c("z_saber", "sisben_score")
  two_var_pdp <- FeatureEffect$new(
    predictor,
    feature = top_vars_mapped,     # a vector of length 2
    method = "pdp",         # partial dependence
    grid.size = grid_sizes          # how fine the grid is. 15 is usual.
  )

  saveRDS(two_var_pdp, file = paste(address_params,
                                    paste0("pdp_",
                                           outcome, "_2015.rds"),
                                    sep = "/")
  )

}
