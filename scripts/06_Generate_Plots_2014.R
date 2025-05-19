# Load required libraries (tidyverse includes dplyr and ggplot2)
library(grf)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(e1071)  # for skewness function
library(iml)
library(patchwork)

# In case you are using a Mac and Visual Studio Code,
# uncomment the following line to set the path to Pandoc
# Sys.setenv(RSTUDIO_PANDOC =
#            '/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64')


############ Data Preparation ############
# --- A. Setup and Data Loading ---
#load the data

df_2014 <- readRDS("df_combined_mod.rds") %>%
  mutate(across(
    where(~ is.factor(.) && all(levels(.) %in% c("0", "1"))),
    ~ as.numeric(as.character(.))
  ))  %>%
  filter(icfes_age < 21)  %>%
  mutate(sisben_area = haven::as_factor(sisben_area))  %>%
  mutate(icfes_privatehs = haven::zap_label(icfes_privatehs) %>% as.double())

# --- C. Define Outcomes and Labels ---
# List of outcome variable names (as strings)
outcomes <- c(
  "access_0",
  "access_0_hq",
  "access_0_hq_pri",
  "graduation_exam_pro",
  "graduation_exam_pro_hq",
  "graduation_exam_pro_hq_pri",
  "mw_9",
  "wage0_9",
  "ln_wage_9",
  "mw_9_c_va",
  "work_9"
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
    "Salary at nine years (in minimum wages)",
    "Salary at nine years (in constant Colombian pesos)",
    "Log of salary at nine years (in constant Colombian pesos)",
    "Program value-added",
    "Working status at nine years (1 = working, 0 = not working)"
  )
)


address_ind <- "Plots/Causal ML 3/Individual Outcomes"
address_tot <- "Plots/Causal ML 3/Total Outcomes"
address_params <- "Plots/Causal ML 3/Params 2014"

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
  "icfes_schoolsch1",          # School schedule indicator
  "ethnminority",              # Additional control: ethnic minority
  "school_schedule",           # Combined school schedule (factor)
  "school_calendar",           # Combined school calendar (factor)
  "floor_material",            # Combined floor materials (factor)
  "family_internet",
  "family_laptop",
  "family_car",
  # "family_cell_phone",
  "school_urban",
  "year"
)

# Create a named vector for pretty variable labels
pretty_labels <- c(
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
  "icfes_schoolsch1"         = "School: Full-time",
  "ethnminority"             = "Ethnic Minority Status",
  "school_schedule"          = "School Schedule",
  "school_calendar"          = "School Calendar",
  "floor_material"           = "Home Floor Material",
  "family_internet"          = "Internet Access at Home",
  "family_laptop"            = "Laptop Ownership",
  "family_car"               = "Car Ownership",
  # "family_cell_phone"        = "Cellphone Ownership",
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
  "sisben_area_Other urban area" = "SISBEN Area 2 - Other urban area",
  "sisben_area_Other.urban.area" = "SISBEN Area 2 - Other urban area",
  "sisben_area_Rural area" = "SISBEN Area 3 - Rural area",
  "school_schedule_mañana" = "School: Morning",
  "school_schedule_tarde" = "School: Afternoon",
  "school_schedule_noche" = "School: Night",
  "school_schedule_fines_de_semana" = "School: Weekend",
  "school_schedule_completa" = "School: Full-time",
  "school_calendar_B_y_otro" = "Calendar B or Other",
  "floor_material_madera_baja" = "Floor: Low-quality wood",
  "floor_material_madera_alta_marmol" = "Floor: High-quality wood/marble",
  "floor_material_tierra_arena" = "Floor: Dirt/Sand"
)

# Function to apply these labels easily in plots
pretty_label <- function(varname) {
  pretty_labels[varname]
}


# --- E. Bandwidth Selection ---
# For each outcome, we will select the bandwidth using rdrobust
# and store the results in a data frame.

bandwiths <- readRDS(paste(address_params, "bandwidths.rds", sep = "/"))



# --- E. Visualize Outcome Distributions ---
# Create an empty list to store histogram plots
hist_plots <- list()
# # # Loop over each outcome to create a histogram

df_2014_model <- df_2014 %>%
  select(all_of(x_vars),
         beneficiary_spp, all_of(outcomes), year, eligible_spp,
         running_saber11, running_sisben) %>%
  filter(running_saber11 > -max(bandwiths$bw_saber)) %>%
  # filter(beneficiary_spp == 1) %>%
  filter(running_sisben > -max(bandwiths$bw_sisben))

# # # # # Load the package
library(DataExplorer)

# # # Create the HTML report
create_report(df_2014_model, output_file = "eda_report_2014.html",
              output_dir = paste(address_tot))

df_2014_model <- df_2014_model %>%
  # filter(year == "2014") %>%
  filter(eligible_spp == 1)

### Create the histograms

for (outcome in outcomes) {
  # Create a histogram for the current outcome variable using
  # aes_string to allow string variable names.
  p <- ggplot(df_2014_model, aes_string(x = outcome, fill = "factor(year)")) +
    geom_histogram(bins = 30, position = "dodge",
                   color = "white", na.rm = TRUE) +
    labs(title = paste("2014 - Histogram of ", outcome, "\n",
           outcome_info$Description[outcome_info$Outcome == outcome],
           sep = ""
         ),
         x = outcome_info$Description[outcome_info$Outcome == outcome],
         y = "Count",
         fill = "Year") +
    theme_minimal()

  hist_plots[[outcome]] <- p
}

# Combine all histograms into a multi-panel plot (3 columns)
combined_hist <- wrap_plots(hist_plots, ncol = 3)
# print(combined_hist)

# Optionally, save the combined plot as a PNG:
ggsave(paste(address_tot, "Combined_Histograms.png", sep = "/"),
       plot = combined_hist, width = 17, height = 11)

rm(df_2014_model, hist_plots, combined_hist, p)

# --- F. Fit Causal Forest and Analyze HTEs ---
# For each outcome, we estimate HTEs using Causal Forests,
# and plot: histogram of HTEs, variable importance, PDPs, and 3D visualization.
# Initialize empty lists to store output plots
plots_list <- list()
varimp_plots <- list()
pdp_plots <- list()
CI_plots <- list()

############### Loop Over Outcomes ###############
for (outcome in outcomes) {
  path <- file.path(address_ind, outcome)
  # Filter the modeling dataset for the covariates,
  # treatment indicator, and outcomes, dropping missing rows
  df_2014_model <- df_2014 %>%
    select(all_of(x_vars), beneficiary_spp, outcome, year, eligible_spp,
           running_saber11, running_sisben) %>%
    drop_na()  %>%
    mutate(across(
      where(~ is.factor(.) && all(levels(.) %in% c("0", "1"))),
      ~ as.numeric(as.character(.))
    ))

  df_2014_2 <- df_2014_model  %>%
    filter(year == "2014")  %>%
    filter(running_saber11 > -bandwiths$bw_saber[bandwiths$outcome == outcome]) %>%
    # filter(beneficiary_spp == 1) %>%
    filter(running_sisben > -bandwiths$bw_sisben[bandwiths$outcome == outcome])

  df_2014_1 <- df_2014_model  %>%
    filter(year == "2013")

  df_2014_model <- rbind(df_2014_1, df_2014_2)  %>%
    select(-running_saber11, -running_sisben)


  # Create design matrix (X) for the covariates (remove intercept with -1)
  x_2014 <- model.matrix(~ . - 1,
                         data = df_2014_model %>% select(all_of(x_vars)))
  # Adjust column names to add an underscore
  # between variable name and level for clarity.
  colnames(x_2014) <- gsub(
    "(mother_educ|school_calendar|sisben_area|father_educ|icfes_stratum|floor_material|school_schedule)(.*)$",
                           "\\1_\\2", colnames(x_2014))
  w_2014 <- as.numeric(df_2014_model$beneficiary_spp)

  cat("\nFitting model for outcome:", outcome, "...\n")

  # Step 1: Fit causal forest model to estimate conditional
  # average treatment effects
  # (a) Define the outcome vector (Y) for the current outcome
  y_2014_raw <- df_2014_model[[outcome]]

  # Ensure y_2014 is a numeric vector
  # If it's factor, convert to numeric codes (0/1 for binary)
  if (is.factor(y_2014_raw)) {
    # If it's binary factor with levels c("0","1"),
    # as.numeric() might yield c(1,2).
    # Subtract 1 to make it c(0,1). Adjust logic if your factor levels differ.
    y_2014 <- as.numeric(y_2014_raw) - 1
  } else {
    y_2014 <- as.numeric(y_2014_raw)
  }
  #Check that y, x and w are the same length
  length(y_2014)
  nrow(x_2014)
  print(length(w_2014))

  # # b. Fit the causal forest
  
  cf_model <- readRDS(paste(address_params, paste0("cf_model_",
                                            outcome, ".rds"),
                       sep = "/"))

  df_2014_model <- df_2014_model %>%
    filter(year == "2014") %>%
    filter(eligible_spp == 1)

  # Crear la matriz de diseño (X) para las covariables
  x_2014 <- model.matrix(~ . - 1,
                         data = df_2014_model %>% select(all_of(x_vars)))
  colnames(x_2014) <- gsub(
    "(mother_educ|school_calendar|sisben_area|father_educ|icfes_stratum|floor_material|school_schedule)(.*)$",
                           "\\1_\\2", colnames(x_2014))

  ################## Histogram of HTEs ##################
  # Step 2: Predict individual-level treatment effects (HTEs)
  tau_hat <- predict(cf_model, newdata = x_2014)$predictions
  hte_skew <- skewness(tau_hat)
  #curtosis
  hte_kurt <- kurtosis(tau_hat)
  # d. Plot the histogram of the HTE
  p <- ggplot(data.frame(tau_hat = tau_hat), aes(x = tau_hat)) +
    geom_histogram(bins = 40, fill = "skyblue", color = "white") +
    labs(title = paste("2014 - Histogram of HTEs for ", outcome, "\n",
           outcome_info$Description[outcome_info$Outcome == outcome],
           sep = ""
         ),
         x = "Estimated Treatment Effect",
         y = "Count") +
    theme_minimal() +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = paste("Skewness =", round(hte_skew, 3),
                    "\nKurtosis =", round(hte_kurt, 3)),
      hjust = 1.1, vjust = 2, size = 3, color = "red"
    )

  # save each plot as a PNG:
  ggsave(filename = paste0(path, "/HTE_", outcome, ".png"),
         plot = p, width = 5, height = 4)

  plots_list[[outcome]] <- p

  # # Step 3: Analyze variable importance
  varimp_df <- variable_importance(cf_model) %>%
    as.data.frame() %>%
    mutate(variable = colnames(cf_model$X.orig)) %>%
    arrange(desc(V1))

  varimp_df$pretty_name <- pretty_label(varimp_df$variable)

  # Check which variables resulted in NA
  na_variables <- varimp_df$variable[is.na(varimp_df$pretty_name)]
  print(na_variables)

  #g. Plot the variable importance
  # Create a bar plot for variable importance
  p2 <- ggplot(varimp_df,
               aes(x = reorder(pretty_label(variable), V1), y = V1)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = paste("2014 - Variable Importance for ", outcome,
           " \n",
           outcome_info$Description[outcome_info$Outcome == outcome],
           sep = ""
         ),
         x = "Variable",
         y = "Importance") +
    theme_minimal()

  ggsave(filename = paste0(path, "/VarImp_", outcome, ".png"),
         plot = p2, width = 8, height = 7)

  p3 <- ggplot(head(varimp_df, 15),
               aes(x = reorder(pretty_label(variable), V1), y = V1)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = paste("2014 - Variable Importance for ", outcome,
           "\n",
           outcome_info$Description[outcome_info$Outcome == outcome],
           sep = ""
         ),
         x = "Variable",
         y = "Importance") +
    theme_minimal()
  # Store the plot in the list
  varimp_plots[[outcome]] <- p3

  rm(p2, p3, na_variables, p, hte_kurt, hte_skew)

  # # 5. Confidence Interval Plots
  # # Predict HTEs with variance
  pred <- predict(cf_model, estimate.variance = TRUE, newdata = x_2014)
  tau_hat <- pred$predictions
  tau_se <- sqrt(pred$variance.estimates)

  # Compute 95% Confidence Interval
  tau_ci_low <- tau_hat - 1.96 * tau_se
  tau_ci_high <- tau_hat + 1.96 * tau_se

  # Optional: sort by tau_hat for cleaner plot
  df_ci <- data.frame(
    tau = tau_hat,
    lower = tau_ci_low,
    upper = tau_ci_high
  ) %>%
    arrange(tau) %>%
    mutate(index = row_number())

  # Plot with ggplot
  library(ggplot2)

  p8 <- ggplot(df_ci, aes(x = index, y = tau)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, alpha = 0.1) +
    geom_point(color = "blue", size = 0.8, alpha = 1) +
    labs(
      title = paste("2014 -  HTE with 95% CI for ", outcome, "\n",
        outcome_info$Description[outcome_info$Outcome == outcome],
        sep = ""
      ),
      x = "Individuals (sorted by effect size)",
      y = "Estimated Treatment Effect (HTE)"
    ) +
    theme_minimal()

  ggsave(filename = paste0(path, "/HTE_CI_", outcome, ".png"),
         plot = p8, width = 7, height = 5)
  CI_plots[[outcome]] <- p8

  rm(tau_se, tau_ci_high, tau_ci_low, df_ci, p8)


  # Step 6: 3D plot of HTEs across top 2 variables

  # 6.1. Get the top 2 variables

  top_vars <- head(varimp_df$variable, 3)
  print(paste("Top 3 variables for", outcome, ":",
              paste(head(varimp_df$variable, 3), collapse = ", ")))
  for (i in 1:length(top_vars)) {
    if(!(top_vars[i] %in% names(df_2014_model))) {
      # Remove trailing digits
      base_name <- gsub("[0-9]+$", "", top_vars[i])
      matched <- grep(paste0("^", base_name, "$"),
                      names(df_2014_model), value = TRUE)
      if(length(matched) >= 1) {
        top_vars[i] <- matched[1]
     } else {
        base_name <- str_extract(top_vars[i], "^[^_]+_[^_]+")  
        # works for "father_educ_university" → "father_educ"
        matched <- grep(paste0("^", base_name, "$"),
                        names(df_2014_model), value = TRUE)

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
    if (!(additional_var %in% names(df_2014_model))) {
      # Remove trailing digits
      base_name <- gsub("[0-9]+$", "", additional_var)
      matched <- grep(paste0("^", base_name, "$"),
                      names(df_2014_model), value = TRUE)
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

  # Step 5: 3D plot of HTEs across top 2 variables
  # (i) Create a 3D scatter plot using plotly for the top 2 variables and HTE
  vars3 <- varimp_df$variable[1:3]
  # 2) helper: detect factor OR 0/1 dummy
  is_dummy <- function(x) {
    is.numeric(x) && length(unique(x)) == 2 && all(sort(unique(x)) == c(0,1))
  }
  is_cat   <- sapply(vars3,
    function(v) is.factor(x_2014[, v]) || is_dummy(x_2014[, v])
  )
  cats <- vars3[is_cat]
  nums <- vars3[!is_cat]

  plot_data <- df_2014_model %>%
    mutate(tau_hat = tau_hat) %>%
    select(all_of(top_vars), tau_hat,puntaje_global_s11_final, sisben_score)

  label_flags <- sapply(top_vars, function(v) {
    haven::is.labelled(plot_data[[v]])
  })

  if (any(label_flags)) {
    warning(
      "These variables were labelled and may trigger a stack overflow,
       so I de-labelled them: ",
      paste(top_vars[label_flags], collapse = ", ")
    )
    plot_data <- haven::zap_labels(plot_data)
  }

  if ("father_educ" %in% top_vars) {
    index <- which(top_vars == "father_educ") 
    # Define ordered levels explicitly
    plot_data[[top_vars[index]]] <- factor(
      plot_data[[top_vars[index]]], 
      levels = c("primary", "secondary", "technical", "university"),
      ordered = TRUE
    )
    plot_data$father_educ_num <- as.numeric(plot_data[[top_vars[index]]])
    # zzz <- top_vars[index]
    # top_vars[index] <- "father_educ_num"
  }

  # 7. Create the 3D scatter plot with plotly
  p3d <- plot_ly(
    data = plot_data,
    x = as.formula(paste0("~", "puntaje_global_s11_final")),
    y = as.formula(paste0("~", "sisben_score")),
    z = ~tau_hat,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      color = ~tau_hat,
      colorscale = "RdBu",
      opacity = 0.7
    )
  ) %>%
    layout(
      title = paste("2014 - 3D HTE Plot for",
                    outcome_info$Description[outcome_info$Outcome == outcome],
                    "\nTop Vars:",
                    paste(pretty_label(top_vars), collapse = " & ")),
      scene = list(
        xaxis = list(title = pretty_label("puntaje_global_s11_final")),
        yaxis = list(title = pretty_label("sisben_score")),
        zaxis = list(title = "HTE")
      )
    )

  # p_t <- ggplot(plot_data, aes(x = puntaje_global_s11_final, y = sisben_score, color = tau_hat)) +
  # geom_point(alpha = 0.6) +
  # scale_color_viridis_b() +
  # facet_wrap(~ mother_educ) + #This is for the categorical variable, which i manually update
  # theme_minimal() +
  # labs(
  #   title = paste("2014 - HTE Plot for",
  #                 outcome_info$Description[outcome_info$Outcome == outcome],
  #                 "\nTop Vars:",
  #                 paste(pretty_label("mother_educ"), collapse = " & ")),
  #   x = pretty_label("puntaje_global_s11_final"),
  #   y = pretty_label("sisben_score"),
  #   color = "HTE"
  # )

  # ggsave(filename = paste0(path, "/HTE_1_2014_", outcome, ".png"),
  #        plot = p_t, width = 7, height = 5)

  # 8. Save the plot as an HTML file
  #(using selfcontained = FALSE if Pandoc is not installed)
  filename <- paste0(path, "/3D_HTE_", outcome, ".html")
  saveWidget(as_widget(p3d), filename, selfcontained = TRUE)

  cat("Saved 3D plot for", outcome, "as", filename, "\n\n")


  # # Step 4: Visualize partial dependence using top 2 important variables
  # # Partial Dependence Plot for the Top 2 Important Variables
  # # Suppose cf_model is your fitted causal forest
  # # We wrap it with Predictor from iml
  predict_function <- function(model, newdata) {
    predict(model, newdata)$predictions
  }

  y_2014_raw <- df_2014_model[[outcome]]

  # Ensure y_2014 is a numeric vector
  # If it's factor, convert to numeric codes (0/1 for binary)
  if (is.factor(y_2014_raw)) {
    # If it's binary factor with levels c("0","1"),
    # as.numeric() might yield c(1,2).
    # Subtract 1 to make it c(0,1). Adjust logic if your factor levels differ.
    y_2014 <- as.numeric(y_2014_raw) - 1
  } else {
    y_2014 <- as.numeric(y_2014_raw)
  }
  predictor <- Predictor$new(cf_model, data = data.frame(x_2014), y = y_2014,
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
  # two_var_pdp <- FeatureEffect$new(
  #   predictor,
  #   feature = top_vars_mapped,     # a vector of length 2
  #   method = "pdp",         # partial dependence
  #   grid.size = grid_sizes          # how fine the grid is
  # )
  # saveRDS(two_var_pdp, file = paste(address_params,
  #                                   paste0("pdp_",
  #                                          outcome, ".rds"),
  #                                   sep = "/")
  # )
  two_var_pdp <- readRDS(paste(address_params,
                               paste0("pdp_",
                                      outcome, ".rds"),
                               sep = "/"))

  if (c("icfes_female") %in% top_vars) {
    print(paste("Variable", top_vars[1],
                "value is", head(varimp_df$variable, 1)[1]))
  }
  # The result is a FeatureEffect object with 2D partial dependence data
  # We can plot it as a heatmap:
  p4 <- plot(two_var_pdp) +
    ggtitle(paste(outcome_info$Description[outcome_info$Outcome == outcome],
                  "\n:HTE PDP on",
                  paste(pretty_label(top_vars), collapse = " & "))) +
    theme(plot.title = element_text(size = 10)) +
    labs(
      x = pretty_label(top_vars_mapped[1]),
      y = pretty_label(top_vars_mapped[2]),
      fill = "Partial Dependence"
    ) +
    scale_fill_gradient(
      low = "lightblue",
      high = "darkblue"
    )

  ggsave(filename = paste0(path, "/PDP_", outcome, ".png"),
         plot = p4, width = 7, height = 5)
  pdp_plots[[outcome]] <- p4


  # Grab the raw grid from iml’s FeatureEffect object
  # Note: FeatureEffect$new(method="pdp") stores its grid in $results
  df_pdp <- as.data.frame(two_var_pdp$results)

  # Identify your two features
  x_feat <- top_vars_mapped[1]
  y_feat <- top_vars_mapped[2]

  # Extract the unique sorted values for each axis
  x_vals <- sort(unique(df_pdp[[x_feat]]))
  y_vals <- sort(unique(df_pdp[[y_feat]]))

  # Re‑shape the PDP values into a matrix for the surface
  z_matrix <- matrix(
    df_pdp$.value,
    nrow = length(x_vals),
    ncol = length(y_vals),
    byrow = FALSE
  )

  # Now build the 3D surface with Plotly
  p3d_surface <- plot_ly(
    x    = x_vals,
    y    = y_vals,
    z    = z_matrix,
    type = "surface",
    colors = colorRamp(c("lightblue", "darkblue"))
  ) %>%
    layout(
      title = paste(
        "2014 - 3D PDP Surface for",
        outcome_info$Description[outcome_info$Outcome == outcome]
      ),
      scene = list(
        xaxis = list(title = pretty_label(x_feat)),
        yaxis = list(title = pretty_label(y_feat)),
        zaxis = list(title = "Partial Dependence")
      )
    )

  # If you want to save it as HTML (non-self-contained) or PNG:
  htmlwidgets::saveWidget(p3d_surface,
    paste(path, "/3D_PDP_", outcome, ".html", sep = ""),
    selfcontained = TRUE
  )


  #Other HTE surfae plot

  # 1) Identify your two top features (in the raw df_model names)
  f1 <- "sisben_score"
  f2 <- "puntaje_global_s11_final"

  # 2) Decide grid values for each feature
  grid_vals <- lapply(list(f1, f2), function(f) {
    vec <- df_2014_model[[f]]
    # treat factors or <=2 unique values as categorical
    if (is.factor(vec) || length(unique(vec)) <= 2) {
      sort(unique(vec))
    } else {
      seq(min(vec, na.rm = TRUE),
          max(vec, na.rm = TRUE),
          length.out = 30)
    }
  })
  names(grid_vals) <- c(f1, f2)
  x_seq <- grid_vals[[f1]]
  y_seq <- grid_vals[[f2]]

  # 3) Compute “typical” values for all other covariates
  others <- setdiff(x_vars, c(f1,f2))
  typical <- df_2014_model %>%
    summarise(across(all_of(others), ~ if(is.numeric(.)) {
      median(., na.rm = TRUE)
    } else {
      names(which.max(table(.)))
    }
    ))
  # 4) Build full grid plus typical covariates
  grid <- expand.grid(x_seq, y_seq)
  colnames(grid) <- c(f1, f2)

  # Attach the typical values
  grid_full <- bind_cols(
    grid,
    typical[rep(1, nrow(grid)), ]
  )
  for (col in x_vars) {
    if (is.factor(df_2014_model[[col]])) {
      grid_full[[col]] <- factor(
        grid_full[[col]],
        levels = levels(df_2014_model[[col]])
      )
    }
  }

  # 5) Create model matrix and predict HTE
  X_grid <- model.matrix(~ . - 1, data = grid_full %>% select(all_of(x_vars)))
  tau_grid <- predict(cf_model, newdata = X_grid)$predictions

  # 6) Reshape into a z‑matrix
  z_mat <- matrix(tau_grid,
                  nrow = length(x_seq),
                  ncol = length(y_seq),
                  byrow = FALSE)
  # 1) Turn `typical` into a little HTML string with line breaks
  note_lines <- lapply(names(typical), function(var) {
    paste0("<b>", pretty_label(var), "</b>: ", typical[[var]])
  })
  note_text <- paste0(note_lines, collapse = "<br>")
  # 7) Plot the 3D surface
  p3d_surface <- plot_ly(
    x    = x_seq,
    y    = y_seq,
    z    = z_mat,
    type = "surface",
    colors = colorRamp(c("lightblue","darkblue"))
  ) %>%
    layout(
      title = paste("2014 - HTE Surface for",
                    outcome_info$Description[outcome_info$Outcome == outcome],
                    ":", pretty_label(f1), "×", pretty_label(f2)),
      scene = list(
        xaxis = list(title = pretty_label(f1)),
        yaxis = list(
          title    = pretty_label(f2),
          tickmode = "array",
          tickvals = y_seq,
          # if categorical/dummy, map 0/1 to labels:
          ticktext = if(length(y_seq)==2) c("0","1") else NULL
        ),
        zaxis = list(title = "Estimated HTE")
      ),
      # 3) Add the “fixed‐covariate” note in the top‐left corner
      annotations = list(
        list(
          text       = note_text,
          x          = 0.99,     # paper‐coords (0 = left, 1 = right)
          y          = 0,     # paper‐coords (0 = bottom, 1 = top)
          xref       = "paper",
          yref       = "paper",
          showarrow  = FALSE,
          align      = "center",
          font       = list(size = 7),
          bordercolor = "#444",
          borderwidth = 1,
          bgcolor    = "rgba(255,255,255,0.7)"

        )
      )
    )
  # Save the plot as an HTML file
  filename <- paste0(path, "/3D_HTE_Surface_", outcome, ".html")
  saveWidget(as_widget(p3d_surface), filename, selfcontained = FALSE)
  
}

# --- G. Combine and Export All Plots ---
library(patchwork)
# Combine all plots into one multi-panel figure (for example, 3 columns)
combined_plot <- wrap_plots(plots_list, ncol = 3,
  ggtitle("2014 - HTE Histogram Plots") +
    theme(plot.title = element_text(size = 10))
)


# Optionally, save the combined plot as a PNG
ggsave(paste(address_tot, "Combined_HTE_Plots_2014.png", sep = "/"),
       plot = combined_plot,
       width = 14, height = 8)

# Combine all the variable importance plots into one multi-panel figure
combined_varimp <- wrap_plots(varimp_plots, ncol = 2,
  ggtitle("2014 - Variable Importance Plots")
)


# Optionally, save the combined plot as a PNG file
ggsave(paste(address_tot, "Combined_VarImp_Plots.png", sep = "/"),
       plot = combined_varimp, width = 13, height = 16)

# Combine all the pdp plots into one
combined_pdp <- wrap_plots(pdp_plots, ncol = 3
) +
  plot_annotation(title = "2014 – PDP Plots for HTE",
    theme = theme(plot.title = element_text(size = 20))
  )


# Optionally, save the combined plot as a PNG file
ggsave(paste(address_tot, "Combined_PDP_Plots_1.png", sep = "/"),
       plot = combined_pdp, width = 17, height = 10)

# End of Script
# Note to future reviewer: this script focuses on HTE estimation.

combined_ci <- wrap_plots(CI_plots, ncol = 3,
  ggtitle("2014 - Confidence Intervals Plots")
)
ggsave(paste(address_tot, "Combined_CI_Plots.png", sep = "/"),
       plot = combined_ci, width = 17, height = 18)
