# Replication Package – Causal Random Forest for SPP (Ser Pilo Paga)

This project estimates the **heterogeneous treatment effects (HTE)** of the *Ser Pilo Paga (SPP)* financial aid program on various outcomes such as university enrollment, graduation, and long-term labor market performance. The analysis focuses on how the effect of receiving the SPP scholarship varies across individuals with different baseline characteristics.

### Causal Identification Strategy

We follow a potential outcomes framework. The treatment of interest is receiving the SPP scholarship (`W_i = 1`), and the outcome (`Y_i`) can be access to higher education, graduation, or labor market measures.

The estimand is the **Conditional Average Treatment Effect (CATE):**

$$\tau(X_i) = \mathbb{E}[Y_i(1) - Y_i(0) \mid X_i]$$


This repository contains the full set of scripts used to prepare, estimate, and visualize heterogeneous treatment effect models of Colombia's *Ser Pilo Paga* (SPP) program for the 2014 and 2015 cohorts, using Causal Forests.

Plots of the Heterogeneus Treatment Effects (HTE) can be found in the following [URL](https://luisjo819ucla.github.io/SPP_Causal_forest/) 
The scripts can be found in the scripts folder.
---

## 📁 Project Structure

Try to follow this structure in your work space.

```
replication_package/
├── data/                  # Required .rds and .dta files
├── scripts/               # Scripts organized by stage
├── output/                # Intermediate results (optional)
├── plots/                 # Generated visualizations
└── README.md              # This document
```

---

The original datasets are confidential and not included in this repository.

---

## 📜 Script Descriptions

### 1. `01_Unite_Data.R`

* Merges the 2014 and 2015 datasets.
* Performs initial cleaning: filters out individuals who were already in higher education and removes critical NAs.
* Merges with the value-added dataset (`va_prog`).
* Output: a cleaned, consolidated dataset.

### 2. `02_Cleaning_2014.R`

* Prepares the *estimation sample* for the 2014 cohort.
* Converts variables to numeric format.
* Removes missing values in key variables.
* Output: `df_robust` 2014, ready for modeling.

### 3. `03_Cleaning_2015.R`

* Repeats the *estimation sample* preparation for the 2015 cohort.
* Same variable conversion and cleaning as in `02_Cleaning_2014.R`.

### 4. `04_Model_Save_2014.R`

* Loads the 2014 estimation sample.
* Estimates bandwidths, Causal Forest models, and PDPs for a list of educational and labor outcomes.
* Saves each model as a `.rds` file per outcome.

### 5. `05_Model_Save_2015.R`

* Same as the previous script, but for the 2015 cohort.
* Follows the same modeling pipeline.

### 6. `06_Generate_Plots_2014.R`

* Loads models and data for 2014.
* Generates visualizations per outcome (3D HTE, surfaces, PDPs, etc.).
* Saves the plots as static or interactive files.

### 7. `07_Generate_Plots_2015.R`

* Same as above, applied to the 2015 cohort.

---

## 📦 Requirements

* R version ≥ 4.0
* Required packages:

  * `grf`, `tidyverse`, `ggplot2`, `haven`, `plotly`, `htmlwidgets`, `e1071`, `iml`, `patchwork`, `naniar`, `rdrobust`

---

## 📝 Additional Notes

* Some scripts may include commented local paths (e.g., `Sys.setenv(...)`) for Mac compatibility.
* It is recommended to run the scripts in order to reproduce results sequentially.
* Generated visualizations may vary depending on your local setup of `plotly` and `htmlwidgets`.

---

## 📬 Contact

This work was conducted by **Luis Zapata** as part of a research project with Professor Juliana Londoño-Vélez.
For more information or future collaboration, feel free to contact:
**Luis Zapata** · [luisjo819@g.ucla.edu](mailto:luisjo819@g.ucla.edu)
