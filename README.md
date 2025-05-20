# Replication Package – Causal Random Forest for SPP (Ser Pilo Paga)

This project estimates the **heterogeneous treatment effects (HTE)** of the *Ser Pilo Paga (SPP)* financial aid program on various outcomes such as university enrollment, graduation, and long-term labor market performance. *Ser Pilo Paga (SPP)*is a colombian student loan scheme, where the loan is forgiven as long as the student graduates from the program. The analysis focuses on how the effect of receiving the SPP scholarship varies across individuals with different baseline characteristics.

### Causal Identification Strategy

We follow the **potential outcomes** framework, where the treatment is receiving the SPP scholarship ($W_i = 1$), and the outcome ($Y_i$) can be education or labor-related.

We aim to estimate the **Conditional Average Treatment Effect (CATE)**:

$$\tau(X_i) = \mathbb{E}[Y_i(1) - Y_i(0) \mid X_i]$$

Where:
- $Y_i(1)$ and $Y_i(0)$ are the potential outcomes with and without treatment.
- $X_i$ is a vector of covariates including:
  - Socioeconomic score (SISBEN)
  - Academic achievement (Saber 11)
  - Household background and school context

This repository contains the full set of scripts used to prepare, estimate, and visualize heterogeneous treatment effect models of Colombia's *Ser Pilo Paga* (SPP) program for the 2014 and 2015 cohorts, using Causal Forests.

### Why use Causal Random Forest?

Causal Random Forest (CRF) is a modern machine learning method designed to estimate **heterogeneous treatment effects** — that is, how the impact of a policy or intervention varies across individuals with different characteristics.

In the context of *Ser Pilo Paga* (SPP), we are not just interested in the **average effect** of receiving the scholarship, but rather:
- Who benefits more or less from it?
- Does the effect vary by socioeconomic status or academic performance?

CRF is ideal because:
- It handles **high-dimensional covariates** without requiring manual variable selection.
- It automatically captures **nonlinear interactions** between variables.
- It produces **individual-level treatment effect estimates** with confidence intervals.
- It avoids overfitting by using **honest estimation** (splitting data between training and estimation sets).

---

### How does it work?

Causal Random Forest builds on the logic of traditional random forests, but is adapted for causal inference. The core idea is to estimate the **Conditional Average Treatment Effect (CATE)**:

$$\tau(X_i) = \mathbb{E}[Y_i(1) - Y_i(0) \mid X_i]$$

Steps (simplified):
1. **Input data**: Observed outcomes, treatment status, and covariates.
2. **Tree construction**: The algorithm grows many trees by recursively partitioning the data based on covariates that best explain differences in treatment effects.
3. **Honest estimation**: Separate samples are used for deciding the splits and estimating treatment effects, which avoids overfitting.
4. **Prediction**: For any individual, the model averages across all trees to predict their personalized treatment effect.

Causal Forest is implemented using the `grf` package in R (Generalized Random Forests).

---

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
