# three-way-decomposition-via-medflex
Reproducible R sample for three-way decomposition (PDE/PIE/MIE) via medflex and Natural Effect Models (NEM).

## Overview
This repository provides a tutorial for performing **three-way decomposition**—Pure Direct Effect (PDE), Pure Indirect Effect (PIE), and Mediated Interaction (MIE)—using **Natural Effect Models (NEM)** via the R package **medflex**.
It includes a worked example with the UPBdata dataset and two applied cases, illustrating how to estimate and visualize decomposed effects under various exposure, mediator, and outcome types.

## File structure
- R/upbdata.R — Full UPBdata example covering:
  - continuous, binary, and categorical exposure/mediator combinations
  - Effect modification by covariates
  - Joint mediation example
  - Marginal effect estimates calculation
- R/case1.R — Case 1: KLoSA dataset preparation + analysis combined in one script, estimating PDE/PIE/MIE and producing summary plots/tables.
- R/case2_prep.R — Case 2: Preprocessing for the Vital Statistics 2014 dataset (variable extraction, cleaning, recoding, filtering).
- R/case2_analysis.R — Case 2: Analysis using prepared data, fitting NEM, generating decomposition results, and saving outputs.
> Note: Scripts contain `setwd()` calls. For portability, replace with relative paths (e.g., `here::here()`).

## Requirements
R ≥ 4.2
Required packages:
<pre> install.packages(c(
  "medflex","dplyr","ggplot2","readxl","haven","readr","tidyr","data.table","broom"
  # optional
  # "here"
)) </pre>

## Quick start
<pre># UPBdata example: includes joint mediation + marginal estimates
source("R/upbdata.R")

# Case 1: Decomposing the Mediating Role of Chronic Conditions in the Relationship between Medicaid Status and Healthcare Expenditure
source("R/case1.R")

# Case 2. Decomposing the Mediating Role of Smoking in the Relationship between Maternal Race/Ethnicity and Preterm Birth
source("R/case2_prep.R")
source("R/case2_analysis.R")
</pre>

## Tips
- `neImpute()` and `neModel()` are sensitive to variable types (factor vs. continuous).
  Double-check variable encoding before running.
- For bootstrap standard errors, set `se="bootstrap"` and fix the seed with `set.seed()` (note: longer runtime).
- For large raw text files (e.g., VS 2014), adjust file paths and encoding to match your environment.

## License
- **Code (R scripts):** MIT License © 2025 Your Name
- **Docs** (README, images): CC BY 4.0

Please retain attribution when reusing.
