# Policy-in-Action-2023-CAP-reform

This project analyzes the causal impact of the 2023 Common Agricultural Policy (CAP) reform on French farm demographics, specifically examining how the new "active farmer" eligibility criterion affected farm exit rates and gross standard output.

## Research Question

How did the 2023 CAP eligibility reform impact farm exit and gross standard output? Is the effect heterogeneous across regions?

## Data

We use administrative census data provided by the Ministry of Agriculture (Agreste), covering 389,779 French farms across two periods (2020 and 2023). The dataset includes:

- **Cross-sectional data** (`PiA_data.parquet`): 13 thematic categories with missing values preserved
- **Panel data** (`PiA_panel_data.parquet`): Balanced two-period panel (farm_id × year ∈ {2020, 2023}), with COEF2023 rescaling applied to Standard Gross Output, Livestock Units, and Labour composition variables

The complete case sample (farms with COEF2023 values) covers 48,003 farms and 21 variables including farm size, output, livestock, legal status, farming type, and geographic controls.

## Methodology
We employ five causal inference strategies on the same policy shock for robustness:

1. **Logistic Model** — Baseline prediction of exit probability using average marginal effects under the Conditional Independence Assumption.
2. **Difference-in-Differences (DiD)** — Two-period panel comparing treated vs. control farms on standardised gross output.
3. **Fuzzy Regression Discontinuity Design (RDD)** — Uses the age-67 cutoff as an instrument in a 2SLS framework.
4. **Difference-in-Discontinuities (Fuzzy)** — Combines DiD and RDD to difference out pre-existing discontinuities at the age cutoff, addressing covariate imbalance
5. **Double/Debiased Machine Learning (DML)** — IRM (ATE) and PLIV (LATE) using Random Forest nuisance learners with 5-fold cross-fitting. Anderson–Rubin weak-IV test confirms instrument strength.

## Project Structure
- `Regional Analysis/`
  - `Regional_Maps.R`: Summary statistics of farms at the regional level, including the point maps depicting the share of active farms by CAP status in 2020 and 2023.
- `Clustering/`
  - `K_Modes.R`: Clustering method to create profiles for farms that have lost CAP eligibility, with corresponding elbow plot to visualize the optimal number of clusters.
- `Logistic_Model/`
  - `Logit_AME.R`: Logistic regression estimating the impact of CAP eligibility loss on the probability of farm exit, with average marginal effects computed across multiple model specifications.
- `Difference-in-Differences/`
  - `DiD.R`: Main script implementing DiD estimations across multiple model specifications, including the generation of confidence interval plots.
  - `Regional_DiD.R`: Extension of the analysis at the regional level, with corresponding confidence interval visualizations.
- `RDD/`
  - `RDD_Fuzzy.R`: Fuzzy RDD using the age-67 cutoff as an instrument, with MSE-optimal bandwidth selection, estimating the effect of eligibility loss on farm exit and gross standard output.
  - `McCrary_Test.R`: Density test for the running variable (age) around the cutoff to assess the no-manipulation assumption.
  - `Covariate_Balance.R`: Balancing tests and bandwidth sensitivity analysis to assess the credibility of the RDD design.
- `Difference_in_Discontinuities/`
  - `DiDisc_Fuzzy.R`: Fuzzy Difference-in-Discontinuities combining RDD and DiD variation to difference out pre-existing discontinuities at the age cutoff, with confidence interval plots across model specifications.
- `DML/`
  - `DML_IRM.R`: Double/Debiased Machine Learning using the Interactive Regression Model to estimate the Average Treatment Effect (ATE) of eligibility loss on exit and gross standard output, with Random Forest learners and 5-fold cross-fitting.
  - `DML_PLIV.R`: DML Partially Linear IV model using OVER_67 as an instrument to estimate the Local Average Treatment Effect (LATE), with Anderson-Rubin weak-IV robust confidence sets.
  - `Regional_DML.R`: Regional heterogeneity analysis using both DML-IRM and DML-PLIV, with corresponding confidence interval visualizations.

## Key Findings

- Losing CAP eligibility increases farm exit probability by **+26 to +34 percentage points** across all identification strategies
- Farms that lost eligibility were on average **11 years older**, produced **48% less output**, and held **61% less land** than retained farms
- **Micro farms** and **elderly operators (67+)** are the most affected groups
- The effect on gross standard output is directionally negative (DiD estimate: −226k€) but causally uncertain in some specifications
- The treatment effect is **heterogeneous across regions**, with Centre-Val de Loire most severely affected
