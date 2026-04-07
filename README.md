# Policy-in-Action-2023-CAP-reform

This project analyzes the impact of the 2023 Common Agricultural Policy (CAP) reform on French farms.

## Data 
We use data provided by the 

## Methodology
We employ causal inference methods to estimate the effect of losing CAP eligibility on gross standard output. Namely Difference-in-Differences, ... 

## Project Structure
- `Regional Analysis/`
  - `Regional_Maps.R`: Summary statistics of farms at the regional level, including the point maps depicting the share of active farms.
- `Clustering/`
  - `K_Modes.R`: Clustering method to create profiles for farms that have lost CAP eligibility, with corresponding elbow plot to visualize the optimal number of clusters. 
- `Difference-in-Differences/`
  - `DiD.R`: Main script implementing DiD estimations across multiple model specifications, including the generation of confidence interval plots.
  - `Regional_DiD.R`: Extension of the analysis at the regional level, with corresponding confidence interval visualizations.

- 
