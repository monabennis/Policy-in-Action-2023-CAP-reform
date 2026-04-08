# Policy-in-Action-2023-CAP-reform

This project analyses the causal impact of the 2023 Common Agricultural Policy (CAP) reform on French farm demographics, specifically examining how the new "active farmer" eligibility criterion affected farm exit rates and gross standard output.

### Presentation
📄 [Download Presentation](https://raw.githubusercontent.com/monabennis/Policy-in-Action-2023-CAP-reform/main/Presentation/Policy_in_Action_Presentation.pdf)

## Research Question

How did the 2023 CAP eligibility reform impact farm exit and gross standard output? Is the effect heterogeneous across regions?

## Data

We use administrative census data provided by the Ministry of Agriculture (Agreste), covering 389,779 French farms across two periods (2020 and 2023). The dataset includes:

- **Cross-sectional data** (`PiA_data.parquet`): 13 thematic categories with missing values preserved
- **Panel data** (`PiA_panel_data.parquet`): Balanced two-period panel (farm_id × year ∈ {2020, 2023}), with COEF2023 rescaling applied to Standard Gross Output, Livestock Units, and Labour composition variables

The complete case sample (farms with COEF2023 values) covers 48,003 farms and 21 variables including farm size, output, livestock, legal status, farming type, and geographic controls.

## Key Findings

- Losing CAP eligibility increases farm exit probability by **+26 to +34 percentage points** across all identification strategies
- Farms that lost eligibility were on average **11 years older**, produced **48% less output**, and held **61% less land** than retained farms
- **Micro farms** and **elderly operators (67+)** are the most affected groups
- The effect on gross standard output is directionally negative (DiD estimate: −226k€) but causally uncertain in some specifications
- The treatment effect is **heterogeneous across regions**, with Centre-Val de Loire most severely affected

## Repository structure
```
Policy-in-Action-2023-CAP-reform/
│
├── PiA_data.parquet                        ← cross-sectional data (not in repo)
├── PiA_panel_data.parquet                  ← panel data (not in repo)
│
├── Regional Analysis/
│   └── Regional_Maps.R                     
│
├── Clustering/
│   └── K_Modes.R                          
│
├── Logit and RDD/
│   ├── PiA_Logit_and_RDD.R                
│   └── Validation and Balance Checks.R     
│
├── Difference-in-Differences/
│   ├── DiD.R                               
│   └── Regional_DiD.R                     
│
├── Difference-in-Discontinuities/
│   └── PiA_Diff_in_Disc.R                  
│
├── Double Machine Learning/
│   └── PiA_Double_ML.R                    
│
└── Presentation/
    └── Policy_in_Action_Presentation.pdf
```
## How to run
Each script is standalone — run them independently in any order, as long as the data files are in the root directory. Open the relevant .R file in RStudio and click Source, or run from the terminal with Rscript filename.R.   

For software requirements and data access, see Requirements.md.
For methodological details, see Methods.md.

## Authors 
Mona Bennis, Célina Madaschi, Alice Zanni 
