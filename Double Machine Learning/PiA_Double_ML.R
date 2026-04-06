#Load libraries
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(ranger)
library(dplyr)
library(tidyverse)
library(haven)
library(arrow)
library(ggplot2)
library(margins)
library(rdrobust)
library(sandwich)
library(lmtest)
library(fixest)
library(arrow)

#========================= Preparation of the analyses =========================

# 1. Load Cleaned Data
path_data <- "francetransfert-1325119685"
file <- file.path(path_data, "PiA_panel_clean_coef.parquet")
panel_data <- read_parquet(file)

# 2. Construct Treatment
panel_data <- panel_data %>%
  filter(!is.na(BENEF_PAC)) %>%
  group_by(farm_id) %>%
  mutate(
    elig_2020 = BENEF_PAC[year == 2020],
    elig_2023 = BENEF_PAC[year == 2023],
    treated = as.integer(year == 2023 & elig_2020 == 1 & elig_2023 == 0)
  ) %>%
  ungroup()

panel_data <- panel_data %>%
  mutate(Post = if_else(year == 2023, 1, 0))

# 3. First-difference
panel_diff <- panel_data %>%
  group_by(farm_id) %>%
  summarize(
    dY = PBSTOT_COEF17[year == 2023] - PBSTOT_COEF17[year == 2020],
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG),
           ~ .[year == 2020])
  )

# 4. One-hot encode categorical variables
# Ensure they are a factor
panel_diff$LABOUR_TYPE <- factor(panel_diff$LABOUR_TYPE)
panel_diff$STATUT <- factor(panel_diff$STATUT)
panel_diff$OTEFDA_COEF17 <- factor(panel_diff$OTEFDA_COEF17)

# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = panel_diff)

# Bind back to dataset
panel_diff <- panel_diff %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

panel_diff_dt <- as.data.table(panel_diff)

# 5. Create a function to fit a Double ML PLIV model
estimate_dml_iv <- function(data, y_col, d_col, x_cols, z_cols, cluster_cols,
                            n_folds = 5, seed = 2222) {
  
  # a. Initialize the DoubleML Cluster Data Object
  # This handles clustering (e.g., farm_id) and the IV structure
  obj_dml_data <- DoubleMLClusterData$new(
    data,
    y_col = y_col,
    d_col = d_col,
    x_cols = x_cols,
    z_cols = z_cols,
    cluster_cols = cluster_cols
  )
  
  # b. Define the Machine Learning Learners (using mlr3)
  # ml_l: regression of Y on X
  # ml_m: regression of Z on X
  # ml_r: regression of D on X
  # We use Random Forest by default as it's robust for DML
  ml_l <- lrn("regr.ranger", num.trees = 500, mtry = floor(sqrt(length(x_cols))), min.node.size = 2, max.depth = 5)
  ml_m <- lrn("regr.ranger", num.trees = 500, mtry = floor(sqrt(length(x_cols))), min.node.size = 2, max.depth = 5)
  ml_r <- lrn("regr.ranger", num.trees = 500, mtry = floor(sqrt(length(x_cols))), min.node.size = 2, max.depth = 5)
  
  # c. Initialize the Partially Linear IV (PLIV) Model
  set.seed(seed)
  dml_iv_model <- DoubleMLPLIV$new(
    obj_dml_data,
    ml_l = ml_l,
    ml_m = ml_m,
    ml_r = ml_r,
    n_folds = n_folds,
    apply_cross_fitting = TRUE
  )
  
  # d. Fit the model
  message("Fitting DoubleML PLIV model...")
  dml_iv_model$fit()
  
  return(dml_iv_model)
}

# 6. Create a function to fit a Double ML Interactive Regression Model
estimate_dml_irm <- function(data, y_col, d_col, x_cols, cluster_cols,
                             n_folds = 5, seed = 2222) {
  
  # a. Initialize the DoubleML Cluster Data Object
  obj_dml_data <- DoubleMLClusterData$new(
    data,
    y_col = y_col,
    d_col = d_col,
    x_cols = x_cols,
    cluster_cols = cluster_cols
  )
  
  # b. Define the Machine Learning Learners (mlr3)
  # ml_g: E[Y|D,X] - nuisance model for the outcome (regression)
  # ml_m: E[D|X]   - nuisance model for the treatment (classification / propensity score)
  ml_g <- lrn("regr.ranger",   num.trees = 500, mtry = floor(sqrt(length(x_cols))), min.node.size = 2, max.depth = 5)
  ml_m <- lrn("classif.ranger", num.trees = 500, mtry = floor(sqrt(length(x_cols))), min.node.size = 2, max.depth = 5)
  
  # c. Initialize the Interactive Regression Model (IRM)
  set.seed(seed)
  dml_irm_model <- DoubleMLIRM$new(
    obj_dml_data,
    ml_g = ml_g,
    ml_m = ml_m,
    n_folds = n_folds,
    apply_cross_fitting = TRUE
  )
  
  # d. Fit the model
  message("Fitting DoubleML IRM model...")
  dml_irm_model$fit()
  
  return(dml_irm_model)
}


# ============================================================
# Double ML PLIV and Double ML IRM
# ============================================================

#################################################
####### Treatment effect (national level) #######
#######               PBS                 #######
#################################################

#================== Estimation DML PLIV and full sample (PBS) ==================

# 1. Create vector of covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(panel_diff), value = TRUE),
           grep("^STATUT", names(panel_diff), value = TRUE),
           grep("^OTEFDA", names(panel_diff), value = TRUE))

# 2. Fit the function
dml_pbs_iv <- estimate_dml_iv(
  data = panel_diff_dt,
  y_col = "dY",
  d_col = "dD",
  x_cols = x_cols,
  z_cols = "OVER_67",
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_pbs_iv$summary())
dml_pbs_iv$coef
dml_pbs_iv$se

#==================== Estimation DML IRM and full sample (PBS) =================

# 1. Create a vector of covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(panel_diff), value = TRUE),
           grep("^STATUT", names(panel_diff), value = TRUE),
           grep("^OTEFDA", names(panel_diff), value = TRUE))

obj_dml_data = DoubleMLClusterData$new(panel_diff_dt,
                                       y_col = "dY",
                                       d_col = "dD",
                                       x_cols = x_cols,
                                       cluster_cols = "farm_id")

# 2. Fit the Model
dml_pbs <- estimate_dml_irm(
  data = panel_diff_dt,
  y_col = "dY",
  d_col = "dD",
  x_cols = x_cols,
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_pbs)
dml_pbs$summary()
dml_pbs$coef
dml_pbs$se

#============= Estimation DML PLIV and BENEF_PAC = 1 in 2020 (PBS) =============

# 1. Prepare dataset and covariates
data_restricted <- panel_data %>%
  group_by(farm_id) %>%
  filter(any(year == 2020 & BENEF_PAC == 1)) %>%
  summarize(
    dY = PBSTOT_COEF17[year == 2023] - PBSTOT_COEF17[year == 2020],
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG,
             DIMECO_COEF2017),
           ~ .[year == 2020])
  )

# One-hot encode categorical variables
# Ensure they are a factor
data_restricted$LABOUR_TYPE <- factor(data_restricted$LABOUR_TYPE)
data_restricted$STATUT <- factor(data_restricted$STATUT)
data_restricted$OTEFDA_COEF17 <- factor(data_restricted$OTEFDA_COEF17)
# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = data_restricted)
# Bind back to dataset
data_restricted <- data_restricted %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

data_restricted <- as.data.table(data_restricted)

# Covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(data_restricted), value = TRUE),
           grep("^STATUT", names(data_restricted), value = TRUE),
           grep("^OTEFDA", names(data_restricted), value = TRUE))

# 2. Fit the function
dml_pbs_iv_rest <- estimate_dml_iv(
  data = data_restricted,
  y_col = "dY",
  d_col = "dD",
  x_cols = x_cols,
  z_cols = "OVER_67",
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_pbs_iv_rest$summary())
dml_pbs_iv_rest$coef
dml_pbs_iv_rest$se

#============== Estimation DML IRM and BENEF_PAC = 1 in 2020 (PBS) =============

# 1. Prepare dataset and covariates
data_restricted <- panel_data %>%
  group_by(farm_id) %>%
  filter(any(year == 2020 & BENEF_PAC == 1)) %>%
  summarize(
    dY = PBSTOT_COEF17[year == 2023] - PBSTOT_COEF17[year == 2020],
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG,
             DIMECO_COEF2017),
           ~ .[year == 2020])
  )

# One-hot encode categorical variables
# Ensure they are a factor
data_restricted$LABOUR_TYPE <- factor(data_restricted$LABOUR_TYPE)
data_restricted$STATUT <- factor(data_restricted$STATUT)
data_restricted$OTEFDA_COEF17 <- factor(data_restricted$OTEFDA_COEF17)
# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = data_restricted)
# Bind back to dataset
data_restricted <- data_restricted %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

data_restricted <- as.data.table(data_restricted)

# Covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(data_restricted), value = TRUE),
           grep("^STATUT", names(data_restricted), value = TRUE),
           grep("^OTEFDA", names(data_restricted), value = TRUE))

# 2. Fit the function
dml_pbs_rest <- estimate_dml_irm(
  data = data_restricted,
  y_col = "dY",
  d_col = "dD",
  x_cols = x_cols,
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_pbs_rest$summary())
dml_pbs_rest$coef
dml_pbs_rest$se

#=============== Estimation DML PLIV and exclude micro farms (PBS) =============

# 1. Prepare dataset and covariates
data_exclusion <- panel_data %>%
  group_by(farm_id) %>%
  mutate(IS_MICRO = if_else(DIMECO_COEF2017 == "1-micros", 1, 0)) %>%
  filter(IS_MICRO == 0) %>%
  summarize(
    dY = PBSTOT_COEF17[year == 2023] - PBSTOT_COEF17[year == 2020],
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG,
             DIMECO_COEF2017),
           ~ .[year == 2020])
  )

# One-hot encode categorical variables
# Ensure they are a factor
data_exclusion$LABOUR_TYPE <- factor(data_exclusion$LABOUR_TYPE)
data_exclusion$STATUT <- factor(data_exclusion$STATUT)
data_exclusion$OTEFDA_COEF17 <- factor(data_exclusion$OTEFDA_COEF17)
# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = data_exclusion)
# Bind back to dataset
data_exclusion <- data_exclusion %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

data_exclusion <- as.data.table(data_exclusion)

# Covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(data_exclusion), value = TRUE),
           grep("^STATUT", names(data_exclusion), value = TRUE),
           grep("^OTEFDA", names(data_exclusion), value = TRUE))

# 2. Fit the function
dml_pbs_iv_excl <- estimate_dml_iv(
  data = data_exclusion,
  y_col = "dY",
  d_col = "dD",
  x_cols = x_cols,
  z_cols = "OVER_67",
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_pbs_iv_excl$summary())
dml_pbs_iv_excl$coef
dml_pbs_iv_excl$se

#=============== Estimation DML IRM and exclude micro farms (PBS) ==============

# 1. Prepare dataset
data_exclusion <- panel_data %>%
  group_by(farm_id) %>%
  mutate(IS_MICRO = if_else(DIMECO_COEF2017 == "1-micros", 1, 0)) %>%
  filter(IS_MICRO == 0) %>%
  summarize(
    dY = PBSTOT_COEF17[year == 2023] - PBSTOT_COEF17[year == 2020],
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG,
             DIMECO_COEF2017),
           ~ .[year == 2020])
  )

# One-hot encode categorical variables
# Ensure they are a factor
data_exclusion$LABOUR_TYPE <- factor(data_exclusion$LABOUR_TYPE)
data_exclusion$STATUT <- factor(data_exclusion$STATUT)
data_exclusion$OTEFDA_COEF17 <- factor(data_exclusion$OTEFDA_COEF17)

# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = data_exclusion)

# Bind back to dataset
data_exclusion <- data_exclusion %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

data_exclusion <- as.data.table(data_exclusion)

# 2. Create DoubleML Data Object
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(data_exclusion), value = TRUE),
           grep("^STATUT", names(data_exclusion), value = TRUE),
           grep("^OTEFDA", names(data_exclusion), value = TRUE))

# 2. Fit the function
dml_pbs_excl <- estimate_dml_irm(
  data = data_exclusion,
  y_col = "dY",
  d_col = "dD",
  x_cols = x_cols,
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_pbs_excl$summary())
dml_pbs_excl$coef
dml_pbs_excl$se


#============== Comparative Plot of different Double ML Estimates ==============

# 1. Put your DoubleML objects into the named list
models_list <- list(
  "Double ML PLIV (full)"                = dml_pbs_iv,
  "Double ML IRM (full)"                 = dml_pbs,
  "Double ML PLIV (BENEF_PAC=1 in 2020)" = dml_pbs_iv_rest,
  "Double ML IRM (BENEF_PAC=1 in 2020)"  = dml_pbs_rest,
  "Double ML PLIV (exclude micro)"       = dml_pbs_iv_excl,
  "Double ML IRM (exclude micro)"        = dml_pbs_excl
)

# 2. Extract results using the DoubleML summary structure
dml_specs <- do.call(rbind, lapply(names(models_list), function(spec_name) {
  
  m <- models_list[[spec_name]]
  
  # DoubleML summary() returns a matrix/table where row 1 is the treatment effect
  # Columns: "estimate", "std_err", "t_stat", "p_val", "2.5 %", "97.5 %"
  res_summary <- m$summary()
  
  data.frame(
    spec     = spec_name,
    estimate = res_summary[1, "Estimate."],
    se       = res_summary[1, "Std. Error"],
    ci_low   = res_summary[1, "Estimate."] - 1.96 * res_summary[1, "Std. Error"],
    ci_high  = res_summary[1, "Estimate."] + 1.96 * res_summary[1, "Std. Error"]
  )
}))

# 3. Process significance and factors
dml_specs <- dml_specs %>%
  mutate(
    significant = ifelse(ci_low > 0 | ci_high < 0,
                         "Significant (5%)",
                         "Not significant"),
    spec = factor(spec, levels = names(models_list))
  )

# 4. Generate the Visualization
plot_dml_specs <- ggplot(dml_specs, aes(x = spec, y = estimate)) +
  # Reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "grey40") +
  
  # Error bars using DoubleML's own calculated CIs
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  
  # Point estimates
  geom_point(aes(color = significant), size = 3.5) +
  
  # Visual formatting
  coord_flip() +
  scale_x_discrete(limits = rev(names(models_list))) +
  scale_y_continuous(labels = scales::label_comma()) + # Use comma for production levels
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  
  labs(
    x = "Model specification",
    y = "Treatment Effect (Standard Gross Output)",
    title = "Double Machine Learning Estimates Across Specifications",
    subtitle = "Comparing Interactive Regression Model (IRM) and IV (PLIV) Results",
    color = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# View plot
print(plot_dml_specs)









#################################################
####### Treatment effect (national level) #######
#######               Exit                #######
#################################################

#==================== Setting DML function for binary outcome ==================

# Create a function to fit a Double ML Interactive Regression Model
estimate_dml_irm2 <- function(data, y_col, d_col, x_cols, cluster_cols,
                              n_folds = 5, seed = 2222) {
  
  # a. Initialize the DoubleML Cluster Data Object
  obj_dml_data <- DoubleMLClusterData$new(
    data,
    y_col = y_col,
    d_col = d_col,
    x_cols = x_cols,
    cluster_cols = cluster_cols
  )
  
  # b. Define the Machine Learning Learners (mlr3)
  # ml_g: E[Y|D,X] - nuisance model for the outcome (regression)
  # ml_m: E[D|X]   - nuisance model for the treatment (classification / propensity score)
  ml_g <- lrn("classif.ranger",   num.trees = 500, mtry = floor(sqrt(length(x_cols))), min.node.size = 2, max.depth = 5)
  ml_m <- lrn("classif.ranger", num.trees = 500, mtry = floor(sqrt(length(x_cols))), min.node.size = 2, max.depth = 5)
  
  # c. Initialize the Interactive Regression Model (IRM)
  set.seed(seed)
  dml_irm_model <- DoubleMLIRM$new(
    obj_dml_data,
    ml_g = ml_g,
    ml_m = ml_m,
    n_folds = n_folds,
    apply_cross_fitting = TRUE
  )
  
  # d. Fit the model
  message("Fitting DoubleML IRM model...")
  dml_irm_model$fit()
  
  return(dml_irm_model)
}

#================= Estimation DML PLIV and full sample (Exit) ==================

# 1. Create vector of covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           "PBSTOT_COEF17",
           grep("^LABOUR_TYPE", names(panel_diff), value = TRUE),
           grep("^STATUT", names(panel_diff), value = TRUE),
           grep("^OTEFDA", names(panel_diff), value = TRUE))

# 2. Fit the function
dml_exit_iv <- estimate_dml_iv(
  data = panel_diff_dt,
  y_col = "EXIT",
  d_col = "dD",
  x_cols = x_cols,
  z_cols = "OVER_67",
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_exit_iv$summary())
dml_exit_iv$coef
dml_exit_iv$se


#=================== Estimation DML IRM and full sample (Exit) =================

# 1. Create vector of covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           "PBSTOT_COEF17",
           grep("^LABOUR_TYPE", names(panel_diff), value = TRUE),
           grep("^STATUT", names(panel_diff), value = TRUE),
           grep("^OTEFDA", names(panel_diff), value = TRUE))

# 2. Fit the function
dml_exit <- estimate_dml_irm2(
  data = panel_diff_dt,
  y_col = "EXIT",
  d_col = "dD",
  x_cols = x_cols,
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_exit$summary())
dml_exit$coef
dml_exit$se
#============= Estimation DML PLIV and BENEF_PAC = 1 in 2020 (Exit) ============

# 1. Prepare dataset and covariates
data_restricted <- panel_data %>%
  group_by(farm_id) %>%
  filter(any(year == 2020 & BENEF_PAC == 1)) %>%
  summarize(
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG,
             DIMECO_COEF2017),
           ~ .[year == 2020])
  )

# One-hot encode categorical variables
# Ensure they are a factor
data_restricted$LABOUR_TYPE <- factor(data_restricted$LABOUR_TYPE)
data_restricted$STATUT <- factor(data_restricted$STATUT)
data_restricted$OTEFDA_COEF17 <- factor(data_restricted$OTEFDA_COEF17)
# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = data_restricted)
# Bind back to dataset
data_restricted <- data_restricted %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

data_restricted <- as.data.table(data_restricted)

# Covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           "PBSTOT_COEF17",
           grep("^LABOUR_TYPE", names(data_restricted), value = TRUE),
           grep("^STATUT", names(data_restricted), value = TRUE),
           grep("^OTEFDA", names(data_restricted), value = TRUE))

# 2. Fit the function
dml_exit_iv_rest <- estimate_dml_iv(
  data = data_restricted,
  y_col = "EXIT",
  d_col = "dD",
  x_cols = x_cols,
  z_cols = "OVER_67",
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_exit_iv_rest$summary())
dml_exit_iv_rest$coef
dml_exit_iv_rest$se

#============= Estimation DML IRM and BENEF_PAC = 1 in 2020 (Exit) =============

# 1. Prepare dataset and covariates
data_restricted <- panel_data %>%
  group_by(farm_id) %>%
  filter(any(year == 2020 & BENEF_PAC == 1)) %>%
  summarize(
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG,
             DIMECO_COEF2017),
           ~ .[year == 2020])
  )

# One-hot encode categorical variables
# Ensure they are a factor
data_restricted$LABOUR_TYPE <- factor(data_restricted$LABOUR_TYPE)
data_restricted$STATUT <- factor(data_restricted$STATUT)
data_restricted$OTEFDA_COEF17 <- factor(data_restricted$OTEFDA_COEF17)
# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = data_restricted)
# Bind back to dataset
data_restricted <- data_restricted %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

data_restricted <- as.data.table(data_restricted)

# Covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           "PBSTOT_COEF17",
           grep("^LABOUR_TYPE", names(data_restricted), value = TRUE),
           grep("^STATUT", names(data_restricted), value = TRUE),
           grep("^OTEFDA", names(data_restricted), value = TRUE))

# 2. Fit the function
dml_exit_rest <- estimate_dml_irm2(
  data = data_restricted,
  y_col = "EXIT",
  d_col = "dD",
  x_cols = x_cols,
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_exit_rest$summary())
dml_exit_rest$coef
dml_exit_rest$se

#============== Estimation DML PLIV and exclude micro farms (Exit) =============

# 1. Prepare dataset and covariates
data_exclusion <- panel_data %>%
  group_by(farm_id) %>%
  mutate(IS_MICRO = if_else(DIMECO_COEF2017 == "1-micros", 1, 0)) %>%
  filter(IS_MICRO == 0) %>%
  summarize(
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG,
             DIMECO_COEF2017),
           ~ .[year == 2020])
  )

# One-hot encode categorical variables
# Ensure they are a factor
data_exclusion$LABOUR_TYPE <- factor(data_exclusion$LABOUR_TYPE)
data_exclusion$STATUT <- factor(data_exclusion$STATUT)
data_exclusion$OTEFDA_COEF17 <- factor(data_exclusion$OTEFDA_COEF17)
# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = data_exclusion)
# Bind back to dataset
data_exclusion <- data_exclusion %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

data_exclusion <- as.data.table(data_exclusion)

# Covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           "PBSTOT_COEF17",
           grep("^LABOUR_TYPE", names(data_exclusion), value = TRUE),
           grep("^STATUT", names(data_exclusion), value = TRUE),
           grep("^OTEFDA", names(data_exclusion), value = TRUE))

# 2. Fit the function
dml_exit_iv_excl <- estimate_dml_iv(
  data = data_exclusion,
  y_col = "EXIT",
  d_col = "dD",
  x_cols = x_cols,
  z_cols = "OVER_67",
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_exit_iv_excl$summary())
dml_exit_iv_excl$coef
dml_exit_iv_excl$se

#=============== Estimation DML IRM and exclude micro farms (PBS) ==============

# 1. Prepare dataset
data_exclusion <- panel_data %>%
  group_by(farm_id) %>%
  mutate(IS_MICRO = if_else(DIMECO_COEF2017 == "1-micros", 1, 0)) %>%
  filter(IS_MICRO == 0) %>%
  summarize(
    dY = PBSTOT_COEF17[year == 2023] - PBSTOT_COEF17[year == 2020],
    dD = treated[year == 2023] - treated[year == 2020],
    OVER_67 = OVER_67[year == 2023],
    EXIT = EXIT[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, SIEGE_LIB_REG,
             DIMECO_COEF2017),
           ~ .[year == 2020])
  )

# One-hot encode categorical variables
# Ensure they are a factor
data_exclusion$LABOUR_TYPE <- factor(data_exclusion$LABOUR_TYPE)
data_exclusion$STATUT <- factor(data_exclusion$STATUT)
data_exclusion$OTEFDA_COEF17 <- factor(data_exclusion$OTEFDA_COEF17)
# One-hot encode (drops reference category automatically)
dummies <- model.matrix(~ LABOUR_TYPE - 1 + STATUT - 1 + OTEFDA_COEF17 - 1, data = data_exclusion)
# Bind back to dataset
data_exclusion <- data_exclusion %>%
  cbind(dummies) %>%
  select(-LABOUR_TYPE, -STATUT, - OTEFDA_COEF17)

data_exclusion <- as.data.table(data_exclusion)

# Covariates
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           "PBSTOT_COEF17",
           grep("^LABOUR_TYPE", names(data_exclusion), value = TRUE),
           grep("^STATUT", names(data_exclusion), value = TRUE),
           grep("^OTEFDA", names(data_exclusion), value = TRUE))

# 2. Fit the function
dml_exit_excl <- estimate_dml_irm2(
  data = data_exclusion,
  y_col = "EXIT",
  d_col = "dD",
  x_cols = x_cols,
  cluster_cols = "farm_id"
)

# 3. View the results
print(dml_exit_excl$summary())
dml_exit_excl$coef
dml_exit_excl$se



#============== Comparative Plot of different Double ML Estimates ==============

# 1. Put your DoubleML objects into the named list
models_list <- list(
  "Double ML PLIV (full)"                = dml_exit_iv,
  "Double ML IRM (full)"                 = dml_exit,
  "Double ML PLIV (BENEF_PAC=1 in 2020)" = dml_exit_iv_rest,
  "Double ML IRM (BENEF_PAC=1 in 2020)"  = dml_exit_rest,
  "Double ML PLIV (exclude micro)"       = dml_exit_iv_excl,
  "Double ML IRM (exclude micro)"        = dml_exit_excl
)

# 2. Extract results using the DoubleML summary structure
dml_specs <- do.call(rbind, lapply(names(models_list), function(spec_name) {
  
  m <- models_list[[spec_name]]
  
  # DoubleML summary() returns a matrix/table where row 1 is the treatment effect
  # Columns: "estimate", "std_err", "t_stat", "p_val", "2.5 %", "97.5 %"
  res_summary <- m$summary()
  
  data.frame(
    spec     = spec_name,
    estimate = res_summary[1, "Estimate."],
    se       = res_summary[1, "Std. Error"],
    ci_low   = res_summary[1, "Estimate."] - 1.96 * res_summary[1, "Std. Error"],
    ci_high  = res_summary[1, "Estimate."] + 1.96 * res_summary[1, "Std. Error"]
  )
}))

# 3. Process significance and factors
dml_specs <- dml_specs %>%
  mutate(
    significant = ifelse(ci_low > 0 | ci_high < 0,
                         "Significant (5%)",
                         "Not significant"),
    spec = factor(spec, levels = names(models_list))
  )

# 4. Generate the Visualization
plot_dml_specs <- ggplot(dml_specs, aes(x = spec, y = estimate)) +
  # Reference line at 0
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "grey40") +
  
  # Error bars using DoubleML's own calculated CIs
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  
  # Point estimates
  geom_point(aes(color = significant), size = 3.5) +
  
  # Visual formatting
  coord_flip() +
  scale_x_discrete(limits = rev(names(models_list))) +
  scale_y_continuous(labels = scales::label_comma()) + # Use comma for production levels
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  
  labs(
    x = "Model specification",
    y = "Treatment Effect (Probability of Exit)",
    title = "Double Machine Learning Estimates Across Specifications",
    subtitle = "Comparing Interactive Regression Model (IRM) and IV (PLIV) Results",
    color = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# View plot
print(plot_dml_specs)






################################################
#######    Treatment effect by region    #######
################################################

#========= Plot of average treatment effect by region (DML PLIV - PBS) =========

x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(panel_diff), value = TRUE),
           grep("^STATUT", names(panel_diff), value = TRUE),
           grep("^OTEFDA", names(panel_diff), value = TRUE))

regions <- unique(panel_diff_dt$SIEGE_LIB_REG)
regions <- regions[!is.na(regions)]

pliv_models <- list()

for (reg in regions) {
  
  region_data <- panel_diff_dt[panel_diff_dt$SIEGE_LIB_REG == reg, ]
  
  pliv_models[[reg]] <- estimate_dml_iv(
    data = region_data,
    y_col = "dY",
    d_col = "dD",
    x_cols = x_cols,
    z_cols = "OVER_67",
    cluster_cols = "farm_id"
  )
}

pliv_plot_data <- do.call(rbind, lapply(names(pliv_models), function(name) {
  
  m <- pliv_models[[name]]
  
  est <- m$coef
  se  <- m$se
  
  data.frame(
    spec = name,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se
  )
}))

pliv_plot_data$significant <- ifelse(
  pliv_plot_data$ci_low > 0 | pliv_plot_data$ci_high < 0,
  "Significant (5%)", "Not significant"
)

ggplot(pliv_plot_data, aes(x = reorder(spec, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Region",
    y = "Treatment Effect on Gross Standard Output",
    title = "DoubleML PLIV Estimates by Region",
    color = NULL
  ) +
  theme_minimal(base_size = 14)


#========== Plot of average treatment effect by region (DML IRM - PBS) =========

irm_models <- list()

for (reg in regions) {
  
  region_data <- panel_diff_dt[panel_diff_dt$SIEGE_LIB_REG == reg, ]
  
  irm_models[[reg]] <- estimate_dml_irm(
    data = region_data,
    y_col = "dY",
    d_col = "dD",
    x_cols = x_cols,
    cluster_cols = "farm_id"
  )
}

irm_plot_data <- do.call(rbind, lapply(names(irm_models), function(name) {
  
  m <- irm_models[[name]]
  
  est <- m$coef
  se  <- m$se
  
  data.frame(
    spec = name,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se
  )
}))

irm_plot_data$significant <- ifelse(
  irm_plot_data$ci_low > 0 | irm_plot_data$ci_high < 0,
  "Significant (5%)", "Not significant"
)

ggplot(irm_plot_data, aes(x = reorder(spec, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Region",
    y = "Treatment Effect on Gross Standard Output",
    title = "DoubleML IRM Estimates by Region",
    color = NULL
  ) +
  theme_minimal(base_size = 14)
#========= Plot of average treatment effect by region (DML PLIV - Exit) ========

x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(panel_diff), value = TRUE),
           grep("^STATUT", names(panel_diff), value = TRUE),
           grep("^OTEFDA", names(panel_diff), value = TRUE))

regions <- unique(panel_diff_dt$SIEGE_LIB_REG)
regions <- regions[!is.na(regions)]

pliv_models <- list()

for (reg in regions) {
  
  region_data <- panel_diff_dt[panel_diff_dt$SIEGE_LIB_REG == reg, ]
  
  pliv_models[[reg]] <- estimate_dml_iv(
    data = region_data,
    y_col = "EXIT",
    d_col = "dD",
    x_cols = x_cols,
    z_cols = "OVER_67",
    cluster_cols = "farm_id"
  )
}

pliv_plot_data <- do.call(rbind, lapply(names(pliv_models), function(name) {
  
  m <- pliv_models[[name]]
  
  est <- m$coef
  se  <- m$se
  
  data.frame(
    spec = name,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se
  )
}))

pliv_plot_data$significant <- ifelse(
  pliv_plot_data$ci_low > 0 | pliv_plot_data$ci_high < 0,
  "Significant (5%)", "Not significant"
)

ggplot(pliv_plot_data, aes(x = reorder(spec, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3.5) +
  coord_flip() +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Region",
    y = "Treatment Effect on Probability of Exit",
    title = "DoubleML PLIV Estimates by Region",
    color = NULL
  ) +
  theme_minimal(base_size = 14)


#========= Plot of average treatment effect by region (DML IRM - Exit) =========

irm_models <- list()

for (reg in regions) {
  
  region_data <- panel_diff_dt[panel_diff_dt$SIEGE_LIB_REG == reg, ]
  
  irm_models[[reg]] <- estimate_dml_irm2(
    data = region_data,
    y_col = "EXIT",
    d_col = "dD",
    x_cols = x_cols,
    cluster_cols = "farm_id"
  )
}

irm_plot_data <- do.call(rbind, lapply(names(irm_models), function(name) {
  
  m <- irm_models[[name]]
  
  est <- m$coef
  se  <- m$se
  
  data.frame(
    spec = name,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se
  )
}))

irm_plot_data$significant <- ifelse(
  irm_plot_data$ci_low > 0 | irm_plot_data$ci_high < 0,
  "Significant (5%)", "Not significant"
)

ggplot(irm_plot_data, aes(x = reorder(spec, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3.5) +
  coord_flip() +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Region",
    y = "Treatment Effect on Probability of Exit",
    title = "DoubleML IRM Estimates by Region",
    color = NULL
  ) +
  theme_minimal(base_size = 14)




# ============================================================
# Double ML with Anderson-Rubin Inference (Weak IV Robust)
# ============================================================

##############################################################
library(AER)  # for ivreg, used to replicate tsls

# 1. TSLS helper
tsls_simple <- function(ytil, dtil, ztil) {
  iv_data <- data.frame(ytil = ytil, dtil = dtil, ztil = ztil)
  fit <- ivreg(ytil ~ dtil - 1 | ztil - 1, data = iv_data)
  coef_est <- coef(fit)[["dtil"]]
  se <- sqrt(diag(vcov(fit)))[["dtil"]]
  return(list(coef_est = coef_est, se = se))
}

# 2. DML2 for PLIV (manually computes the cross-fitted residuals)
dml2_for_plivm <- function(x, d, z, y, dreg, yreg, zreg, nfold = 5) {
  
  nobs   <- nrow(x)
  foldid <- rep.int(1:nfold, times = ceiling(nobs / nfold))[sample.int(nobs)]
  I      <- split(1:nobs, foldid)
  
  ytil <- dtil <- ztil <- rep(NA, nobs)
  
  cat("fold: ")
  for (b in seq_along(I)) {
    
    dfit <- dreg(x[-I[[b]], ], d[-I[[b]]])
    zfit <- zreg(x[-I[[b]], ], z[-I[[b]]])
    yfit <- yreg(x[-I[[b]], ], y[-I[[b]]])
    
    dtil[I[[b]]] <- d[I[[b]]] - predict(dfit, data.frame(x[I[[b]], ]))$predictions
    ztil[I[[b]]] <- z[I[[b]]] - predict(zfit, data.frame(x[I[[b]], ]))$predictions
    ytil[I[[b]]] <- y[I[[b]]] - predict(yfit, data.frame(x[I[[b]], ]))$predictions
    
    cat(b, " ")
  }
  
  ivfit    <- tsls_simple(ytil, dtil, ztil)
  coef_est <- ivfit$coef_est
  se       <- ivfit$se
  
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef_est, se))
  return(list(coef_est = coef_est, se = se,
              dtil = dtil, ytil = ytil, ztil = ztil))
}

# 3. DML-AR (Anderson-Rubin)
dml_ar_pliv <- function(rY, rD, rZ, grid, alpha = 0.05) {
  
  n     <- length(rY)
  Cstat <- rep(0, length(grid))
  
  for (i in seq_along(grid)) {
    Cstat[i] <- n * (mean((rY - grid[i] * rD) * rZ))^2 /
      var((rY - grid[i] * rD) * rZ)
  }
  
  crit   <- qchisq(1 - alpha, 1)
  inside <- grid[Cstat < crit]
  
  if (length(inside) == 0) {
    warning("Confidence set is empty — widen the grid.")
    LB <- NA; UB <- NA
  } else {
    LB <- min(inside)
    UB <- max(inside)
  }
  
  axis_breaks <- pretty(grid, n = 7)
  axis_labels <- paste0(format(axis_breaks / 1000, big.mark = ",",
                               scientific = FALSE), "k")
  
  plot(range(grid), range(Cstat), type = "n",
       xlab = "Effect of treatment (€)",
       ylab = "AR Statistic",
       main = sprintf("DML-AR %.0f%% Confidence Set", (1 - alpha) * 100),
       xaxt = "n")  
  
  axis(1, at = axis_breaks, labels = axis_labels, las = 1)
  
  lines(grid, Cstat, lty = 1, col = 1, lwd = 2)
  abline(h = crit, lty = 3, col = 4, lwd = 1.5)
  abline(v = LB,   lty = 3, col = 2, lwd = 1.5)
  abline(v = UB,   lty = 3, col = 2, lwd = 1.5)
  
  cat(sprintf("\n── AR %.0f%% Confidence Interval ──\n", (1 - alpha) * 100))
  cat(sprintf("   LB : %s\n", format(LB, big.mark = ",", scientific = FALSE)))
  cat(sprintf("   UB : %s\n", format(UB, big.mark = ",", scientific = FALSE)))
  cat(sprintf("   Chi2 critical value (df=1): %.4f\n\n", crit))
  
  return(invisible(list(LB = LB, UB = UB, Cstat = Cstat, grid = grid)))
}

# 4. Run it for our data (PBS)
set.seed(2222)

# Prepare matrices
x_cols = c("REGL",
           "BIO_FIL",
           "EXTERNALISFIL",
           "CERTIFICATION_BINARY",
           "RDEV",
           "SAU_TOT",
           "UGBTA.TOT",
           grep("^LABOUR_TYPE", names(panel_diff), value = TRUE),
           grep("^STATUT", names(panel_diff), value = TRUE),
           grep("^OTEFDA", names(panel_diff), value = TRUE))
x <- as.matrix(panel_diff_dt[, ..x_cols])
y <- panel_diff_dt$dY
d <- panel_diff_dt$dD
z <- panel_diff_dt$OVER_67

# Random Forest learners
dreg <- function(x, d) ranger(y ~ ., data = data.frame(y = d, x),
                              num.trees = 500,
                              mtry = floor(sqrt(ncol(x))),
                              min.node.size = 2,
                              max.depth = 5)

yreg <- function(x, y) ranger(y ~ ., data = data.frame(y = y, x),
                              num.trees = 500,
                              mtry = floor(sqrt(ncol(x))),
                              min.node.size = 2,
                              max.depth = 5)

zreg <- function(x, z) ranger(y ~ ., data = data.frame(y = z, x),
                              num.trees = 500,
                              mtry = floor(sqrt(ncol(x))),
                              min.node.size = 2,
                              max.depth = 5)

# Step 1: DML
dml2_rf <- dml2_for_plivm(x, d, z, y, dreg, yreg, zreg, nfold = 5)

# Step 2: AR inference
dml_ar_pliv(
  rY   = dml2_rf$ytil,
  rD   = dml2_rf$dtil,
  rZ   = dml2_rf$ztil,
  grid = seq(-600000, 0, by = 1000)
)
