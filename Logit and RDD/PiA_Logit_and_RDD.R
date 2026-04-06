# Load libraries
library(dplyr)
library(tidyverse)
library(haven)
library(arrow)
library(ggplot2)
library(margins)
library(rdrobust)
library(sandwich)
library(lmtest)
library(rddensity)

#========================= Preparation of the analyses =========================

# Load data file
path_data <- "/Users/monabnis/Desktop/Polytechnique/Polytechnique M2/Policy-in-action/Data/francetransfert-1325119685"

file <- file.path(path_data, "PiA_panel_clean_coef.parquet")
panel_data <- read_parquet(file)

# Create treatment variable (1 if treated, meaning loss of eligibility)
panel_data <- panel_data %>%
  filter(!is.na(BENEF_PAC)) %>%
  group_by(farm_id) %>%
  mutate(treated = if_else(any(year == 2020 & BENEF_PAC == 1) & any(year == 2023 & BENEF_PAC == 0), 1, 0)) %>%
  mutate(treated = if_else(year == 2023, treated, 0))

# Create appropriate dataset for analysis
data <- panel_data %>%
  group_by(farm_id) %>%
  summarize(
    Y = EXIT[year == 2023],
    D = treated[year == 2023],
    running_age = AGE[year == 2020] - 64,
    PBSTOT_COEF17_2023 = PBSTOT_COEF17[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, DIMECO_COEF2017,
             SIEGE_LIB_REG, AGE),
           ~ .x[year == 2020])
  ) %>%
  ungroup()

# Factor categorical variables 
data$LABOUR_TYPE <- factor(data$LABOUR_TYPE)
data$STATUT <- factor(data$STATUT)
data$OTEFDA_COEF17 <- factor(data$OTEFDA_COEF17)


##################################################
###########         Logit model        ###########
###########       Impact on exit       ###########
##################################################

#========================== Preliminary preparations ===========================

# List of the variables we will use in the analysis
categorical_variables <- c(
  "REGL",
  "STATUT",
  "DIMECO_COEF2017",
  "OTEFDA_COEF17",
  "BIO_FIL",
  "EXTERNALISFIL",
  "CERTIFICATION_BINARY", 
  "RDEV", 
  "LABOUR_TYPE"
)

numerical_variables <- c("SAU_TOT",
                         "PBSTOT_COEF17", 
                         "UGBTA.TOT")

#==================== Logistic regression without controls =====================

logit_no_controls <- glm(Y ~ D, data = data, family = binomial(link = "logit"))
summary(logit_no_controls)
exp(coef(logit_no_controls))

# Estimate marginal effects (by how many percentage points does treatment change exit probability?)
marginal <- margins(logit_no_controls, variables = "D")
summary(marginal) 
# On average, being treated increases the probability of farm exit by 
# 11.11 percentage points, compared to not being treated, holding other covariates constant.
# Statistically significant (p-value < 0.01)


#====================== Logistic regression with controls ======================

#Logistic regression
logit_formula <- as.formula(paste("Y ~ D +", paste(c(categorical_variables, numerical_variables), collapse = " + ")))

logit <- glm(logit_formula, data = data, family = binomial(link = "logit"))
summary(logit)
exp(coef(logit))

# Estimate marginal effects (by how many percentage points does treatment change exit probability?)
marginal <- margins(logit, variables = "D")
summary(marginal) 
# On average, being treated increases the probability of farm exit by 
# 10.22 percentage points, compared to not being treated, holding other covariates constant.
# Statistically significant (p-value < 0.01)

#================= Restrict dataset to BENEF_PAC = 1 in 2020  ==================

data_restricted <- panel_data %>%
  group_by(farm_id) %>%
  filter(any(year == 2020 & BENEF_PAC == 1)) %>%
  summarize(
    Y = EXIT[year == 2023],
    D = treated[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, DIMECO_COEF2017),
           ~ .x[year == 2020])
  )

data_restricted$LABOUR_TYPE <- factor(data_restricted$LABOUR_TYPE)
data_restricted$STATUT <- factor(data_restricted$STATUT)
data_restricted$OTEFDA_COEF17 <- factor(data_restricted$OTEFDA_COEF17)

#Logistic regression (controls)
logit_restricted <- glm(logit_formula, data = data_restricted, family = binomial(link = "logit"))
summary(logit_restricted)
exp(coef(logit_restricted))

# Estimate marginal effects (by how many percentage points does treatment change exit probability?)
marginal <- margins(logit_restricted, variables = "D")
summary(marginal) 
# On average, being treated increases the probability of farm exit by 
# 10.46 percentage points, compared to not being treated, holding other covariates constant.
# Statistically significant (p-value < 0.01)

#Logistic regression (no controls)
logit_restricted_nc <- glm(Y ~ D, data = data_restricted, family = binomial(link = "logit"))
summary(logit_restricted_nc)
exp(coef(logit_restricted_nc))

# Estimate marginal effects (by how many percentage points does treatment change exit probability?)
marginal <- margins(logit_restricted_nc, variables = "D")
summary(marginal) 
# On average, being treated increases the probability of farm exit by 
# 11.42 percentage points, compared to not being treated, holding other covariates constant.
# Statistically significant (p-value < 0.01)

#=========================== Exclude the micro farms ===========================

data_exclusion <- panel_data %>%
  group_by(farm_id) %>%
  mutate(IS_MICRO = if_else(DIMECO_COEF2017 == "1-micros", 1, 0)) %>%
  filter(IS_MICRO == 0) %>%
  summarize(
    Y = EXIT[year == 2023],
    D = treated[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, DIMECO_COEF2017),
           ~ .x[year == 2020])
  )

data_exclusion$LABOUR_TYPE <- factor(data_exclusion$LABOUR_TYPE)
data_exclusion$STATUT <- factor(data_exclusion$STATUT)
data_exclusion$OTEFDA_COEF17 <- factor(data_exclusion$OTEFDA_COEF17)

#Logistic regression (controls)
logit_exclusion <- glm(logit_formula, data = data_exclusion, family = binomial(link = "logit"))
summary(logit_exclusion)
exp(coef(logit_exclusion))

# Estimate marginal effects (by how many percentage points does treatment change exit probability?)
marginal <- margins(logit_exclusion, variables = "D")
summary(marginal) 
# On average, being treated increases the probability of farm exit by 
# 7.90 percentage points, compared to not being treated, holding other covariates constant.
# Statistically significant (p-value < 0.01)

#Logistic regression (no controls)
logit_exclusion_nc <- glm(Y ~ D, data = data_exclusion, family = binomial(link = "logit"))
summary(logit_exclusion_nc)
exp(coef(logit_exclusion_nc))

# Estimate marginal effects (by how many percentage points does treatment change exit probability?)
marginal <- margins(logit_exclusion_nc, variables = "D")
summary(marginal) 
# On average, being treated increases the probability of farm exit by 
# 8.23 percentage points, compared to not being treated, holding other covariates constant.
# Statistically significant (p-value < 0.01)

#================ Comparative Plot of different Logit Estimates ================

# 1. Put models into a named list
models_list <- list(
  "Controls (full)"     = logit,
  "No controls (full)"         = logit_no_controls,
  "Controls (BENEF_PAC=1 in 2020)"   = logit_restricted,
  "No Controls (BENEF_PAC=1 in 2020)" = logit_restricted_nc,
  "Controls (exclude micro)"    = logit_exclusion,
  "No controls (exclude micro)" = logit_exclusion_nc
)

# 2. Extract Marginal Effects for variable "D"
plot_data <- do.call(rbind, lapply(names(models_list), function(name) {
  m <- models_list[[name]]
  
  # Calculate marginal effects for this specific model
  # We wrap this in summary() to get the tidy data frame of effects
  marg_sum <- summary(margins(m, variables = "D"))
  
  # Extract the row where factor == "D"
  res <- marg_sum[marg_sum$factor == "D", ]
  
  data.frame(
    spec     = name,
    estimate = res$AME,
    se       = res$SE,
    ci_low   = res$lower,
    ci_high  = res$upper
  )
}))

# 3. Add significance flag
plot_data$significant <- ifelse(plot_data$ci_low > 0 | plot_data$ci_high < 0, 
                                "Significant (5%)", "Not significant")

# 4. Generate the Visualization
ggplot(plot_data, aes(x = reorder(spec, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant), 
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3.5) +
  coord_flip() +
  scale_x_discrete(limits = rev(names(models_list))) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.07, 0.12),                  # Starts at 7%, ends at 12%
    breaks = seq(0.07, 0.12, by = 0.01)      # Tick marks every 1%
  ) +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Model specification",
    y = "Average Marginal Effect (Change in Exit Probability)",
    title = "Logit Marginal Effects Across Model Specifications",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.ticks.x = element_line(color = "black"),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )


#=================== Logistic regression by region (controls) ==================

# 1. Create a function to create and run logit model for each region
run_regional_logits <- function(data, formula, region_col = "SIEGE_LIB_REG") {
  
  # Get a list of unique regions, removing any NAs
  regions <- unique(data[[region_col]])
  regions <- regions[!is.na(regions)]
  
  # Loop through each region
  for (reg in regions) {
    
    # Create a safe name for the object (replaces spaces/special chars with underscores)
    safe_name <- gsub("[^[:alnum:]]", "_", reg)
    model_name <- paste0("logit_", safe_name)
    
    # Subset data for the specific region
    region_data <- data[data[[region_col]] == reg, ]
    
    # Run the model (wrapped in tryCatch in case a region has no variation in Y)
    tryCatch({
      model <- glm(formula, data = region_data, family = binomial(link = "logit"))
      
      # Assign the model to the global environment with the desired name
      assign(model_name, model, envir = .GlobalEnv)
      
      message(paste("Successfully created:", model_name))
    }, error = function(e) {
      message(paste("Could not run model for", reg, ":", e$message))
    })
  }
}

# 2. Define the formula
logit_formula <- as.formula(paste("Y ~ D +", paste(c(categorical_variables, numerical_variables), collapse = " + ")))

# 3. Run the function
run_regional_logits(data = data, formula = logit_formula)


#================== Logistic regression by region (no controls) ================

# 1. Create a function to create and run logit model for each region
run_regional_logits_nc <- function(data, formula, region_col = "SIEGE_LIB_REG") {
  
  # Get a list of unique regions, removing any NAs
  regions <- unique(data[[region_col]])
  regions <- regions[!is.na(regions)]
  
  # Loop through each region
  for (reg in regions) {
    
    # Create a safe name for the object (replaces spaces/special chars with underscores)
    safe_name <- gsub("[^[:alnum:]]", "_", reg)
    model_name <- paste0("logit_nc_", safe_name)
    
    # Subset data for the specific region
    region_data <- data[data[[region_col]] == reg, ]
    
    # Run the model (wrapped in tryCatch in case a region has no variation in Y)
    tryCatch({
      model <- glm(formula, data = region_data, family = binomial(link = "logit"))
      
      # Assign the model to the global environment with the desired name
      assign(model_name, model, envir = .GlobalEnv)
      
      message(paste("Successfully created:", model_name))
    }, error = function(e) {
      message(paste("Could not run model for", reg, ":", e$message))
    })
  }
}

# 2. Define the formula
logit_formula_nc <- as.formula(Y ~ D)

# 3. Run the function
run_regional_logits_nc(data = data, formula = logit_formula_nc)


#==== Comparative Plot of different Logit Estimates by region (with controls) ====

# 1. Put models into a named list
regions <- unique(data[["SIEGE_LIB_REG"]])
regions <- regions[!is.na(regions)]

models_list <- list()

for (reg in regions) {
  obj_name <- paste0("logit_", gsub(" ", "_", reg))
  # Check if the model object actually exists before trying to add it
  if (exists(obj_name)) {
    # Assign the ACTUAL object to the list entry named after the region
    models_list[[reg]] <- get(obj_name)
    message(paste("Added", reg, "to the list."))
  } else {
    message(paste("Warning: Object", obj_name, "not found."))
  }
}

# 2. Extract Marginal Effects for variable "D"
plot_data <- do.call(rbind, lapply(names(models_list), function(name) {
  m <- models_list[[name]]
  
  # Specify the subset of the data to use
  region_data <- data[data$SIEGE_LIB_REG == name, ]
  
  # Get the margins
  tryCatch({
    marg_sum <- summary(margins(m, variables = "D", data = region_data))
    res <- marg_sum[marg_sum$factor == "D", ]
    
    if(nrow(res) == 0) return(NULL)
    
    data.frame(
      spec     = name,
      estimate = res$AME,
      se       = res$SE,
      ci_low   = res$lower,
      ci_high  = res$upper
    )
  }, error = function(e) {
    message(paste("Skipping", name, "due to error:", e$message))
    return(NULL)
  })
}))

# 3. Add significance flag
plot_data$significant <- ifelse(plot_data$ci_low > 0 | plot_data$ci_high < 0, 
                                "Significant (5%)", "Not significant")

# 4. Generate the Visualization
ggplot(plot_data, aes(x = reorder(spec, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant), 
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3.5) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.04, 0.19),                  # Starts at 4%, ends at 19%
    breaks = seq(0.04, 0.19, by = 0.01)      # Tick marks every 1%
  ) +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Model specification",
    y = "Average Marginal Effect (Change in Exit Probability)",
    title = "Logit Marginal Effects by Region (with Covariates)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.ticks.x = element_line(color = "black"),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )



#==== Comparative Plot of different Logit Estimates by region (no controls) ====

# 1. Put models into a named list
regions <- unique(data[["SIEGE_LIB_REG"]])
regions <- regions[!is.na(regions)]

models_list <- list()

for (reg in regions) {
  obj_name <- paste0("logit_nc_", gsub(" ", "_", reg))
  # Check if the model object actually exists before trying to add it
  if (exists(obj_name)) {
    # Assign the ACTUAL object to the list entry named after the region
    models_list[[reg]] <- get(obj_name)
    message(paste("Added", reg, "to the list."))
  } else {
    message(paste("Warning: Object", obj_name, "not found."))
  }
}

# 2. Extract Marginal Effects for variable "D"
plot_data <- do.call(rbind, lapply(names(models_list), function(name) {
  m <- models_list[[name]]
  
  # Specify the subset of the data to use
  region_data <- data[data$SIEGE_LIB_REG == name, ]
  
  # Get the margins
  tryCatch({
    marg_sum <- summary(margins(m, variables = "D", data = region_data))
    res <- marg_sum[marg_sum$factor == "D", ]
    
    if(nrow(res) == 0) return(NULL)
    
    data.frame(
      spec     = name,
      estimate = res$AME,
      se       = res$SE,
      ci_low   = res$lower,
      ci_high  = res$upper
    )
  }, error = function(e) {
    message(paste("Skipping", name, "due to error:", e$message))
    return(NULL)
  })
}))

# 3. Add significance flag
plot_data$significant <- ifelse(plot_data$ci_low > 0 | plot_data$ci_high < 0, 
                                "Significant (5%)", "Not significant")

# 4. Generate the Visualization
ggplot(plot_data, aes(x = reorder(spec, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant), 
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3.5) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.04, 0.19),                  # Starts at 4%, ends at 19%
    breaks = seq(0.04, 0.19, by = 0.01)      # Tick marks every 1%
  ) +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Model specification",
    y = "Average Marginal Effect (Change in Exit Probability)",
    title = "Logit Marginal Effects by Region (without Covariates)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.ticks.x = element_line(color = "black"),
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )






##################################################
###########            RDD             ###########
###########       Impact on exit       ###########
##################################################

#========================== Preliminary preparations ===========================

# List of the covariates
categorical_variables <- c(
  "REGL",
  "STATUT",
  "DIMECO_COEF2017",
  "OTEFDA_COEF17",
  "BIO_FIL",
  "EXTERNALISFIL",
  "CERTIFICATION_BINARY", 
  "RDEV", 
  "LABOUR_TYPE"
)

numerical_variables <- c("SAU_TOT",
                         "PBSTOT_COEF17", 
                         "UGBTA.TOT")

# Create the design matrix of covariates
covs_matrix <- model.matrix(
  ~ 0 + ., 
  data = data[, c(categorical_variables, numerical_variables)]
)

# Choose an optimal bandwidth
bw <- rdbwselect(
  y = data$Y,
  x = data$running_age,
  c = 0,
  fuzzy = data$treated,
  masspoints = "adjust"
)

summary(bw)

#============================= RDD with controls ===============================

h_mse <- bw$bws["mserd", c("h (left)", "h (right)")]

rd <- rdrobust(
  y = data$Y,
  x = data$running_age,
  c = 0,
  h = h_mse,
  covs = covs_matrix,
  fuzzy = data$D,
  masspoints = "adjust"
)

summary(rd)    # Statistically significant at 5% level

#============================ RDD without controls =============================

rd_nc <- rdrobust(
  y = data$Y,
  x = data$running_age,
  c = 0,
  h = h_mse,
  fuzzy = data$D,
  masspoints = "adjust"
)

summary(rd_nc)    # Statistically significant at 5% level

#================= Restrict dataset to BENEF_PAC = 1 in 2020  ==================

data_restricted <- panel_data %>%
  group_by(farm_id) %>%
  filter(any(year == 2020 & BENEF_PAC == 1)) %>%
  summarize(
    Y = EXIT[year == 2023],
    D = treated[year == 2023],
    running_age = AGE[year == 2020] - 64,
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, DIMECO_COEF2017,
             SIEGE_LIB_REG, AGE),
           ~ .x[year == 2020])
  ) %>%
  ungroup()

# Create the design matrix of covariates
covs_matrix <- model.matrix(
  ~ 0 + ., 
  data = data_restricted[, c(categorical_variables, numerical_variables)]
)

# Choose an optimal bandwidth
bw <- rdbwselect(
  y = data_restricted$Y,
  x = data_restricted$running_age,
  c = 0,
  fuzzy = data_restricted$D,
  masspoints = "adjust"
)

summary(bw)

h_mse <- bw$bws["mserd", c("h (left)", "h (right)")]

# With controls
rd_restricted <- rdrobust(
  y = data_restricted$Y,
  x = data_restricted$running_age,
  c = 0,
  h = h_mse,
  covs = covs_matrix,
  fuzzy = data_restricted$D,
  masspoints = "adjust"
)

summary(rd_restricted) #Statistically significant at the 5% level

# Without controls
rd_restricted_nc <- rdrobust(
  y = data_restricted$Y,
  x = data_restricted$running_age,
  c = 0,
  h = h_mse,
  fuzzy = data_restricted$D,
  masspoints = "adjust"
)

summary(rd_restricted_nc) #Statistically significant at the 5% level


#=========================== Exclude the micro farms ===========================

data_exclusion <- panel_data %>%
  group_by(farm_id) %>%
  mutate(IS_MICRO = if_else(DIMECO_COEF2017 == "1-micros", 1, 0)) %>%
  filter(IS_MICRO == 0) %>%
  summarize(
    Y = EXIT[year == 2023],
    D = treated[year == 2023],
    running_age = AGE[year == 2020] - 64,
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, DIMECO_COEF2017,
             SIEGE_LIB_REG, AGE),
           ~ .x[year == 2020])
  ) %>%
  ungroup()

# Create the design matrix of covariates
covs_matrix <- model.matrix(
  ~ 0 + ., 
  data = data_exclusion[, c(categorical_variables, numerical_variables)]
)

# Choose an optimal bandwidth
bw <- rdbwselect(
  y = data_exclusion$Y,
  x = data_exclusion$running_age,
  c = 0,
  fuzzy = data_exclusion$D,
  masspoints = "adjust"
)

summary(bw)

h_mse <- bw$bws["mserd", c("h (left)", "h (right)")]

# With controls
rd_exclusion <- rdrobust(
  y = data_exclusion$Y,
  x = data_exclusion$running_age,
  c = 0,
  h = h_mse,
  covs = covs_matrix,
  fuzzy = data_exclusion$D,
  masspoints = "adjust"
)

summary(rd_exclusion) #Statistically significant at the 10% level

# Without controls
rd_exclusion_nc <- rdrobust(
  y = data_exclusion$Y,
  x = data_exclusion$running_age,
  c = 0,
  h = h_mse,
  fuzzy = data_exclusion$D,
  masspoints = "adjust"
)

summary(rd_exclusion_nc) #Statistically significant at the 10% level



#================ Comparative Plot of different RDD Estimates ================

models_list <- list(
  "Controls (full)"     = rd,
  "No controls (full)"         = rd_nc,
  "Controls (BENEF_PAC=1 in 2020)"   = rd_restricted,
  "No Controls (BENEF_PAC=1 in 2020)" = rd_restricted_nc,
  "Controls (exclude micro)"    = rd_exclusion,
  "No controls (exclude micro)" = rd_exclusion_nc
)

rd_specs <- do.call(rbind, lapply(names(models_list), function(spec) {
  
  m <- models_list[[spec]]
  
  est <- m$Estimate[2]   
  se  <- m$se[3]
  
  data.frame(
    spec = spec,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se
  )
  
})) %>%
  mutate(significant = ifelse(ci_low > 0 | ci_high < 0,
                              "Significant (5%)",
                              "Not significant"))

rd_specs$spec <- factor(rd_specs$spec, levels = names(models_list))

plot_rd_specs <- ggplot(rd_specs, aes(x = spec, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "grey40") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3) +
  coord_flip() +
  scale_x_discrete(limits = rev(names(models_list))) +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Model specification",
    y = "RD estimate (Lost eligibility effect on exit)",
    title = "RD Estimates Across Model Specifications",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_rd_specs


##################################################
###########            RDD             ###########
###########    Impact on production    ###########
##################################################

#========================== Preliminary preparations ===========================

# List of the covariates
categorical_variables <- c(
  "REGL",
  "STATUT",
  "OTEFDA_COEF17",
  "BIO_FIL",
  "EXTERNALISFIL",
  "CERTIFICATION_BINARY", 
  "RDEV", 
  "LABOUR_TYPE"
)

numerical_variables <- c("SAU_TOT", 
                         "UGBTA.TOT")

# Create the design matrix of covariates
covs_matrix <- model.matrix(
  ~ 0 + ., 
  data = data[, c(categorical_variables, numerical_variables)]
)

# Choose an optimal bandwidth
bw <- rdbwselect(
  y = data$PBSTOT_COEF17_2023,
  x = data$running_age,
  c = 0,
  fuzzy = data$D,
  masspoints = "adjust"
)

summary(bw)

h_mse <- bw$bws["mserd", c("h (left)", "h (right)")]

#============================= RDD with controls ===============================

rd <- rdrobust(
  y = data$PBSTOT_COEF17_2023,
  x = data$running_age,
  c = 0,
  h = h_mse,
  covs = covs_matrix,
  fuzzy = data$D,
  masspoints = "adjust"
)

summary(rd)    # Not statistically significant 

#============================ RDD without controls =============================

rd_nc <- rdrobust(
  y = data$PBSTOT_COEF17_2023,
  x = data$running_age,
  c = 0,
  h = h_mse,
  fuzzy = data$D,
  masspoints = "adjust"
)

summary(rd_nc)    # Not statistically significant

#================= Restrict dataset to BENEF_PAC = 1 in 2020  ==================

data_restricted <- panel_data %>%
  group_by(farm_id) %>%
  filter(any(year == 2020 & BENEF_PAC == 1)) %>%
  summarize(
    Y = EXIT[year == 2023],
    D = treated[year == 2023],
    running_age = AGE[year == 2020] - 64,
    PBSTOT_COEF17_2023 = PBSTOT_COEF17[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, DIMECO_COEF2017,
             SIEGE_LIB_REG, AGE),
           ~ .x[year == 2020])
  ) %>%
  ungroup()

# Create the design matrix of covariates
covs_matrix <- model.matrix(
  ~ 0 + ., 
  data = data_restricted[, c(categorical_variables, numerical_variables)]
)

# Choose an optimal bandwidth
bw <- rdbwselect(
  y = data_restricted$PBSTOT_COEF17_2023,
  x = data_restricted$running_age,
  c = 0,
  fuzzy = data_restricted$D,
  masspoints = "adjust"
)

summary(bw)

h_mse <- bw$bws["mserd", c("h (left)", "h (right)")]

# With controls
rd_restricted <- rdrobust(
  y = data_restricted$PBSTOT_COEF17_2023,
  x = data_restricted$running_age,
  c = 0,
  h = h_mse,
  covs = covs_matrix,
  fuzzy = data_restricted$D,
  masspoints = "adjust"
)

summary(rd_restricted) #Not statistically significant

# Without controls
rd_restricted_nc <- rdrobust(
  y = data_restricted$PBSTOT_COEF17_2023,
  x = data_restricted$running_age,
  c = 0,
  h = h_mse,
  fuzzy = data_restricted$D,
  masspoints = "adjust"
)

summary(rd_restricted_nc) #Not statistically significant 


#=========================== Exclude the micro farms ===========================

data_exclusion <- panel_data %>%
  group_by(farm_id) %>%
  mutate(IS_MICRO = if_else(DIMECO_COEF2017 == "1-micros", 1, 0)) %>%
  filter(IS_MICRO == 0) %>%
  summarize(
    Y = EXIT[year == 2023],
    D = treated[year == 2023],
    running_age = AGE[year == 2020] - 64,
    PBSTOT_COEF17_2023 = PBSTOT_COEF17[year == 2023],
    across(c(REGL, OTEFDA_COEF17, BIO_FIL, EXTERNALISFIL, LABOUR_TYPE, PBSTOT_COEF17,
             CERTIFICATION_BINARY, RDEV, SAU_TOT, UGBTA.TOT, STATUT, DIMECO_COEF2017,
             SIEGE_LIB_REG, AGE),
           ~ .x[year == 2020])
  ) %>%
  ungroup()

# Create the design matrix of covariates
covs_matrix <- model.matrix(
  ~ 0 + ., 
  data = data_exclusion[, c(categorical_variables, numerical_variables)]
)

# Choose an optimal bandwidth
bw <- rdbwselect(
  y = data_exclusion$PBSTOT_COEF17_2023,
  x = data_exclusion$running_age,
  c = 0,
  fuzzy = data_exclusion$D,
  masspoints = "adjust"
)

summary(bw)

h_mse <- bw$bws["mserd", c("h (left)", "h (right)")]

# With controls
rd_exclusion <- rdrobust(
  y = data_exclusion$PBSTOT_COEF17_2023,
  x = data_exclusion$running_age,
  c = 0,
  h = h_mse,
  covs = covs_matrix,
  fuzzy = data_exclusion$D,
  masspoints = "adjust"
)

summary(rd_exclusion) #Statistically significant at the 10% level

# Without controls
rd_exclusion_nc <- rdrobust(
  y = data_exclusion$PBSTOT_COEF17_2023,
  x = data_exclusion$running_age,
  c = 0,
  h = h_mse,
  fuzzy = data_exclusion$D,
  masspoints = "adjust"
)

summary(rd_exclusion_nc) #Statistically significant at the 10% level


#================ Comparative Plot of different RDD Estimates ================

models_list <- list(
  "Controls (full)"     = rd,
  "No controls (full)"         = rd_nc,
  "Controls (BENEF_PAC=1 in 2020)"   = rd_restricted,
  "No Controls (BENEF_PAC=1 in 2020)" = rd_restricted_nc,
  "Controls (exclude micro)"    = rd_exclusion,
  "No controls (exclude micro)" = rd_exclusion_nc
)

rd_specs <- do.call(rbind, lapply(names(models_list), function(spec) {
  
  m <- models_list[[spec]]
  
  est <- m$Estimate[2]   
  se  <- m$se[3]
  
  data.frame(
    spec = spec,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se
  )
  
})) %>%
  mutate(significant = ifelse(ci_low > 0 | ci_high < 0,
                              "Significant (5%)",
                              "Not significant"))

rd_specs$spec <- factor(rd_specs$spec, levels = names(models_list))

plot_rd_specs <- ggplot(rd_specs, aes(x = spec, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "grey40") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.15, linewidth = 1) +
  geom_point(aes(color = significant), size = 3) +
  coord_flip() +
  scale_x_discrete(limits = rev(names(models_list))) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = c(
    "Significant (5%)" = "#1B9E77",
    "Not significant"  = "#D95F02"
  )) +
  labs(
    x = "Model specification",
    y = "RD estimate (Lost eligibility effect on standard gross output)",
    title = "RD Estimates Across Model Specifications",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_rd_specs


