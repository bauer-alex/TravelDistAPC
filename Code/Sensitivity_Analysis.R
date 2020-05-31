### This contains the sensitivity analyses done for the
### publication "Semiparametric APC analysis of destination choice patterns:
### Using Generalized Additive Models to Quantify the Impact of Age, Period
### and Cohort on Travel Distances".

# Loading of necessary packages and sourcing of self-defined functions:
library(tidyverse)
library(mgcv)
library(ggpubr)
library(ROCR)
source("Functions.R")


################################################################################

# Sensitivity analyses regarding German reunification (1990): analysis for
# West German population only

################################################################################

# Data preparation:
data <- data_preparation(data = data, thresholds = c(500, 1000, 2000, 6000),
                         labels = c("< 500 km", "500 - 1,000 km",
                                    "1,000 - 2,000 km", "2,000 - 6,000 km",
                                    "> 6,000 km"))
# Filtering for West German population:
data_west <- data %>% filter(is.na(S_Herkunft) | S_Herkunft == "West")


################################################################################

# Descriptive analysis:

# Relative frequencies of travel distances over time:
freq_periods(data_west)

# Individual distance curve for travelers in 2018:
plot_ridgeline(data = data_west, period = 2018, weighted = TRUE)

# Ridgeline matrix:
ridgeline_matrix(data = data_west, ages = list(20, 30, 40, 50, 60, 70, 80),
                 periods = list(1971:1979, 1980:1989, 1990:1999,
                                2000:2009, 2010:2018),
                 log = TRUE) 


################################################################################

# Modeling (pure APC model):

# Preprocessing of data for running APC models:
data_west <- preprocessing_model(data_west)

# Estimation of pure APC models:
model_list_pure <- lapply(X = 1:5, FUN = function(i) {
  model <- model_pure(data = data_west, target = i, method = "gam")
  return(model)
})

# Evaluation of pure APC models via AUC:
auc_list_pure <- lapply(1:5, function(i) {
  calculate_auc(data = data_west, target = i, method = "gam")
})

# Visualization of estimated tensor product surfaces via heatmaps:
heatmaps <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- plot_heatmap(model = i, data = data_west)
  return(plots)
})

# Visualization of marginal effects of all distance categories:
plot_marginal_effects(model_list = model_list_pure, data = data_west,
                      type = "pure")

# Visualization of partial APC plots:
# Age:
partial_plots_age <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_west, variable = "age",
                             type = "pure")
  return(plots)
})
# Period:
partial_plots_period <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_west, variable = "period",
                             type = "pure")
  return(plots)
})
# Cohort:
partial_plots_cohort <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_west, variable = "cohort",
                             type = "pure")
  return(plots)
})


################################################################################

# Modeling (covariate model):

# Estimation of covariate models:
model_list_covariate <- lapply(X = 1:5, FUN = function(i) {
  model <- model_covariate(data = data_west, target = i, method = "gam")
  return(model)
})

# Evaluation of covariate APC models via AUC:
auc_list_covariate <- lapply(1:5, function(i) {
  calculate_auc(data = data_west, target = i, method = "gam",
                type = "covariate")
})

# Visualization of estimated tensor product surfaces via heatmaps:
heatmaps <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- plot_heatmap(model = i, data = data_west, type = "covariate")
  return(plots)
})

# Visualization of marginal effects of all distance categories:
plot_marginal_effects(model_list = model_list_covariate, data = data_west,
                      type = "covariate")

# Visualization of partial APC plots:
# Age:
partial_plots_age <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_west, variable = "age",
                             type = "covariate")
  return(plots)
})
# Period:
partial_plots_period <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_west, variable = "period",
                             type = "covariate")
  return(plots)
})
# Cohort:
partial_plots_cohort <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_west, variable = "cohort",
                             type = "covariate")
  return(plots)
})

# Visualization of other covariates' effects:
plots_covariates <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- plot_covariates(model = i, data = data_west)
  return(plots)
})

# Comparison of marginal effects between pure and covariate APC models:
comparison_plots <- lapply(X = 1:5, FUN = function(i) {
  plots <- compare_pure_covariate(model_pure = model_list_pure[[i]],
                                  model_covariate = model_list_covariate[[i]],
                                  data = data_west)
  return(plots)
})



################################################################################
################################################################################

# Sensitivity analyses regarding change from German to German-speaking
# population (2010): analysis for German citizens only

################################################################################

# Filtering for German citizens:
data_german <- data %>% filter(is.na(S_Staatsangehoerigkeit) |
                                 S_Staatsangehoerigkeit == "Deutsch")


################################################################################

# Descriptive analysis:

# Relative frequencies of travel distances over time:
freq_periods(data_german)

# Individual distance curve for travelers in 2018:
plot_ridgeline(data = data_german, period = 2018, weighted = TRUE)

# Ridgeline matrix:
ridgeline_matrix(data = data_german, ages = list(20, 30, 40, 50, 60, 70, 80),
                 periods = list(1971:1979, 1980:1989, 1990:1999,
                                2000:2009, 2010:2018),
                 log = TRUE) 


################################################################################

# Modeling (pure APC model):

# Preprocessing of data for running APC models:
data_german <- preprocessing_model(data_german)

# Estimation of pure APC models:
model_list_pure <- lapply(X = 1:5, FUN = function(i) {
  model <- model_pure(data = data_german, target = i, method = "gam")
  return(model)
})

# Evaluation of pure APC models via AUC:
auc_list_pure <- lapply(1:5, function(i) {
  calculate_auc(data = data_german, target = i, method = "gam")
})

# Visualization of estimated tensor product surfaces via heatmaps:
heatmaps <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- plot_heatmap(model = i, data = data_german)
  return(plots)
})

# Visualization of marginal effects of all distance categories:
plot_marginal_effects(model_list = model_list_pure, data = data_german,
                      type = "pure")

# Visualization of partial APC plots:
# Age:
partial_plots_age <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_german, variable = "age")
  return(plots)
})
# Period:
partial_plots_period <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_german,
                             variable = "period")
  return(plots)
})
# Cohort:
partial_plots_cohort <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_german,
                             variable = "cohort")
  return(plots)
})


################################################################################

# Modeling (covariate model):

# Estimation of covariate models:
model_list_covariate <- lapply(X = 1:5, FUN = function(i) {
  model <- model_covariate(data = data_german, target = i, method = "gam")
  return(model)
})

# Evaluation of covariate APC models via AUC:
auc_list_covariate <- lapply(1:5, function(i) {
  calculate_auc(data = data_german, target = i, method = "gam",
                type = "covariate")
})

# Visualization of estimated tensor product surfaces via heatmaps:
heatmaps <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- plot_heatmap(model = i, data = data_german, type = "covariate")
  return(plots)
})

# Visualization of marginal effects of all distance categories:
plot_marginal_effects(model_list = model_list_covariate, data = data_german,
                      type = "covariate")

# Visualization of partial APC plots:
# Age:
partial_plots_age <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_german, variable = "age")
  return(plots)
})
# Period:
partial_plots_period <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_german,
                             variable = "period")
  return(plots)
})
# Cohort:
partial_plots_cohort <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data_german,
                             variable = "cohort")
  return(plots)
})

# Visualization of other covariates' effects:
plots_covariates <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- plot_covariates(model = i, data = data_german)
  return(plots)
})

# Comparison of marginal effects between pure and covariate APC models:
comparison_plots <- lapply(X = 1:5, FUN = function(i) {
  plots <- compare_pure_covariate(model_pure = model_list_pure[[i]],
                                  model_covariate = model_list_covariate[[i]],
                                  data = data_german)
  return(plots)
})


###############################################################################

