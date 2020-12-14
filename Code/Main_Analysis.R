### This is the main code file containing the analyses done for the
### publication "Semiparametric APC analysis of destination choice patterns:
### Using Generalized Additive Models to Quantify the Impact of Age, Period
### and Cohort on Travel Distances".

# Loading of necessary packages and sourcing of self-defined functions:
library(tidyverse)
library(mgcv)
library(ggpubr)
library(pROC)
source("Code/Functions.R")


################################################################################

# Data preparation:
data <- readRDS("Data/data.rds")
data <- data_preparation(data = data, thresholds = c(500, 1000, 2000, 6000),
                         labels = c("< 500 km", "500 - 1,000 km",
                                    "1,000 - 2,000 km", "2,000 - 6,000 km",
                                    "> 6,000 km"))


################################################################################

# Descriptive analysis:

# Relative frequencies of travel distances over time:
freq_periods(data)
ggsave("Graphics/Figure3.pdf", width = 8, height = 4.5)

# Individual distance curve for travelers in 2018:
plot_ridgeline(data = data, period = 2018, weighted = TRUE)
ggsave("Graphics/Figure4.pdf", width = 10, height = 4.5)

# Ridgeline matrix:
ridgeline_matrix(data = data, ages = list(20, 30, 40, 50, 60, 70, 80),
                 periods = list(1971:1979, 1980:1989, 1990:1999,
                                2000:2009, 2010:2018),
                 log = TRUE) 
ggsave("Graphics/Figure5.pdf", width = 11, height = 7.33)


################################################################################

# Modeling (pure APC model):

# Preprocessing of data for running APC models:
data <- preprocessing_model(data)

# Estimation of pure APC models:
model_list_pure <- lapply(X = 1:5, FUN = function(i) {
  model <- model_pure(data = data, target = i, method = "gam")
  saveRDS(object = model,
          file = paste0("Models/Main_Analysis/Pure_Model_", i, ".rds"))
  return(model)
})

# Evaluation of pure APC models via AUC:
auc_list_pure <- lapply(1:5, function(i) {
  calculate_auc(data = data, target = i, method = "gam")
})

# Visualization of estimated tensor product surfaces via heatmaps:
heatmaps <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- plot_heatmap(model = i, data = data)
  return(plots)
})
ggsave(plot = heatmaps[[5]], filename = "Graphics/Figure6.pdf", width = 17.5,
       height = 7)
for (i in 1:4) {
  ggsave(plot = heatmaps[[i]],
         filename = paste0("Graphics/A_Appendix/Figure", i, ".pdf"),
         width = 17.5, height = 7)
}

# Visualization of marginal effects of all distance categories:
plot_marginal_effects(model_list = model_list_pure, data = data, type = "pure")
ggsave("Graphics/Figure7.pdf", width = 10, height = 6)

# Visualization of partial APC plots:
# Cohort:
partial_plots_cohort <- lapply(X = 1:5, FUN = function(i) {
  if (i < 5) {
    title <- TRUE
  }
  else {
    title <- FALSE
  }
  plots <- partial_APC_plots(model = model_list_pure[[i]], data = data,
                             variable = "cohort", title = title)
  return(plots)
})
ggsave(plot = partial_plots_cohort[[5]], filename = "Graphics/Figure8.pdf",
       width = 10, height = 5)
for (i in 1:4) {
  ggsave(plot = partial_plots_cohort[[i]],
         filename = paste0("Graphics/B_Appendix/Figure", i * 3 + 2, ".pdf"),
         width = 10, height = 5)
}
# Age:
partial_plots_age <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data, variable = "age",
                             type = "pure")
  return(plots)
})
for (i in 1:4) {
  ggsave(plot = partial_plots_age[[i]],
         filename = paste0("Graphics/B_Appendix/Figure", i * 3 + 3, ".pdf"),
         width = 10, height = 5)
}
ggsave(plot = partial_plots_age[[5]],
       filename = "Graphics/B_Appendix/Figure17.pdf", width = 10, height = 5)
# Period:
partial_plots_period <- lapply(X = model_list_pure, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data, variable = "period")
  return(plots)
})
for (i in 1:4) {
  ggsave(plot = partial_plots_period[[i]],
         filename = paste0("Graphics/B_Appendix/Figure", i * 3 + 4, ".pdf"),
         width = 10, height = 5)
}
ggsave(plot = partial_plots_period[[5]],
       filename = "Graphics/B_Appendix/Figure18.pdf", width = 10, height = 5)


################################################################################

# Modeling (covariate model):

# Estimation of covariate models:
model_list_covariate <- lapply(X = 1:5, FUN = function(i) {
  model <- model_covariate(data = data, target = i, method = "gam")
  saveRDS(object = model,
          file = paste0("Models/Main_Analysis/Covariate_Model_", i, ".rds"))
  return(model)
})

# Evaluation of covariate APC models via AUC:
auc_list_covariate <- lapply(1:5, function(i) {
  calculate_auc(data = data, target = i, method = "gam", type = "covariate")
})

# Visualization of estimated tensor product surfaces via heatmaps:
heatmaps <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- plot_heatmap(model = i, data = data, type = "covariate")
  return(plots)
})

# Visualization of marginal effects of all distance categories:
plot_marginal_effects(model_list = model_list_covariate, data = data,
                      type = "covariate")

# Visualization of partial APC plots:
# Age:
partial_plots_age <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data, variable = "age",
                             type = "covariate")
  return(plots)
})
# Period:
partial_plots_period <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data, variable = "period",
                             type = "covariate")
  return(plots)
})
# Cohort:
partial_plots_cohort <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- partial_APC_plots(model = i, data = data, variable = "cohort",
                             type = "covariate")
  return(plots)
})

# Visualization of other covariates' effects:
plots_covariates <- lapply(X = model_list_covariate, FUN = function(i) {
  plots <- plot_covariates(model = i, data = data)
  return(plots)
})
ggsave(plot = plots_covariates[[5]], filename = "Graphics/Figure9.pdf",
       width = 10, height = 7)

# Comparison of marginal effects between pure and covariate APC models:
comparison_plots <- lapply(X = 1:5, FUN = function(i) {
  plots <- compare_pure_covariate(model_pure = model_list_pure[[i]],
                                  model_covariate = model_list_covariate[[i]],
                                  data = data)
  return(plots)
})
ggsave(plot = comparison_plots[[5]], filename = "Graphics/Figure10.pdf",
       width = 10, height = 5)


###############################################################################


