# Load necessary libraries
library(writexl)
library(haven)
library(tidyverse)
library(scales)              # For formatting y-axis with thousand separators
library(sysfonts)
library(showtext)
library(caret)               # For confusion matrix and model evaluation
library(pROC)                # For ROC curve and AUC calculation
library(survey)
library(randomForest)
library(smotefamily)
library(themis)              # For SMOTE (via tidymodels)
library(tidymodels)          # For SMOTE (via tidymodels)
library(sf)
library(survival)
library(survminer)


# Set tidymodels preferences
tidymodels_prefer()

# Load helper functions
source("scripts/helpers.R")

# Load data frames from external R script
data_frames <- source("scripts/load.R")$value

# Access the loaded data frames
bh_src <- data_frames$bh_data
ch_src <- data_frames$ch_data
fs_src <- data_frames$fs_data
hh_src <- data_frames$hh_data
hl_src <- data_frames$hl_data
wm_src <- data_frames$wm_data

# Store population data
pop_data <- pop_data

# Clean up workspace by removing unnecessary objects and freeing memory
rm(data_list, data, data_frames, columns_info)
gc()  # Trigger garbage collection

# Load theme and color palette
source("scripts/theme.R")

# Load dataset transformations
source("scripts/etl.R")

# Perform exploratory data analysis
source("scripts/eda.R")

# Perform statistical analysis

source("scripts/analysis/lr_anc4_any_multivariate.R")
source("scripts/analysis/u5mr_anc_sba_by_governorate.R")
source("scripts/analysis/survival_analysis.R")

