# Load necessary libraries
library(tidyverse)          # Data wrangling and visualization
library(survey)             # Complex survey analysis
library(randomForest)       # Modeling
library(caret)              # Model evaluation
library(pROC)               # ROC curve and AUC calculation
library(survival)           # Survival analysis
library(sf)                 # Spatial data
library(survminer)          # Survival plots
library(writexl)            # Writing data to Excel
library(haven)              # Reading .sav files
library(smotefamily)        # SMOTE for handling imbalanced data
library(themis)             # SMOTE (via tidymodels)
library(tidymodels)         # For SMOTE (via tidymodels)
library(scales)             # Formatting y-axis with thousand separators
library(sysfonts)           # Custom fonts for plots
library(showtext)           # Additional text rendering for plots



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

