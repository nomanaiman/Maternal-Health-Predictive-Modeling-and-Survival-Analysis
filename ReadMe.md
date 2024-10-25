
# Improving Health Outcomes in Yemen: A Quantitative Analysis Approach in A Challenging Context



- [Improving Health Outcomes in Yemen: A Quantitative Analysis Approach in A Challenging Context](#improving-health-outcomes-in-yemen-a-quantitative-analysis-approach-in-a-challenging-context)
  - [Abstract](#abstract)
  - [Setup and Installation](#setup-and-installation)
    - [1. Clone the Repository](#1-clone-the-repository)
    - [2. Install Dependencies](#2-install-dependencies)
    - [3. Project Structure](#3-project-structure)
  - [Data Sources](#data-sources)
  - [Analysis Overview](#analysis-overview)
    - [1. Logistic Regression on Antenatal Care (ANC)](#1-logistic-regression-on-antenatal-care-anc)
    - [2. Child Mortality and SBA Coverage by Governorate](#2-child-mortality-and-sba-coverage-by-governorate)
    - [3. Survival Analysis](#3-survival-analysis)
  - [How to Run the Analysis](#how-to-run-the-analysis)
  - [Future Work](#future-work)
  - [License](#license)


## Abstract

Yemen is the poorest country in the Middle East and the ninth poorest in the world. Enduring a protracted conflict that is continuing for more than 9 years, Yemen’s health care system suffered immensely. This project uses quantitative data analysis to better understand crucial health outcomes in the unique context of Yemen. Drawing mainly on Yemen MICS 2023 data, this project attempts to identify and measure some of the key socio-economic, educational, and regional factors that affect health variables impacting child mortality, antenatal care, and skilled birth attendance rates.

Through data preprocessing, exploratory analysis, statistical and predictive modeling techniques, i.e. weighted multivariate logistic regression and Welch t-test, survival analysis, this research aims to provide practical insights for governmental policymakers and humanitarian organizations. The analysis shows, among other things, that mothers’ education and household wealth have significant relationships with antenatal care and child survival rates in Yemen.

---

## Setup and Installation

### 1. Clone the Repository
To clone the repository to your local system, use:

```bash
git clone https://github.com/nomanaiman/Maternal-Health-Predictive-Modeling-and-Survival-Analysis.git
cd Maternal-Health-Predictive-Modeling-and-Survival-Analysis

```

### 2. Install Dependencies

This project uses several R libraries for data analysis, modeling, and visualization. You can install all necessary packages by running the following command:

```R
install.packages(c('cli', 'writexl', 'haven', 'tidyverse', 'scales', 'sysfonts', 'showtext', 
                   'caret', 'pROC', 'ResourceSelection', 'survey', 'car', 'glmnet', 
                   'smotefamily', 'randomForest', 'xgboost', 'corrplot', 'lme4', 'ROSE', 
                   'themis', 'tidymodels', 'DescTools', 'descr', 'weights', 'sf', 'rpart', 
                   'rpart.plot', 'xgboost', 'Matrix', 'lightgbm', 'survival', 'survminer', 
                   'naniar', 'cmprsk', 'patchwork'))
```

### 3. Project Structure

```
Maternal-Health-Predictive-Modeling-and-Survival-Analysis/
│
├── data/
│   ├── population.csv
│   ├── yem_admin_shapes/
│   │   └── yem_admbnda_adm1_govyem_cso_20191002.shp
│   ├── bh.sav
│   ├── ch.sav
│   ├── fs.sav
│   ├── hh.sav
│   ├── hl.sav
│   └── wm.sav
│
├── docs/
│
├── scripts/
│   ├── analysis/
│   │   ├── lr_anc4_any_multivariate.R
│   │   ├── u5mr_anc_sba_by_governorate.R
│   │   └── survival_analysis.R
│   ├── eda.R
│   ├── etl.R
│   ├── helpers.R
│   ├── load.R
│   └── theme.R
│
└── README.md
```

---

## Data Sources

The data used in this project comes from the Yemen Multiple Indicator Cluster Survey (MICS) 2023. This survey provides insights into key health indicators such as:

- **Under-5 mortality rate (U5MR)**
- **Antenatal care (ANC) coverage**
- **Skilled birth attendance (SBA)**
- **Maternal education level and household wealth**

Additionally, the shapefile used for mapping regional analyses is included under `yem_admin_shapes/`.

---

## Analysis Overview

### 1. Logistic Regression on Antenatal Care (ANC)
Using a multivariate logistic regression model, we examined the impact of socio-economic factors (e.g., wealth index, education level, and access to water) on antenatal care attendance (ANC4+ visits).

### 2. Child Mortality and SBA Coverage by Governorate
We conducted Welch t-tests and weighted analyses to explore the disparity in health outcomes across Yemen’s governorates. This part of the project identified regions that are “on-track” or “off-track” based on under-5 mortality rates and the coverage of skilled birth attendants.

### 3. Survival Analysis
Survival analysis was conducted to explore child mortality and survival rates. Factors such as mother’s age, wealth index, and maternal education were included in the model to predict survival probabilities.


---

## How to Run the Analysis

1. **Load the necessary helper functions and themes**:
    ```R
    source("scripts/helpers.R")
    source("scripts/load.R")
    source("scripts/theme.R")
    ```

2. **Run the Exploratory Data Analysis (EDA)**:
    ```R
    source("scripts/eda.R")
    ```

3. **Perform Specific Analyses**:
   - **Multivariate Logistic Regression**: `lr_anc4_any_multivariate.R`
   - **Under-5 Mortality and SBA Coverage**: `u5mr_anc_sba_by_governorate.R`
   - **Survival Analysis**: `survival_analysis.R`

---

## Future Work

1. **Incorporating more recent health data** as it becomes available.
2. **Extending analysis** to include other key health indicators such as malnutrition and maternal mortality.
3. **Refining the predictive models** using advanced machine learning techniques.

---

## License

This project is licensed under the [MIT License](LICENSE).
