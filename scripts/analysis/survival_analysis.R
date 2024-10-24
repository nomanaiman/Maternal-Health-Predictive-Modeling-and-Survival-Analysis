# -------------------------------------
# Step 1: Prepare the survival_data dataframe
# -------------------------------------
survival_data <- bh_data %>%
  filter(!(stratum %in% c(13, 21, 25))) %>%  # Exclude certain strata
  select(HH1, HH2, BH9C, age_at_interview, education_level, windex5, water_access, HH6, p1_code,
         magebrt, wmweight, stratum, PSU, mother_age_at_birth) %>%
  mutate(
    # Event indicator: 1 if child died, 0 if alive at interview
    event = if_else(!is.na(BH9C), 1, 0),
    # Time variable: age at death if event occurred, otherwise age at interview
    time = if_else(event == 1, BH9C, age_at_interview),
    # Adjust time_months to avoid zero or negative values
    time_months = ifelse(time <= 0, 0.1, time),
    # Log of time_months
    log_time = log(time_months),
    
    # Log2 of time_months
    log2_time = log2(time_months),
    # Log of time_months using log1p to avoid negative values
    log1p_time = log1p(time_months),
    
    event = factor(event, levels = c(0, 1), labels = c("Censored", "Event")),
    time_years = time_months / 12,  # Convert months to years
    
    # Define neonatal and under-5 deaths
    neonatal = ifelse(time_months <= (28/30), 1, 0),
    under5 = ifelse(time_months <= 60, 1, 0)
  )

# -------------------------------------
# Step 2: Create the survey design object
# -------------------------------------
surv_design <- svydesign(
  id = ~PSU,                  # Primary sampling units
  strata = ~stratum,           # Strata
  weights = ~wmweight,         # Weights for the survey
  data = survival_data,
  nest = TRUE,
  options = list(lonely.psu = "certainty")  # Adjust for single-PSU strata
)

# -------------------------------------
# Step 3: Fit the Cox proportional hazards model
# -------------------------------------
# Create a survival formula
cox_model_formula <- as.formula(Surv(time_months, event == "Event") ~ education_level + windex5 + magebrt + water_access + HH6)

# Fit the survey-weighted Cox proportional hazards model
cox_model_svy <- svycoxph(
  formula = cox_model_formula,
  design = surv_design
)

# -------------------------------------
# Step 4: Display the summary and test proportional hazards assumption
# -------------------------------------
# Summary of the Cox model
cat("\nSummary of the survey-weighted Cox proportional hazards model:\n")
summary(cox_model_svy)

# Testing proportional hazards assumption
cat("\nTesting proportional hazards assumption:\n")
cox_zph_svy <- cox.zph(cox_model_svy)
print(cox_zph_svy)
ggcoxzph(cox_zph_svy)  # Plot Schoenfeld residuals

# -------------------------------------
# Step 5: Calculate Neonatal and Under-5 Mortality Rates (NMR, U5MR)
# -------------------------------------
# Calculate deaths in neonatal and under-5 periods
neonatal_deaths <- sum(survival_data$neonatal == 1 & survival_data$event == "Event")
under5_deaths <- sum(survival_data$under5 == 1 & survival_data$event == "Event")

# Calculate total number of live births
total_live_births <- nrow(survival_data)

# Neonatal and Under-5 Mortality Rates (per 1,000 live births)
nmr_corrected <- (neonatal_deaths / total_live_births) * 1000
u5mr_corrected <- (under5_deaths / total_live_births) * 1000

cat("\nCorrected Neonatal Mortality Rate (NMR): ", nmr_corrected, " per 1,000 live births\n")
cat("\nCorrected Under-5 Mortality Rate (U5MR): ", u5mr_corrected, " per 1,000 live births\n")

# -------------------------------------
# Step 6: Plot Hazard Ratios with 95% Confidence Intervals (Forest Plot)
# -------------------------------------
# Extract coefficients and confidence intervals
cox_model_summary <- summary(cox_model_svy)
hazard_ratios <- data.frame(
  variable = rownames(cox_model_summary$coefficients),
  HR = cox_model_summary$coefficients[, "exp(coef)"],
  lower_ci = exp(cox_model_summary$coefficients[, "coef"] - 1.96 * cox_model_summary$coefficients[, "robust se"]),
  upper_ci = exp(cox_model_summary$coefficients[, "coef"] + 1.96 * cox_model_summary$coefficients[, "robust se"])
)

# Forest plot for Hazard Ratios
ggplot(hazard_ratios, aes(x = variable, y = HR, ymin = lower_ci, ymax = upper_ci)) +
  geom_pointrange() +
  coord_flip() +
  labs(
    title = "Hazard Ratios with 95% Confidence Intervals",
    x = "Covariates",
    y = "Hazard Ratio (HR)"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  custom_theme()

# -------------------------------------
# Step 7: Plot Distribution of Child Deaths (by Education Level)
# -------------------------------------
# Filter events (child deaths)
death_data <- survival_data %>%
  filter(event == "Event")

# Plot deaths by education level
ggplot(death_data, aes(x = time_years, y = education_level)) +
  geom_jitter(aes(color = HH6), width = 0.1, height = 0.3, alpha = 0.3, size = 1) +
  geom_vline(xintercept = 28 / 30 / 12, color = "red", linetype = "dashed", size = 1, alpha = 0.5) +
  geom_vline(xintercept = 5, color = "red", linetype = "dashed", size = 1, alpha = 0.5) +
  annotate("text", x = (28 / 30 / 12) + 0.2, y = max(as.numeric(death_data$education_level)) + 0.5, label = "Neonatal Mortality", color = "black", size = 4, fontface = "italic") +
  annotate("text", x = 5.2, y = max(as.numeric(death_data$education_level)) + 0.5, label = "Under-5 Mortality", color = "black", size = 4, fontface = "italic") +
  labs(title = "Distribution of Child Deaths Over Time", subtitle = "by Residence and Maternal Education", x = "Age at Death (Years)", y = "Maternal Education", color = "Residence") +
  custom_theme()

# -------------------------------------
# Step 8: Kaplan-Meier Survival Curves by Wealth Quintile and Education Level
# -------------------------------------
# Fit Kaplan-Meier survival curve by wealth index
km_fit_under5 <- survfit(Surv(time_years, event == "Event") ~ windex5, data = death_data)

# Plot Kaplan-Meier survival curve for Wealth Quintiles
ggsurvplot(
  km_fit_under5, data = death_data, conf.int = TRUE, pval = TRUE, risk.table = TRUE, risk.table.col = "strata",
  ggtheme = custom_theme(), xlim = c(0, 5), xlab = "Age at Death (Years)", ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Wealth Quintile", subtitle = "Focusing on Under-Five Mortality",
  legend.title = "Wealth Quintile", risk.table.y.text.col = TRUE, risk.table.height = 0.3
)

# Kaplan-Meier survival curve by education level
km_fit_under5_education <- survfit(Surv(time_years, event == "Event") ~ education_level, data = death_data)

# Plot Kaplan-Meier survival curve for Education Levels
ggsurvplot(
  km_fit_under5_education, data = death_data, conf.int = TRUE, pval = TRUE, risk.table = TRUE,
  ggtheme = custom_theme(), xlim = c(0, 5), xlab = "Age at Death (Years)", ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Education Level", subtitle = "Focusing on Under-Five Mortality",
  legend.title = "Education Level", risk.table.y.text.col = TRUE, risk.table.height = 0.3
)

# -------------------------------------
# Step 9: Re-check Proportional Hazards Assumption with Schoenfeld Residuals
# -------------------------------------
# Schoenfeld residuals test
cox_zph_svy <- cox.zph(cox_model_svy)
print(cox_zph_svy)

# Plot Schoenfeld residuals
schoenfeld_plots <- ggcoxzph(cox_zph_svy, plot = FALSE)
schoenfeld_plots <- lapply(schoenfeld_plots, function(p) { p + custom_theme() })
combined_plot <- wrap_plots(schoenfeld_plots, ncol = 2) + plot_annotation(
  title = "Schoenfeld Residuals for Proportional Hazards Test",
  theme = theme(plot.title = element_text(size = 20, face = "bold", family = font_family, color = custom_colors["primary"]))
)
print(combined_plot)
