# -------------------------------------
# Step 1: Prepare the dataset and select relevant columns
# -------------------------------------
wm_data_filtered_lr <- wm_data_filtered %>%
  select(
    ANC4_any,          # Outcome variable
    windex5,              # Wealth index quintiles
    education_level,      # Education level
    water_access,         # Access to water (binary)
    sanitation_access,    # Access to sanitation (binary)
    HH6,                  # Urban/Rural classification
    wmweight,             # Sample weight
    HH1,                  # Household Number
    HH2,                  # Cluster Number
    p1_code,              # Governorate Code "YE"
    HH7                   # Governorate
  )

# Set survey option for handling single-PSU strata
options(survey.lonely.psu = "adjust")

# -------------------------------------
# Step 2: Create the Stratification Variable
# -------------------------------------
# Combine HH6 (Urban/Rural) and HH7 (Governorate)
wm_data_filtered_lr$stratum <- interaction(wm_data_filtered_lr$HH6, wm_data_filtered_lr$HH7)

# Verify the number of strata
length(unique(wm_data_filtered_lr$stratum))
# Should be up to 44 strata (22 governorates * 2 urban/rural statuses)

# -------------------------------------
# Step 3: Set up the Survey Design
# -------------------------------------
design <- svydesign(
  ids = ~HH1,                         # PSUs (Enumeration Areas)
  strata = ~stratum,                  # Strata (Urban/Rural within Governorates)
  weights = ~wmweight,                # Sample weights
  data = wm_data_filtered_lr,
  nest = TRUE
)

# -------------------------------------
# Step 4: Ensure Outcome Variable is Correctly Coded
# -------------------------------------
# Convert ANC4_any to a factor with "No" as the reference level
wm_data_filtered_lr$ANC4_any <- factor(wm_data_filtered_lr$ANC4_any, levels = c("No", "Yes"))

# -------------------------------------
# Step 5: Fit the Multivariate Logistic Regression Model Including HH7
# -------------------------------------
model <- svyglm(
  ANC4_any ~ windex5 + education_level + water_access + sanitation_access + HH6 + HH7,
  design = design,
  family = quasibinomial()
)

# View the summary of the updated model
summary(model)

# -------------------------------------
# Step 6: Extract Odds Ratios and Confidence Intervals from the Model
# -------------------------------------
tidy_model <- tidy(model, conf.int = TRUE, exponentiate = TRUE)

# -------------------------------------
# Step 7: Clean up and label terms properly
# -------------------------------------
tidy_model <- tidy_model %>%
  filter(term != "(Intercept)", !str_detect(term, "HH7")) %>%  # Remove intercept and governorates (HH7)
  mutate(
    # Create readable term names
    term_named = case_when(
      term == "windex5Quintile 2" ~ "Wealth Quintile 2",
      term == "windex5Quintile 3" ~ "Wealth Quintile 3",
      term == "windex5Quintile 4" ~ "Wealth Quintile 4",
      term == "windex5Quintile 5" ~ "Wealth Quintile 5",
      term == "education_levelLower Secondary Education" ~ "Lower Secondary Education",
      term == "education_levelPrimary Education" ~ "Primary Education",
      term == "education_levelSecondary And Higher" ~ "Secondary And Higher",
      term == "water_access1" ~ "Water Access",
      term == "sanitation_access1" ~ "Sanitation Access",
      term == "HH6Rural" ~ "Urban (Baseline: Rural)",
      TRUE ~ NA_character_
    ),
    category = case_when(
      str_detect(term_named, "Wealth Quintile") ~ "Wealth Index",
      str_detect(term_named, "Education|Secondary") ~ "Education Level",
      TRUE ~ "Other Factors"
    )
  )

# -------------------------------------
# Step 8: Add Baseline Rows (without estimates) for Proper Visualization
# -------------------------------------
baseline_rows <- data.frame(
  term_named = c(
    "Wealth Quintile 1 (Baseline)", 
    "Early Childhood Education (Baseline)" 
  ),
  estimate = NA,  # No estimate for baselines
  conf.low = NA,
  conf.high = NA,
  term_ordered = factor(c(
    "Wealth Quintile 1 (Baseline)", 
    "Wealth Quintile 2", 
    "Wealth Quintile 3", 
    "Wealth Quintile 4", 
    "Wealth Quintile 5",
    "Early Childhood Education (Baseline)", 
    "Primary Education", 
    "Lower Secondary Education", 
    "Secondary And Higher", 
    "Urban (Baseline: Rural)", 
    "Water Access", 
    "Sanitation Access"
  )),
  category = c("Wealth Index", "Education Level")
)

# Bind baseline rows for visualization without estimates
tidy_model <- bind_rows(tidy_model, baseline_rows)

# -------------------------------------
# Step 9: Create an Explicit Order Rank Column
# -------------------------------------
tidy_model <- tidy_model %>%
  mutate(order_rank = case_when(
    term_named == "Wealth Quintile 1 (Baseline)" ~ 1,
    term_named == "Wealth Quintile 2" ~ 2,
    term_named == "Wealth Quintile 3" ~ 3,
    term_named == "Wealth Quintile 4" ~ 4,
    term_named == "Wealth Quintile 5" ~ 5,
    term_named == "Early Childhood Education (Baseline)" ~ 6,
    term_named == "Primary Education" ~ 7,
    term_named == "Lower Secondary Education" ~ 8,
    term_named == "Secondary And Higher" ~ 9,
    term_named == "Urban (Baseline: Rural)" ~ 10,
    term_named == "Water Access" ~ 11,
    term_named == "Sanitation Access" ~ 12,
    TRUE ~ NA_real_
  )) %>% arrange(order_rank)

# -------------------------------------
# Step 10: Plot Odds Ratios with the Custom Theme
# -------------------------------------
ggplot(tidy_model, aes(x = estimate, y = term_named)) +
  # Plot points and error bars only for non-baseline terms
  geom_point(data = filter(tidy_model, !is.na(estimate)), size = 3, color = custom_colors[["primary"]]) + 
  geom_errorbar(data = filter(tidy_model, !is.na(estimate)), aes(xmin = conf.low, xmax = conf.high), 
                width = 0.2, color = custom_colors[["primary"]]) +  
  # Reference line at odds ratio of 1
  geom_vline(xintercept = 1, linetype = "dashed", color = custom_colors[["accent1"]]) +
  # Add text labels for odds ratios
  geom_text(aes(label = ifelse(is.na(estimate), "", round(estimate, 2))), 
            vjust = -0.5, size = 3.5, color = "black") +  
  # Adjust the x-axis range for better visibility
  coord_cartesian(xlim = c(0.5, 10)) +  
  scale_x_log10() +  # Logarithmic scale for odds ratios
  labs(
    title = "Odds Ratios for ANC4_any by Predictor",
    x = "Odds Ratio (Log Scale)",
    y = "Predictor"
  ) +
  facet_grid(category ~ ., scales = "free_y", space = "free") +  # Facet by category
  custom_theme()

# -------------------------------------
# Step 11: Extract Odds Ratios for Governorates (HH7)
# -------------------------------------
tidy_governorates <- tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(str_detect(term, "HH7")) %>%  # Filter only governorate terms (HH7)
  mutate(
    gov_code = as.numeric(gsub("HH7", "", term))  # Extract numeric part of the HH7 code
  )

# -------------------------------------
# Step 12: Join Governorate Data and Replace Code with Names
# -------------------------------------
tidy_governorates <- tidy_governorates %>%
  left_join(pop_data, by = c("gov_code" = "gov_code")) %>%  # Join on 'gov_code' column
  mutate(
    term_named = p1_name  # Replace governorate code with the governorate name (p1_name)
  )

# -------------------------------------
# Step 13: Add Baseline Row for Sana'a City
# -------------------------------------
baseline_row <- data.frame(
  term_named = "Sana'a City (Baseline)",  # Name of the baseline
  estimate = NA,  # No estimate for baseline
  conf.low = NA,
  conf.high = NA,
  gov_code = 13,  # Code for Sana'a City
  stringsAsFactors = FALSE
)

# -------------------------------------
# Step 14: Combine and Ensure Proper Ordering
# -------------------------------------
tidy_governorates <- bind_rows(baseline_row, tidy_governorates) %>%
  arrange(desc(is.na(estimate)), estimate)  # Make sure the baseline is at the top

# -------------------------------------
# Step 15: Plot Odds Ratios for Governorates
# -------------------------------------
ggplot(tidy_governorates, aes(x = estimate, y = reorder(term_named, estimate))) +  # Reorder y-axis by odds ratio
  geom_point(size = 3, color = custom_colors[["primary"]]) +  # Plot points for odds ratios
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = custom_colors[["primary"]]) +  # Error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = custom_colors[["accent1"]]) +  # Reference line at odds ratio = 1
  geom_text(aes(label = round(estimate, 2)), vjust = -0.5, size = 3.5, color = "black") +  # Add text labels for odds ratios
  scale_x_log10() +  # Logarithmic scale for odds ratios
  labs(
    title = "Odds Ratios for Adequate Prenatal Care by Governorate",
    x = "Odds Ratio (Log Scale)",
    y = "Governorate"
  ) +
  custom_theme()

# -------------------------------------
# Step 16: Re-assess Model Fit
# -------------------------------------
# 1. McFadden's Pseudo R-squared
null_model <- svyglm(ANC4_any ~ 1, design = design, family = quasibinomial())
log_lik_full <- as.numeric(logLik(model))
log_lik_null <- as.numeric(logLik(null_model))
pseudo_R2 <- 1 - (log_lik_full / log_lik_null)
cat("McFadden's pseudo R-squared:", round(pseudo_R2, 4), "\n")

# 2. Survey-Weighted ROC Curve and AUC Calculation
wm_data_filtered_lr$predicted_prob <- predict(model, type = "response")
roc_curve <- roc(
  response = wm_data_filtered_lr$ANC4_any,
  predictor = wm_data_filtered_lr$predicted_prob,
  weights = wm_data_filtered_lr$wmweight,
  levels = c("No", "Yes"),
  direction = "<",
  ci = TRUE
)

cat("Weighted AUC:", round(auc(roc_curve), 4), "\n")

# Plot ROC curve
plot(roc_curve, main = "Survey-Weighted ROC Curve")
abline(a = 0, b = 1, lty = 2, col = "red")

# 3. Adjusted Wald Test for Overall Model Significance
anova_result <- regTermTest(
  model,
  ~ windex5 + education_level + water_access + sanitation_access + HH6 + HH7
)
print(anova_result)

# -------------------------------------
# Step 17: Prepare the Data for Mapping
# -------------------------------------
wm_data_map <- tidy_governorates %>%
  mutate(p1_code = ifelse(is.na(p1_code), "YE13", p1_code)) %>%
  left_join(yem_shape, by = c("p1_code" = "ADM1_PCODE"))  # Join odds with shapefile

# -------------------------------------
# Step 18: Plot Governorate-Level Odds Ratios on a Map
# -------------------------------------
ggplot(wm_data_map) +
  geom_sf(aes(geometry = geometry, fill = estimate), color = "black", linewidth = 0.2) +  
  scale_fill_gradient(low = "white", high = custom_colors["primary"], name = "Odds Ratio", na.value = "grey70") +
  labs(
    title = "Governorate-Level Odds Ratios for ANC4_any",
    subtitle = "Logistic Regression Results for ANC4_any by Governorate",
    fill = "Odds Ratio"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.9, 0.4),
    legend.title = element_text(face = "bold", size = 12, color = custom_colors["primary"]),
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(face = "italic", size = 12, margin = margin(b = 10))
  ) +
  guides(fill = guide_colorbar(direction = "vertical", title.position = "top", title.hjust = 0.5))
