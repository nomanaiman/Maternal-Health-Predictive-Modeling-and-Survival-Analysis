# Wealth Quintile Distribution Plot with labels inside the bars
ggplot(hh_data, aes(x = factor(windex5))) +
  geom_bar(fill = custom_colors["primary"], alpha = 0.7) +  # Primary color for bars
  labs(
    title = "Distribution of Wealth Quintiles",
    x = "Wealth Quintile",
    y = "Number of Households"
  ) +
  scale_y_continuous(labels = comma_format()) +  # Format y-axis with commas
  geom_text(
    aes(label = scales::comma(..count..)),  # Add labels with comma formatting
    stat = "count", 
    vjust = 2,  # Position labels inside the bars
    size = 4, 
    color = "white"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Wealth Quintile vs. Wealth Score Distribution
ggplot(hh_data, aes(x = factor(windex5), y = wscore)) +
  geom_boxplot(fill = custom_colors["primary"]) +  # Box plot with primary color
  labs(
    title = "Wealth Quintile vs. Wealth Score Distribution",
    x = "Wealth Quintile",
    y = "Wealth Score"
  ) +
  scale_y_continuous(labels = comma_format()) +  # Format y-axis with commas
  custom_theme()

# Wealth Quintile vs. Antenatal Care (Skilled)
ggplot(wm_data_filtered, aes(x = factor(windex5), fill = ANC_skilled)) +
  geom_bar(position = "fill", alpha = 0.7) +  # Stack and normalize bars
  labs(
    title = "Wealth Quintile vs. ANC Skilled",
    subtitle = "Women Receiving Antenatal Care by a Skilled Provider",
    x = "Wealth Quintile",
    y = "Percentage of Women",
    fill = "ANC Skilled"
  ) +
  scale_fill_manual(
    values = c("No" = custom_colors[["accent1"]], "Yes" = custom_colors[["primary"]])
  ) +
  scale_y_continuous(labels = percent_format()) +  # Format y-axis as percentages
  geom_text(
    aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..], accuracy = 1)),
    stat = "count", 
    position = position_fill(vjust = 0.5),  # Center labels inside the bars
    size = 4, 
    color = "white"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Wealth Quintile vs. ANC4+ Visits
ggplot(wm_data_filtered, aes(x = factor(windex5), fill = ANC4_any)) +
  geom_bar(position = "fill", alpha = 0.7) +  # Stack and normalize bars
  labs(
    title = "Wealth Quintile vs. ANC4",
    subtitle = "Women Receiving Antenatal Care (4+ Visits)",
    x = "Wealth Quintile",
    y = "Percentage of Women",
    fill = "ANC4"
  ) +
  scale_fill_manual(
    values = c("No" = custom_colors[["accent1"]], "Yes" = custom_colors[["primary"]])
  ) +
  scale_y_continuous(labels = percent_format()) +  # Format y-axis as percentages
  geom_text(
    aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..], accuracy = 1)),
    stat = "count", 
    position = position_fill(vjust = 0.5),  # Center labels inside the bars
    size = 4, 
    color = "white"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Wealth Quintile vs. Skilled Birth Attendance (SBA)
ggplot(wm_data_filtered, aes(x = factor(windex5), fill = as.factor(SBA_skilled))) +
  geom_bar(position = "fill", alpha = 0.7) +
  labs(
    title = "Wealth Quintile vs. Skilled Birth Attendance",
    subtitle = "Women Who Had Birth Attended by a Skilled Provider",
    x = "Wealth Quintile",
    y = "Percentage of Women",
    fill = "Skilled Birth Attendance"
  ) +
  scale_fill_manual(
    values = c("No" = custom_colors[["accent1"]], "Yes" = custom_colors[["primary"]])
  ) +
  scale_y_continuous(labels = percent_format()) +  # Format y-axis as percentages
  geom_text(
    aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..], accuracy = 1)),
    stat = "count", 
    position = position_fill(vjust = 0.5),  # Center labels inside the bars
    size = 4, 
    color = "white"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Wealth Quintile vs. Institutional Delivery
ggplot(wm_data_filtered, aes(x = factor(windex5), fill = as.factor(SBA_facility))) +
  geom_bar(position = "fill", alpha = 0.7) +
  labs(
    title = "Wealth Quintile vs. Institutional Delivery",
    subtitle = "Women Who Had Birth in a Health Facility",
    x = "Wealth Quintile",
    y = "Percentage of Women",
    fill = "Institutional Delivery"
  ) +
  scale_fill_manual(
    values = c("No" = custom_colors[["accent1"]], "Yes" = custom_colors[["primary"]])
  ) +
  scale_y_continuous(labels = percent_format()) +  # Format y-axis as percentages
  geom_text(
    aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..], accuracy = 1)),
    stat = "count", 
    position = position_fill(vjust = 0.5),  # Center labels inside the bars
    size = 4, 
    color = "white"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Logistic Regression VIF Check
check_vif <- function(model) {
  vif_values <- vif(model)
  return(vif_values)
}

# Example model and VIF calculation
model_sba_area <- glm(SBA_skilled ~ windex5 + education_level + water_access + sanitation_access + HH6, 
                      family = binomial, data = wm_data)
vif_values <- check_vif(model_sba_area)
print(vif_values)

# Summary Statistics for Continuous and Binary Variables
summary_continuous <- function(data, var_name) {
  cat("\nSummary of", var_name, "\n")
  summary(data[[var_name]])
}

summary_binary <- function(data, var_name) {
  cat("\nSummary of", var_name, "\n")
  table(data[[var_name]], useNA = "ifany")
  prop.table(table(data[[var_name]], useNA = "ifany"))
}

# Wealth Quintile and Household Characteristics Summaries
summary_binary(wm_data, "windex5")
summary_continuous(wm_data, "age_at_interview")
summary_binary(wm_data, "ANC_skilled")
summary_binary(wm_data, "ANC4_any")
summary_binary(wm_data, "SBA_skilled")
summary_binary(wm_data, "SBA_facility")
summary_binary(bh_data, "mortality_u5")
summary_binary(bh_data, "mortality_neonatal")
summary_binary(hh_data, "water_access")
summary_binary(hh_data, "sanitation_access")
summary_binary(hh_data, "HH6")

# Summary of Women's Education Levels
table(wm_data$education_level, useNA = "ifany")
prop.table(table(wm_data$education_level, useNA = "ifany"))

# Check Missing Data
missing_data <- sapply(wm_data, function(x) sum(is.na(x)))
missing_data <- missing_data[missing_data > 0]  # Only show variables with missing data
print(missing_data)

# Filter data for mothers aged 10-45 and create age distribution histogram
filtered_data <- bh_data %>%
  filter(mother_age_at_birth >= 10 & mother_age_at_birth <= 45)

# Histogram of mothers' age at birth
ggplot(filtered_data, aes(x = mother_age_at_birth)) +
  geom_histogram(binwidth = 5, fill = custom_colors[["primary"]], color = "black", alpha = 0.4) +
  stat_bin(binwidth = 5, geom = "text", aes(label = scales::comma(after_stat(count))), vjust = -0.5, color = "black") +
  scale_x_continuous(breaks = seq(10, 45, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of Mothers' Age at Birth (10–45 Years)",
    subtitle = "Grouped in 5-Year Intervals",
    x = "Mother's Age at Birth (Years)",
    y = "Number of Mothers"
  ) +
  custom_theme() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0)
  )

# Maternal Education Levels Distribution with Labels
ggplot(bh_data, aes(x = factor(education_level))) +
  geom_bar(fill = custom_colors["primary"], alpha = 0.7) +
  labs(
    title = "Distribution of Maternal Education Levels",
    x = "Maternal Education Level",
    y = "Number of Mothers"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  geom_text(
    aes(label = scales::comma(..count..)),
    stat = "count",
    vjust = 2,
    size = 4,
    color = "white"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Urban vs Rural Distribution with Labels
ggplot(hh_data, aes(x = factor(HH6))) +
  geom_bar(fill = custom_colors["primary"], alpha = 0.7) +
  labs(
    title = "Urban vs Rural Residence Distribution",
    x = "Residence",
    y = "Number of Households"
  ) +
  scale_y_continuous(labels = scales::comma_format()) +
  geom_text(
    aes(label = scales::comma(..count..)),
    stat = "count",
    vjust = 2,
    size = 4,
    color = "white"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# U5MR by Governorate
u5mr_governorate <- bh_data %>%
  group_by(p1_code) %>%
  summarise(
    All_Births = n(),
    Deaths_U5 = sum(mortality_u5 == "Yes", na.rm = TRUE),
    U5MR = (Deaths_U5 / (All_Births - Deaths_U5)) * 1000
  )

yemen_map <- yem_shape %>%
  left_join(u5mr_governorate, by = c("ADM1_PCODE" = "p1_code"))

# U5MR Map by Governorate
ggplot(yemen_map) +
  geom_sf(aes(fill = U5MR), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", name = "U5MR per 1,000") +
  labs(
    title = "Under-5 Mortality Rate by Governorate in Yemen",
    caption = "Source: [Your Data Source Here]"
  ) +
  custom_theme() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Population-Weighted ANC4 Coverage by Governorate
wm_data_weighted <- wm_data_filtered %>%
  group_by(p1_code) %>%
  summarise(
    weighted_ANC4 = sum((ANC4_any == "Yes") * pop_data$population[pop_data$p1_code == p1_code], na.rm = TRUE) / 
      sum(pop_data$population[pop_data$p1_code == p1_code], na.rm = TRUE),
    weighted_ANC_skilled = sum((ANC_skilled == "Yes") * pop_data$population[pop_data$p1_code == p1_code], na.rm = TRUE) /
      sum(pop_data$population[pop_data$p1_code == p1_code], na.rm = TRUE),
    weighted_SBA_skilled = sum((SBA_skilled == "Yes") * pop_data$population[pop_data$p1_code == p1_code], na.rm = TRUE) /
      sum(pop_data$population[pop_data$p1_code == p1_code], na.rm = TRUE),
    weighted_SBA_facility = sum((SBA_facility == "Yes") * pop_data$population[pop_data$p1_code == p1_code], na.rm = TRUE) /
      sum(pop_data$population[pop_data$p1_code == p1_code], na.rm = TRUE)
  ) %>%
  left_join(yem_shape, by = c("p1_code" = "ADM1_PCODE"))

# Population-Weighted ANC4 Map
ggplot(wm_data_weighted) +
  geom_sf(aes(geometry = geometry, fill = weighted_ANC4), color = "white", size = 0.2) +
  scale_fill_gradient(low = "white", high = custom_colors["primary"], name = "Weighted Proportion") +
  labs(
    title = "Population-Weighted ANC4 Proportion",
    subtitle = "for Women Receiving 4+ Antenatal Care Visits",
    fill = "Proportion"
  ) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.90, 0.4),
    legend.title = element_text(face = "bold", size = 12, color = custom_colors["primary"]),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_colorbar(direction = "vertical", title.position = "top", title.hjust = 0.5))

# U5MR by 5-Year Period and Governorates
mortality_u5_data <- bh_data %>%
  filter(period_5_year_categories != "< 1997") %>%
  group_by(p1_code, period_5_year_categories) %>%
  summarise(
    total_births = sum(wmweight, na.rm = TRUE),
    weighted_deaths_u5 = sum((mortality_u5 == "Yes") * wmweight, na.rm = TRUE),
    u5mr = (weighted_deaths_u5 / total_births) * 1000
  ) %>%
  ungroup()

weighted_mean_u5mr <- mortality_u5_data %>%
  group_by(period_5_year_categories) %>%
  summarise(
    national_weighted_u5mr = sum(weighted_deaths_u5) / sum(total_births) * 1000
  )

mortality_u5_data$Type <- 'Governorates'
weighted_mean_u5mr$Type <- 'National Figure'

combined_u5_data <- bind_rows(
  mortality_u5_data %>% select(period_5_year_categories, u5mr, Type),
  weighted_mean_u5mr %>% rename(u5mr = national_weighted_u5mr) %>% select(period_5_year_categories, u5mr, Type)
)

ggplot(combined_u5_data, aes(x = factor(period_5_year_categories), y = u5mr)) +
  geom_boxplot(
    data = subset(combined_u5_data, Type == 'Governorates'),
    fill = custom_colors["primary"], alpha = 0.5, color = custom_colors["primary"], size = 0.5
  ) +
  geom_point(
    data = subset(combined_u5_data, Type == 'National Figure'),
    aes(shape = Type), color = custom_colors["accent1"], size = 3
  ) +
  geom_text(
    data = subset(combined_u5_data, Type == 'National Figure'),
    aes(label = round(u5mr, 1)), vjust = -1, color = "gray30"
  ) +
  labs(
    title = "Distribution of Under-5 Mortality Rate (U5MR) by 5-Year Period",
    subtitle = "Boxplot showing weighted U5MR distribution across governorates",
    x = "5-Year Period",
    y = "Under-5 Mortality Rate (per 1,000 live births)"
  ) +
  scale_shape_manual(name = '', values = c('National Figure' = 18)) +
  custom_theme() +
  ylim(0, 100) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    legend.position = 'top',
    legend.justification = 'left',
    legend.title = element_blank()
  )

# Neonatal Mortality by 5-Year Period and Governorates
mortality_neonatal_data <- bh_data %>%
  filter(period_5_year_categories != "< 1997") %>%
  group_by(p1_code, period_5_year_categories) %>%
  summarise(
    total_births = sum(wmweight, na.rm = TRUE),
    weighted_deaths_neonatal = sum((mortality_neonatal == "Yes") * wmweight, na.rm = TRUE),
    nmr = (weighted_deaths_neonatal / total_births) * 1000
  ) %>%
  ungroup()

weighted_mean_nmr <- mortality_neonatal_data %>%
  group_by(period_5_year_categories) %>%
  summarise(
    national_weighted_nmr = sum(weighted_deaths_neonatal) / sum(total_births) * 1000
  )

mortality_neonatal_data$Type <- 'Governorates'
weighted_mean_nmr$Type <- 'National Figure'

combined_nmr_data <- bind_rows(
  mortality_neonatal_data %>% select(period_5_year_categories, nmr, Type),
  weighted_mean_nmr %>% rename(nmr = national_weighted_nmr) %>% select(period_5_year_categories, nmr, Type)
)

ggplot(combined_nmr_data, aes(x = factor(period_5_year_categories), y = nmr)) +
  geom_boxplot(
    data = subset(combined_nmr_data, Type == 'Governorates'),
    fill = custom_colors["primary"], alpha = 0.5, color = custom_colors["primary"], size = 0.5
  ) +
  geom_point(
    data = subset(combined_nmr_data, Type == 'National Figure'),
    aes(shape = Type), color = custom_colors["accent1"], size = 3
  ) +
  geom_text(
    data = subset(combined_nmr_data, Type == 'National Figure'),
    aes(label = round(nmr, 1)), vjust = -1, color = "gray30"
  ) +
  labs(
    title = "Distribution of Neonatal Mortality Rate (NMR) by 5-Year Period",
    subtitle = "Boxplot showing weighted NMR distribution across governorates",
    x = "5-Year Period",
    y = "Neonatal Mortality Rate (per 1,000 live births)"
  ) +
  scale_shape_manual(name = '', values = c('National Figure' = 18)) +
  custom_theme() +
  ylim(0, 60) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    legend.position = 'top',
    legend.justification = 'left',
    legend.title = element_blank()
  )
