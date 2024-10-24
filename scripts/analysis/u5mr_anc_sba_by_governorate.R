# -------------------------------------
# Step 1: Calculate U5MR and classify governorates
# -------------------------------------
u5mr_data <- bh_data %>%
  filter(period_5_year == "0–4 years") %>%  # Last 5 years data
  group_by(HH7) %>%
  summarise(
    deaths_u5 = sum(mortality_u5 == "Yes", na.rm = TRUE),
    live_births = n(),
    U5MR = (deaths_u5 / live_births) * 1000
  ) %>%
  mutate(U5MR_status = if_else(U5MR > 30, 1, 0))  # Threshold: 30 per 1,000

# -------------------------------------
# Step 2: Merge U5MR with population and health data (includes ANC_skilled and SBA_facility)
# -------------------------------------
analysis_data <- u5mr_data %>%
  left_join(pop_data %>%
              mutate(gov_code = as.factor(gov_code)), 
            by = c("HH7" = "gov_code")) %>%
  left_join(
    wm_data_filtered %>%
      group_by(HH7) %>%
      summarise(
        ANC4_coverage = mean(ANC4_any == "Yes", na.rm = TRUE),
        SBA_coverage = mean(SBA_skilled == "Yes", na.rm = TRUE),
        ANC_skilled_coverage = mean(ANC_skilled == "Yes", na.rm = TRUE),
        SBA_facility_coverage = mean(SBA_facility == "Yes", na.rm = TRUE)
      ), 
    by = "HH7"
  )

# -------------------------------------
# Step 3: Calculate population-weighted coverage for health indicators
# -------------------------------------
calculate_weighted_coverage <- function(data, coverage_var) {
  sum(data$population * data[[coverage_var]], na.rm = TRUE) / 
    sum(data$population, na.rm = TRUE)
}

# Weighted coverage for ANC4, SBA, ANC_skilled, and SBA_facility
weighted_results <- data.frame(
  Group = c("On-Track", "Off-Track"),
  Weighted_ANC4_Coverage = c(
    calculate_weighted_coverage(analysis_data %>% filter(U5MR_status == 0), 
                                "ANC4_coverage"),
    calculate_weighted_coverage(analysis_data %>% filter(U5MR_status == 1), 
                                "ANC4_coverage")
  ),
  Weighted_SBA_Coverage = c(
    calculate_weighted_coverage(analysis_data %>% filter(U5MR_status == 0), 
                                "SBA_coverage"),
    calculate_weighted_coverage(analysis_data %>% filter(U5MR_status == 1), 
                                "SBA_coverage")
  ),
  Weighted_ANC_Skilled_Coverage = c(
    calculate_weighted_coverage(analysis_data %>% filter(U5MR_status == 0), 
                                "ANC_skilled_coverage"),
    calculate_weighted_coverage(analysis_data %>% filter(U5MR_status == 1), 
                                "ANC_skilled_coverage")
  ),
  Weighted_SBA_Facility_Coverage = c(
    calculate_weighted_coverage(analysis_data %>% filter(U5MR_status == 0), 
                                "SBA_facility_coverage"),
    calculate_weighted_coverage(analysis_data %>% filter(U5MR_status == 1), 
                                "SBA_facility_coverage")
  )
)

# -------------------------------------
# Step 4: Perform weighted t-tests for health indicators
# -------------------------------------
anc4_t_test <- wtd.t.test(
  x = analysis_data$ANC4_coverage[analysis_data$U5MR_status == 0],
  y = analysis_data$ANC4_coverage[analysis_data$U5MR_status == 1],
  weight = analysis_data$population[analysis_data$U5MR_status == 0],
  weighty = analysis_data$population[analysis_data$U5MR_status == 1]
)

sba_t_test <- wtd.t.test(
  x = analysis_data$SBA_coverage[analysis_data$U5MR_status == 0],
  y = analysis_data$SBA_coverage[analysis_data$U5MR_status == 1],
  weight = analysis_data$population[analysis_data$U5MR_status == 0],
  weighty = analysis_data$population[analysis_data$U5MR_status == 1]
)

anc_skilled_t_test <- wtd.t.test(
  x = analysis_data$ANC_skilled_coverage[analysis_data$U5MR_status == 0],
  y = analysis_data$ANC_skilled_coverage[analysis_data$U5MR_status == 1],
  weight = analysis_data$population[analysis_data$U5MR_status == 0],
  weighty = analysis_data$population[analysis_data$U5MR_status == 1]
)

sba_facility_t_test <- wtd.t.test(
  x = analysis_data$SBA_facility_coverage[analysis_data$U5MR_status == 0],
  y = analysis_data$SBA_facility_coverage[analysis_data$U5MR_status == 1],
  weight = analysis_data$population[analysis_data$U5MR_status == 0],
  weighty = analysis_data$population[analysis_data$U5MR_status == 1]
)

# Print t-test results
print(anc4_t_test)
print(sba_t_test)
print(anc_skilled_t_test)
print(sba_facility_t_test)

# -------------------------------------
# Step 5: Prepare data for visualization
# -------------------------------------
coverage_summary <- analysis_data %>%
  group_by(U5MR_status) %>%
  summarise(
    Weighted_ANC4_Coverage = sum(population * ANC4_coverage, na.rm = TRUE) / 
      sum(population, na.rm = TRUE),
    Weighted_SBA_Coverage = sum(population * SBA_coverage, na.rm = TRUE) / 
      sum(population, na.rm = TRUE),
    Weighted_ANC_Skilled_Coverage = sum(population * ANC_skilled_coverage, 
                                        na.rm = TRUE) / 
      sum(population, na.rm = TRUE),
    Weighted_SBA_Facility_Coverage = sum(population * SBA_facility_coverage, 
                                         na.rm = TRUE) / 
      sum(population, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Weighted_ANC4_Coverage, Weighted_SBA_Coverage,
                        Weighted_ANC_Skilled_Coverage, Weighted_SBA_Facility_Coverage),
               names_to = "indicator", values_to = "Weighted_Coverage") %>%
  mutate(
    u5mr_classification = if_else(U5MR_status == 1, "off-track", "on-track"),
    indicator = case_when(
      str_detect(indicator, "ANC4") ~ "ANC4",
      str_detect(indicator, "SBA_Coverage") ~ "SBA Skilled",
      str_detect(indicator, "ANC_Skilled") ~ "ANC Skilled",
      str_detect(indicator, "SBA_Facility") ~ "SBA Facility"
    )
  ) %>%
  replace_na(list(Weighted_Coverage = 0))  # Replace NA values with 0 for visualization

# -------------------------------------
# Step 6: Bar chart for Health Coverage by U5MR Classification
# -------------------------------------
ggplot(coverage_summary, 
       aes(x = indicator, y = Weighted_Coverage, fill = u5mr_classification)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE, 
           alpha = 0.4) +
  scale_fill_manual(values = c("on-track" = custom_colors[["primary"]], 
                               "off-track" = custom_colors[["accent1"]])) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Health Coverage by Indicator and U5MR Status", 
       x = "Health Indicator", y = "Weighted Coverage (%)", fill = "U5MR Status") +
  geom_text(aes(label = percent(Weighted_Coverage, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  custom_theme() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )

# -------------------------------------
# Step 7: Jitter plot for all indicators
# -------------------------------------
weighted_averages_long <- analysis_data %>%
  select(U5MR_status, ANC4_coverage, SBA_coverage, ANC_skilled_coverage, 
         SBA_facility_coverage, population) %>%
  pivot_longer(cols = c(ANC4_coverage, SBA_coverage, 
                        ANC_skilled_coverage, SBA_facility_coverage), 
               names_to = "indicator", values_to = "value") %>%
  mutate(
    u5mr_classification = if_else(U5MR_status == 1, "off-track", "on-track"),
    indicator = case_when(
      str_detect(indicator, "ANC4") ~ "ANC4",
      str_detect(indicator, "SBA_coverage") ~ "SBA Skilled",
      str_detect(indicator, "ANC_skilled") ~ "ANC Skilled",
      str_detect(indicator, "SBA_facility") ~ "SBA Facility"
    )
  ) %>%
  replace_na(list(value = 0))  # Handle any NA values

# Plot jitter for all indicators
ggplot(weighted_averages_long, 
       aes(x = indicator, y = value, color = u5mr_classification)) +
  geom_point(aes(x = ifelse(u5mr_classification == "on-track", as.numeric(factor(indicator)) + 0.1, 
                            as.numeric(factor(indicator)) - 0.1)), 
             size = 3, alpha = 0.6) +  # Manual adjustment for on/off-track
  scale_y_continuous(labels = percent_format(scale = 100), limits = c(0, 1)) +
  scale_x_continuous(breaks = unique(as.numeric(factor(weighted_averages_long$indicator))),
                     labels = unique(weighted_averages_long$indicator)) +  # Correct labels for indicators
  scale_color_manual(values = c("off-track" = custom_colors[["accent1"]], 
                                "on-track" = custom_colors[["primary"]])) +
  labs(title = "Coverage Distribution by Indicator and U5MR Status", 
       x = "Indicator", y = "Coverage (%)", color = "U5MR Classification") +  # Update legend title
  custom_theme()

# -------------------------------------
# Step 8: Boxplot for all indicators by U5MR classification
# -------------------------------------
ggplot(data = weighted_averages_long,  
       mapping = aes(x = indicator, y = value, fill = u5mr_classification)) +  
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, 
               position = position_dodge(0.75), alpha = 0.5) +  
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +  
  scale_fill_manual(values = c("on-track" = custom_colors[["primary"]], 
                               "off-track" = custom_colors[["accent1"]])) +  
  labs(title = "Boxplot of Coverage by U5MR Classification", 
       x = "Health Indicator", y = "Coverage (%)", fill = "U5MR Classification") +  
  geom_vline(xintercept = seq(1.5, length(unique(weighted_averages_long$indicator)) - 0.5, by = 1), 
             color = "gray", linetype = "solid", size = 0.5) +  
  custom_theme()

# -------------------------------------
# Step 9: Scatter plot for coverage vs population with log scale
# -------------------------------------
ggplot(weighted_averages_long, 
       aes(x = value * 100, y = population, color = u5mr_classification)) +
  geom_point(shape = 16, alpha = 0.5, size = 3) +
  scale_x_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = c("off-track" = custom_colors[["accent1"]], 
                                "on-track" = custom_colors[["primary"]])) +
  facet_wrap(~indicator, scales = "free_y", nrow = 4) +
  labs(title = "Coverage vs Population (Log Scale)", 
       x = "Coverage (%)", y = "Population (Log Scale)", color = "U5MR Status") +
  custom_theme() + theme(legend.position = "bottom", panel.spacing = unit(1, "lines"))

# -------------------------------------
# Step 10: Create maps for each indicator with U5MR overlay
# -------------------------------------
map_data <- yem_shape %>%
  left_join(analysis_data, by = c("ADM1_PCODE" = "p1_code"))

create_coverage_map <- function(map_data, indicator_col, u5mr_col, title) {
  ggplot(map_data) +  
    geom_sf(aes(fill = .data[[indicator_col]]), color = NA) +  
    geom_sf(data = map_data %>% filter(.data[[u5mr_col]] == 1),  
            fill = NA, color = "red", size = 1, linetype = "dashed") +
    geom_sf(data = map_data %>% filter(.data[[u5mr_col]] == 0),  
            fill = NA, color = "green", size = 1, linetype = "solid") +
    scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white",
                        name = "Coverage %") +
    labs(title = title, fill = "Coverage") +
    theme_minimal() +
    theme(legend.position = "right")
}

# Map for ANC4 Coverage
anc4_map <- create_coverage_map(map_data, "ANC4_coverage", "U5MR_status", 
                                "Antenatal Care (4+ Visits) Coverage by Governorate")
print(anc4_map)

# Map for SBA Skilled Coverage
sba_skilled_map <- create_coverage_map(map_data, "SBA_coverage", "U5MR_status", 
                                       "Skilled Birth Attendance (SBA) Coverage by Governorate")
print(sba_skilled_map)

# Map for ANC Skilled Coverage
anc_skilled_map <- create_coverage_map(map_data, "ANC_skilled_coverage", "U5MR_status", 
                                       "ANC Skilled Coverage by Governorate")
print(anc_skilled_map)

# Map for SBA Facility Coverage
sba_facility_map <- create_coverage_map(map_data, "SBA_facility_coverage", "U5MR_status", 
                                        "Facility-Based Delivery Coverage by Governorate")
print(sba_facility_map)
