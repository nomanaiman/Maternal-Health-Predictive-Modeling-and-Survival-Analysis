# Household Data Processing
hh_data <- hh_src %>%
  filter(HH12 == 1) %>%  # Include only households that have consented
  mutate(
    # Step 2: Create the 'water_access' variable based on improved water, WS3, and WS4
    water_access = case_when(
      WS1 %in% c(11, 12) ~ 1,  # Water piped into dwelling or yard
      WS1 %in% c(13, 14, 21, 31, 41, 51, 61, 71, 91, 92) & WS3 %in% c(1, 2) ~ 1,  # Improved source in dwelling/yard
      WS1 %in% c(13, 14, 21, 31, 41, 51, 61, 71, 91, 92) & WS4 <= 30 ~ 1,  # Fetch time <= 30 mins
      TRUE ~ 0  # Default to false for all other cases
    ) %>% as.factor(),  # Convert water_access to factor
    
    # Create the 'sanitation_access' variable
    sanitation_access = case_when(
      WS11 %in% c(11, 13, 21, 22) & WS15 == 2 ~ 1,  # Improved sanitation, not shared
      WS11 %in% c(14, 18, 23, 41, 95, 96) ~ 0,      # Unimproved sanitation facilities
      WS11 %in% c(11, 13, 21, 22) & WS15 == 1 ~ 0,  # Improved sanitation but shared
      TRUE ~ 0  # Handle missing values
    ) %>% as.factor(),  # Convert sanitation_access to factor
    
    # Factor levels for various variables
    windex5 = factor(windex5, levels = c(1, 2, 3, 4, 5), 
                     labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")),
    p1_code = factor(paste0("YE", sprintf("%02d", HH7))),  # Format HH7 and concatenate with "YE"
    p1_code = relevel(p1_code, ref = "YE13"),  # Set "YE13" as reference level
    HH7 = relevel(factor(HH7), ref = "13"),
    HH6 = factor(HH6, levels = c(1, 2), labels = c("Urban", "Rural")),
    HH6 = relevel(factor(HH6), ref = 2)
  )

# Set labels for the new variables
attr(hh_data$water_access, "label") <- "Basic Drinking Water Services"
attr(hh_data$sanitation_access, "label") <- "Basic Sanitation Services"

# Women's Data Processing
wm_data <- wm_src %>%
  filter(WM9 == 1) %>%  # Filter consenting interviewees
  mutate(
    # Education level
    education_level = case_when(
      WB6AN %in% c(0, 9) ~ "Early Childhood Education",
      WB6AN == 1 ~ "Primary Education",
      WB6AN == 2 ~ "Lower Secondary Education",
      WB6AN >= 3 ~ "Secondary And Higher",
      TRUE ~ "Early Childhood Education"  # Handle missing or unknown values
    ),
    education_level = relevel(factor(education_level), ref = "Early Childhood Education"),
    
    # ANC Skilled Provider and other variables
    ANC_skilled = if_else(MN3A == "A" | MN3B == "B" | MN3C == "C", 1, 0),
    ANC4_any = if_else(replace_na(MN5, 0) >= 4, 1, 0),
    SBA_skilled = if_else(MN19A == "A" | MN19B == "B" | MN19C == "C", 1, 0),
    SBA_facility = if_else(replace_na(MN20, 0) >= 21 & replace_na(MN20, 0) <= 76, 1, 0),
    
    # Convert to factors
    ANC_skilled = factor(ANC_skilled, levels = c(0, 1), labels = c("No", "Yes")),
    ANC4_any = factor(ANC4_any, levels = c(0, 1), labels = c("No", "Yes")),
    SBA_skilled = factor(SBA_skilled, levels = c(0, 1), labels = c("No", "Yes")),
    SBA_facility = factor(SBA_facility, levels = c(0, 1), labels = c("No", "Yes")),
    
    # Factor levels for various variables
    windex5 = factor(windex5, levels = c(1, 2, 3, 4, 5), 
                     labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")),
    HH6 = factor(HH6, levels = c(1, 2), labels = c("Urban", "Rural")),
    p1_code = factor(paste0("YE", sprintf("%02d", HH7))),
    p1_code = relevel(p1_code, ref = "YE13"),  # Set reference level
    HH7 = relevel(factor(HH7), ref = "13")
  ) %>%
  # Join with household data
  left_join(hh_data %>% select(HH1, HH2, water_access, sanitation_access), by = c("HH1", "HH2"))

# Set labels for women's data
attr(wm_data$education_level, "label") <- "Highest Level of Education"
attr(wm_data$ANC_skilled, "label") <- "Prenatal care by skilled provider"
attr(wm_data$ANC4_any, "label") <- "Prenatal care +4 by any provider"
attr(wm_data$SBA_skilled, "label") <- "Skilled Attendance at Birth"
attr(wm_data$SBA_facility, "label") <- "Institutional Delivery"
attr(wm_data$water_access, "label") <- "Basic Drinking Water Services"
attr(wm_data$sanitation_access, "label") <- "Basic Sanitation Services"

# Birth History Data Processing
bh_data <- bh_src %>%
  mutate(
    # Convert CMC to human-readable dates
    month_of_birth = cmc_to_yyyymm(BH4C),
    month_of_death = cmc_to_yyyymm(BH4C + BH9C),
    interview_date = cmc_to_yyyymm(WDOI),
    
    # Calculate years since death/birth
    years_since_death = if_else(!is.na(BH9C) & !is.na(WDOI), round((WDOI - (BH4C + BH9C)) / 12, 2), NA_real_),
    years_since_birth = if_else(!is.na(BH4C) & !is.na(WDOI), round((WDOI - BH4C) / 12, 2), NA_real_),
    
    # Mortality indicators
    mortality_u5 = case_when(
      !is.na(BH9C) & BH9C <= 60 ~ 1,  # Under-5 mortality
      !is.na(BH9C) & BH9C > 60 ~ 0,   # Death after 5 years old
      TRUE ~ NA_real_  # Censored
    ),
    mortality_neonatal = if_else(BH9C == 0, 1, 0),  # Neonatal mortality (death at birth or within the first month)
    
    # Additional variables
    age_at_death_years = if_else(!is.na(BH9C), round(BH9C / 12, 2), NA_real_),
    age_at_interview = WDOI - BH4C,
    period_5_year = case_when(
      age_at_interview <= 60 ~ "0–4 years",
      age_at_interview > 60 & age_at_interview <= 120 ~ "5–9 years",
      age_at_interview > 120 & age_at_interview <= 180 ~ "10–14 years",
      age_at_interview > 180 & age_at_interview <= 240 ~ "15–19 years",
      age_at_interview > 240 & age_at_interview <= 300 ~ "20–24 years",
      age_at_interview > 300 ~ "24+ years",
      TRUE ~ NA_character_
    ),
    year_of_interview = 2022 - (age_at_interview / 12),
    
    # Assign 5-year periods
    period_5_year_categories = case_when(
      year_of_interview >= 2017 & year_of_interview <= 2022 ~ "2017–2022",
      year_of_interview >= 2012 & year_of_interview < 2017 ~ "2012–2017",
      year_of_interview >= 2007 & year_of_interview < 2012 ~ "2007–2012",
      year_of_interview >= 2002 & year_of_interview < 2007 ~ "2002–2007",
      year_of_interview >= 1997 & year_of_interview < 2002 ~ "1997–2002",
      year_of_interview < 1998 ~ "< 1997",
      TRUE ~ NA_character_
    ),
    
    # Convert variables to factors
    mortality_u5 = factor(mortality_u5, levels = c(0, 1), labels = c("No", "Yes")),
    mortality_neonatal = factor(mortality_neonatal, levels = c(0, 1), labels = c("No", "Yes")),
    windex5 = factor(windex5, levels = c(1, 2, 3, 4, 5), 
                     labels = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5")),
    windex5 = relevel(factor(windex5), ref = "Quintile 1"),
    p1_code = factor(paste0("YE", sprintf("%02d", HH7))),
    p1_code = relevel(p1_code, ref = "YE13"),
    HH7 = relevel(factor(HH7), ref = "13"),
    HH6 = factor(HH6, levels = c(1, 2), labels = c("Urban", "Rural")),
    HH6 = relevel(factor(HH6), ref = 2),
    mother_age_at_birth = round((BH4C - WDOB) / 12),
    magebrt = factor(magebrt, levels = c(1, 2, 3), labels = c("<20", "20-34", "35+")),
    magebrt = relevel(factor(magebrt), ref = 1)
  ) %>%
  # Join with household and women's data
  left_join(hh_data %>% select(HH1, HH2, water_access, sanitation_access), by = c("HH1", "HH2")) %>%
  left_join(wm_data %>% select(HH1, HH2, WM3, education_level), by = c("HH1", "HH2", "WM3"))

# Set labels for birth history data
attr(bh_data$mortality_u5, "label") <- "Child death under 5 years"
attr(bh_data$mortality_neonatal, "label") <- "Infant death under 30 days"
attr(bh_data$month_of_birth, "label") <- "Month of birth of child"
attr(bh_data$month_of_death, "label") <- "Month of death of child"
attr(bh_data$interview_date, "label") <- "Interview date"
attr(bh_data$years_since_death, "label") <- "Interview date minus death date"
attr(bh_data$years_since_birth, "label") <- "Interview date minus birth date"
attr(bh_data$age_at_death_years, "label") <- "Age at death in years"
attr(bh_data$age_at_interview, "label") <- "Age at interview CMC"
attr(bh_data$period_5_year, "label") <- "5 year period"
attr(bh_data$mother_age_at_birth, "label") <- "Mother's age at birth"
attr(bh_data$magebrt, "label") <- "Category of mother's age at birth"
attr(bh_data$period_5_year, "label") <- "5 year period"

# Filter dataset for women with live births in the past 2 years
wm_data_filtered <- wm_data %>%
  filter(CM17 >= 1) %>%
  select(HH1, HH2, LN, wmweight, ANC_skilled, ANC4_any, SBA_skilled, SBA_facility, windex5, 
         education_level, water_access, sanitation_access, HH6, p1_code, HH7, WB4)
