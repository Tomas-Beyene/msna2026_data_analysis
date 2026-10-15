#  -----------------------------------------------------------------------------
#
# Title : Analysis - Preparation Analysis Dataset
#    By : IMPACT NGA
#  Date : 09/2026
#    
#  -----------------------------------------------------------------------------

# Clear environment -----------------------------------------------------------
rm(list=ls())

# Resources path -------------------------------------------------------------
kobo_tool <- "./resources/kobo_for_analysis.xlsx"
dap <- "resources/data_analysis_plan.xlsx"
cluster_table_file_path <- "resources/R1_cluster_table_NE_NW.xlsx"
clean_data_path <- "./input/IMPACT_NGA_Dataset_MSNA-UNHCR-2026-Round1_08102026_v5.xlsx"

# Load src scripts -------------------------------------------------------------
source("src/init.R")

# Load Data --------------------------------------------------------------------

# clean and validated data 

# data <- read_multisheet_raw_data()
# 
# 
# hh_a <- data[["hh_clean_data"]]
# roster_a <- data[["roster_clean_data"]]
# wgq_a <- data[["wgss_clean_data"]]
# health_a <- data[["health_clean_data"]]
# nut_a <- data[["nut_clean_data"]]
# edu_a <- data[["edu_clean_data"]]
# prot_a <- data[["prot_clean_data"]]

hh_a <- read_multisheet_raw_data(clean_data_path,sheets = "hh_clean_data")[[1]]
roster_a <- read_multisheet_raw_data(clean_data_path,sheets = "roster_clean_data")[[1]]
wgq_a <- read_multisheet_raw_data(clean_data_path,sheets = "wgss_clean_data")[[1]]
health_a <- read_multisheet_raw_data(clean_data_path,sheets = "health_clean_data")[[1]]
nut_a <- read_multisheet_raw_data(clean_data_path,sheets = "nut_clean_data")[[1]]
edu_a <- read_multisheet_raw_data(clean_data_path,sheets = "edu_clean_data")[[1]]
prot_a <- read_multisheet_raw_data(clean_data_path,sheets = "prot_clean_data")[[1]]

hh_data <- hh_a
roster <- roster_a
wgq <- wgq_a
health <- health_a
nut <- nut_a
edu <- edu_a
prot <- prot_a


# Convert ----------------------------------------------------------------------

# - convert "integer" columns to numeric
# - convert "date" columns to Date
# - also check if the conversion added any NA values to the dataset

#  ------------------------ HH
NA0 <- sum(is.na(hh_data))
for (col in colnames(hh_data)) hh_data[[col]] <- convertColumnType(hh_data, col, tool_survey, tool_choices)

NA1 <- sum(is.na(hh_data))
if(NA0 != NA1) stop("NA added!")

# ------------------------ roster 
NA0 <- sum(is.na(roster))
for (col in colnames(roster)) roster[[col]] <- convertColumnType(roster, col, tool_survey, tool_choices)

NA1 <- sum(is.na(roster))
if(NA0 != NA1) stop("NA added!")

# ------------------------ WGQ
NA0 <- sum(is.na(wgq))
for (col in colnames(wgq)) wgq[[col]] <- convertColumnType(wgq, col, tool_survey, tool_choices)

NA1 <- sum(is.na(wgq))
if(NA0 != NA1) stop("NA added!")

# ------------------------ health 
NA0 <- sum(is.na(health))
for (col in colnames(health)) health[[col]] <- convertColumnType(health, col, tool_survey, tool_choices)

NA1 <- sum(is.na(health))
if(NA0 != NA1) stop("NA added!")

# ------------------------ nutrition 
NA0 <- sum(is.na(nut))
for (col in colnames(nut)) nut[[col]] <- convertColumnType(nut, col, tool_survey, tool_choices)

NA1 <- sum(is.na(nut))
if(NA0 != NA1) stop("NA added!")

# ------------------------ education 
NA0 <- sum(is.na(edu))
for (col in colnames(edu)) edu[[col]] <- convertColumnType(edu, col, tool_survey, tool_choices)

NA1 <- sum(is.na(edu))
if(NA0 != NA1) stop("NA added!")

# ------------------------ protection  
NA0 <- sum(is.na(prot))
for (col in colnames(prot)) prot[[col]] <- convertColumnType(prot, col, tool_survey, tool_choices)
prot$prot_ind_age <- as.numeric(prot$prot_ind_age)

NA1 <- sum(is.na(prot))
if(NA0 != NA1) stop("NA added!")

################################################################################
# COMPOSITE INDICATORS ---------------------------------------------------------
################################################################################

#########################################
# Main Dataset (HH level) Disaggregation
#########################################

# add the disaggregation level: MSNA-wide, State, LGA

hh_data <- hh_data %>%
  mutate(
    `MSNA-wide` = "MSNA-wide",
    State = paste0(admin1, " - ", admin1_name),
    LGA = paste0(admin2, " - ", admin2_name),
    Ward = paste0(admin3, " - ", admin3_name),
    )%>%
  relocate(`MSNA-wide`, .after = admin0) %>%
  relocate(State, .after = admin1_name) %>%
  relocate(LGA, .after = admin2_name) %>%
  relocate(Ward, .after = admin3_name)


hh_data <- hh_data %>%
  mutate(
    Strata = case_when(
      pop_group == "non_idp" ~ paste("Non IDP", LGA, sep = "-"),
      pop_group == "idp" ~ paste("IDP", LGA, sep = "-"),
      pop_group == "refugee" ~ "",
      TRUE ~ ""
    ),
    `State - Population Group` = case_when(
      pop_group == "non_idp" ~ paste("Non IDP", LGA, sep = "-"),
      pop_group == "idp" ~ paste("IDP", LGA, sep = "-"),
      TRUE ~ ""
    )
  ) %>%
  relocate(Strata, .after = uuid)

#  -----------------------------------------------------------------------------
# combine respondent and hoh age and gender when the hoh is not the respondent
hh_data <- hh_data %>%
  humind::add_hoh_final()

# add the disaggregation level: Gender of HoH
hh_data <- hh_data %>%
  mutate(`Gender of HoH` = ifelse(hoh_gender == "female", "Female-Headed Household", "Male-Headed Household"),
         `State - Gender of HoH` = paste0(State, " - ", `Gender of HoH`), 
         `LGA - Gender of HoH` = paste0(LGA, " - ", `Gender of HoH`), .before = setting)

#  -----------------------------------------------------------------------------
#' add the disaggregation level: Age of HoH   # This assume the *hoh_age* can't be less than @18 years old.
hh_data <- hh_data %>%
  mutate(`Age of HoH` = ifelse(as.numeric(hoh_age) >= 18 &  as.numeric(hoh_age) <= 59, "Head of Household 18-59 years", "Head of Household 60+ years"),
         `State - Age of HoH` = paste0(State, " - ", `Age of HoH`),
         `LGA - Age of HoH` = paste0(LGA, " - ", `Age of HoH`), .before = setting)

#  -----------------------------------------------------------------------------
# add the disaggregation level: Age & Gender of HoH
hh_data <- hh_data %>%
  mutate(`Age & Gender HoH` = paste0(`Gender of HoH`, " - ", `Age of HoH`), .before = setting) %>%
  mutate(`Age & Gender HoH` = case_when(
    `Age & Gender HoH` == "Female-Headed Household - Head of Household 18-59 years" ~ "Female-Headed Household aged 18-59 years",
    `Age & Gender HoH` == "Female-Headed Household - Head of Household 60+ years" ~ "Female-Headed Household aged 60+ years",
    `Age & Gender HoH` == "Male-Headed Household - Head of Household 18-59 years" ~ "Male-Headed Household aged 18-59 years",
    `Age & Gender HoH` == "Male-Headed Household - Head of Household 60+ years" ~ "Male-Headed Household aged 60+ years",
  )) %>%
  mutate(`State - Age & Gender HoH` = paste0(State, " - ", `Age & Gender HoH`),
         `LGA - Age & Gender HoH` = paste0(LGA, " - ", `Age & Gender HoH`), .before = setting)
  
#  -----------------------------------------------------------------------------
# add the disaggregation level: Setting
hh_data <- hh_data %>%
  mutate(Setting = case_when(
    setting == "rural" ~ "Rural",
    setting == "urban" ~ "Urban or peri-urban",
    setting == "camp_informal" ~ "Informal camp or site setting",
    setting == "camp_formal" ~ "Formal camp or site setting",
    TRUE ~ NA 
  ), .before = hh_size)

hh_data <- hh_data %>%
  mutate(`State - Setting` = paste0(State, " - ", Setting),
         `LGA - Setting` = paste0(LGA, " - ", Setting), .before = hh_size)
    

#  -----------------------------------------------------------------------------
# add the disaggregation level: Presence of a member with a disability within the HH
# disability cut-off 2: any domain coded as "a lot of difficulty" or "cannot do at all" or "some_difficulty".


# first add column Member with disability - Level 2 to the wgq dataset then merge it to hh_data 
dis.choices = c("a_lot_of_difficulty", "cannot_do_at_all", "some_difficulty")
wgq <- wgq %>%
mutate(`Member with disability - Level 2` = ifelse(( wgq_vision %in% dis.choices |
                                                     wgq_hearing %in% dis.choices |
                                                     wgq_mobility %in% dis.choices |
                                                     wgq_cognition %in% dis.choices |
                                                     wgq_self_care %in% dis.choices |
                                                     wgq_communication %in% dis.choices), "Disability reported", "No disability reported"),
                                                        .after = wgq_communication)
#  -----------------------------------------------------------------------------
# add the disaggregation level: Presence of a member with a disability within the HH
# disability cut-off 3: any domain coded as "a lot of difficulty" or "cannot do at all".


# first add column Member with disability - Level 3 to the roster dataset then merge it to hh_data 
dis.choices = c("a_lot_of_difficulty", "cannot_do_at_all")
wgq <- wgq %>%
  mutate(`Member with disability - Level 3` = ifelse(
                                                  (wgq_vision %in% dis.choices |
                                                   wgq_hearing %in% dis.choices |
                                                   wgq_mobility %in% dis.choices |
                                                   wgq_cognition %in% dis.choices |
                                                   wgq_self_care %in% dis.choices |
                                                   wgq_communication %in% dis.choices), "Disability reported", "No disability reported"),
                                                     .after = `Member with disability - Level 2`)

# join columns from roster to hh data
dis_data <- wgq %>%
  group_by(uuid) %>%
  summarise(`Household with disability - Level 3` = sum(`Member with disability - Level 3` == "Disability reported", na.rm = T)) %>% 
  ungroup() %>% 
  mutate(`Household with disability - Level 3` = ifelse(`Household with disability - Level 3` > 0, "Disability in the household reported", "No disability in the household reported"))

hh_data <- hh_data %>%
  left_join(dis_data) %>%
  relocate(`Household with disability - Level 3`, .before = dis_forced) %>%
  mutate(`State - Household with disability - Level 3` = paste0(State, " - ", `Household with disability - Level 3`),
         `LGA - Household with disability - Level 3` = paste0(LGA, " - ", `Household with disability - Level 3`), .before = dis_forced)

#  -----------------------------------------------------------------------------
# add the disaggregation level: State - Population Group
hh_data <- hh_data %>%
  mutate(
    `State - Population Group` = paste0(State, " - ", pop_group),
    `State - Population Group & Gender of HoH` = paste0(State, " - ", pop_group, " - ", `Gender of HoH`),
    `State - Population Group & Age of HoH` = paste0(State, " - ", pop_group, " - ", `Age of HoH`),
    `State - Population Group & Setting` = paste0(State, " - ", pop_group, " - ", Setting),
    `State - Population Group & Household with disability - Level 3` = paste0(State, " - ", pop_group, " - ", `Household with disability - Level 3`),
    `State - Population Group & Age & Gender HoH` = paste0(State, " - ", pop_group, " - ", `Age & Gender HoH`), .before = hh_size)

#  -----------------------------------------------------------------------------
# add the disaggregation level: Strata - Population Group
hh_data <- hh_data %>%
  mutate(
    `Strata - Gender of HoH` = paste0(Strata, " - ", pop_group, " - ", `Gender of HoH`),
    `Strata - Age of HoH` = paste0(Strata, " - ", pop_group, " - ", `Age of HoH`),
    `Strata - Setting` = paste0(Strata, " - ", pop_group, " - ", Setting),
    `Strata - Household with disability - Level 3` = paste0(Strata, " - ", pop_group, " - ", `Household with disability - Level 3`),
    `Strata - Age & Gender HoH` = paste0(Strata, " - ", pop_group, " - ", `Age & Gender HoH`), .before = hh_size)


#  -----------------------------------------------------------------------------
# split the string column fsl_source_food_ranked (space-separated values) into three ordered columns:
# fsl_source_food_ranked_first
# fsl_source_food_ranked_second
# fsl_source_food_ranked_third

hh_data <- hh_data %>%
  separate(fsl_source_food_ranked,
           into = c("fsl_source_food_ranked_first",
                    "fsl_source_food_ranked_second",
                    "fsl_source_food_ranked_third"),
           sep = " ",       # split by space
           fill = "right",  # if fewer than 3 parts, fill with NA
           extra = "drop",  # if there are more than 3 items, only the first 3 are kept (the rest are ignored).
           remove = FALSE) %>% 
  relocate(fsl_source_food_ranked_first:fsl_source_food_ranked_third,
           .after = fsl_source_food_ranked)

#  -----------------------------------------------------------------------------
# Add FSL Indicators

hh_data <- hh_data %>%
#   add_fcs(   # Food Consumption Score (FCS)
#   cutoffs = "normal",
#   fsl_fcs_cereal = "fsl_fcs_cereal",
#   fsl_fcs_legumes = "fsl_fcs_legumes",
#   fsl_fcs_veg = "fsl_fcs_veg",
#   fsl_fcs_fruit = "fsl_fcs_fruit",
#   fsl_fcs_meat = "fsl_fcs_meat",
#   fsl_fcs_dairy = "fsl_fcs_dairy",
#   fsl_fcs_sugar = "fsl_fcs_sugar",
#   fsl_fcs_oil = "fsl_fcs_oil"
# ) %>% add_hhs(         # Household Hunger Scale (HHS)
#   fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh",
#   fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
#   fsl_hhs_sleephungry = "fsl_hhs_sleephungry",
#   fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
#   fsl_hhs_alldaynight = "fsl_hhs_alldaynight",
#   fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
#   yes_answer = "yes",
#   no_answer = "no",
#   rarely_answer = "rarely",
#   sometimes_answer = "sometimes",
#   often_answer = "often"
# ) %>% add_lcsi(           # Livelihood Coping Strategy score (LCSI)
#   fsl_lcsi_stress1 = "fsl_lcsi_stress1",
#   fsl_lcsi_stress2 = "fsl_lcsi_stress2",
#   fsl_lcsi_stress3 = "fsl_lcsi_stress3",
#   fsl_lcsi_stress4 = "fsl_lcsi_stress4",
#   fsl_lcsi_crisis1 = "fsl_lcsi_crisis1",
#   fsl_lcsi_crisis2 = "fsl_lcsi_crisis2",
#   fsl_lcsi_crisis3 = "fsl_lcsi_crisis3",
#   fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
#   fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
#   fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
#   yes_val = "yes",
#   no_val = "no_had_no_need",
#   exhausted_val = "no_exhausted",
#   not_applicable_val = "not_applicable"
#   ) %>% add_rcsi(        # Reduced Household Coping Strategy score (rCSI)
#   fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
#   fsl_rcsi_borrow = "fsl_rcsi_borrow",
#   fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
#   fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
#   fsl_rcsi_mealnb = "fsl_rcsi_mealnb"
#   ) %>%
  add_fcm_phase(              # Food Consumption Matrix (FCM)
    fcs_column_name = "fsl_fcs_cat",
    rcsi_column_name = "fsl_rcsi_cat",
    hhs_column_name = "fsl_hhs_cat_ipc",
    fcs_categories_acceptable = "Acceptable",
    fcs_categories_poor = "Poor",
    fcs_categories_borderline = "Borderline",
    rcsi_categories_low = "No to Low",
    rcsi_categories_medium = "Medium",
    rcsi_categories_high = "High",
    hhs_categories_none = "None",
    hhs_categories_little = "Little",
    hhs_categories_moderate = "Moderate",
    hhs_categories_severe = "Severe",
    hhs_categories_very_severe = "Very Severe"
  ) %>%
  add_fclcm_phase(           # Calculating FEWSNET Food Consumption-Livelihood Coping Matrix
    fc_phase_var = "fsl_fc_phase",
    fc_phase_1 = "Phase 1 FC",
    fc_phase_2 = "Phase 2 FC",
    fc_phase_3 = "Phase 3 FC",
    fc_phase_4 = "Phase 4 FC",
    fc_phase_5 = "Phase 5 FC",
    lcs_cat_var = "fsl_lcsi_cat",
    lcs_cat_none = "None",
    lcs_cat_stress = "Stress",
    lcs_cat_crisis = "Crisis",
    lcs_cat_emergency = "Emergency")


cols <- c("fcs_weight_cereal1", "fcs_weight_legume2", "fcs_weight_dairy3",
               "fcs_weight_meat4", "fcs_weight_veg5", "fcs_weight_fruit6",
               "fcs_weight_oil7", "fcs_weight_sugar8", "fsl_fcs_score", "fsl_fcs_cat")

hh_data <- hh_data %>%
  relocate(all_of(cols), .after = fsl_fcs_condiments) 


cols <- c("fsl_hhs_nofoodhh_recoded", "fsl_hhs_nofoodhh_freq_recoded", "fsl_hhs_sleephungry_recoded",
          "fsl_hhs_alldaynight_recoded", "fsl_hhs_sleephungry_freq_recoded", "fsl_hhs_alldaynight_freq_recoded",
          "fsl_hhs_comp1", "fsl_hhs_comp2", "fsl_hhs_comp3", "fsl_hhs_score", "fsl_hhs_cat_ipc", "fsl_hhs_cat")

hh_data <- hh_data %>%
  relocate(all_of(cols), .after = fsl_hhs_alldaynight_freq) 

cols <- c("fsl_lcsi_stress_yes", "fsl_lcsi_stress_exhaust", "fsl_lcsi_stress", "fsl_lcsi_crisis_yes",
          "fsl_lcsi_crisis_exhaust", "fsl_lcsi_crisis", "fsl_lcsi_emergency_yes",
          "fsl_lcsi_emergency_exhaust", "fsl_lcsi_emergency", "fsl_lcsi_cat_yes",
          "fsl_lcsi_cat_exhaust", "fsl_lcsi_cat")

hh_data <- hh_data %>%
  relocate(all_of(cols), .after = fsl_lcsi_emergency4) 


cols <- c("rcsi_lessquality_weighted", "rcsi_borrow_weighted", "rcsi_mealsize_weighted",
          "rcsi_mealadult_weighted", "rcsi_mealnb_weighted", "fsl_rcsi_score",
          "fsl_rcsi_cat", "fsl_fc_cell", "fclcm_phase", "fsl_fc_phase")

hh_data <- hh_data %>%
  relocate(all_of(cols), .after = fsl_rcsi_mealnb) 

#  -----------------------------------------------------------------------------
# Quantile Intervals to Income

hh_data <- hh_data %>%
  add_quantile_interval(
    "cm_income_total",
    cut_offs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    weight = NULL
  ) %>%
  relocate(cm_income_total_qtl, .after = cm_income_total)

#  -----------------------------------------------------------------------------
# Expenditure

freq_expenditure_cols= c("cm_expenditure_frequent_food", "cm_expenditure_frequent_rent", "cm_expenditure_frequent_water", 
                         "cm_expenditure_frequent_nfi", "cm_expenditure_frequent_utilities", "cm_expenditure_frequent_fuel", 
                         "cm_expenditure_frequent_transportation", "cm_expenditure_frequent_communication", "cm_expenditure_frequent_other")

infreq_expenditure_cols = c("cm_expenditure_infrequent_shelter","cm_expenditure_infrequent_clothing","cm_expenditure_infrequent_nfi",
                            "cm_expenditure_infrequent_health","cm_expenditure_infrequent_education","cm_expenditure_infrequent_debt", 
                            "cm_expenditure_infrequent_other")

#  -----------------------------------------------------------------------------
# Rank Top 3 Frequent & Infrequent Expenditure Types
hh_data <- hh_data %>%
  add_expenditure_type_freq_rank(
    expenditure_freq_types = freq_expenditure_cols
  ) %>%
  add_expenditure_type_infreq_rank(
    expenditure_infreq_types = infreq_expenditure_cols
  )

# Relocate the new columns to be after the respective expenditure columns
hh_data <- hh_data %>%
  relocate(cm_freq_expenditure_top1:cm_freq_expenditure_top3, .after = cm_expenditure_frequent_other) %>%
  relocate(cm_infreq_expenditure_top1:cm_infreq_expenditure_top3, .after = cm_expenditure_infrequent_other)

#  -----------------------------------------------------------------------------
# Total Monthly Expenditure

hh_data <- hh_data %>%
  mutate(
    exp_unknown = `cm_expenditure_frequent.dnk` == 1 | `cm_expenditure_frequent.pnta` == 1 |
      `cm_expenditure_infrequent.dnk` == 1 | `cm_expenditure_infrequent.pnta` == 1 |
      is.na(cm_expenditure_frequent) | is.na(cm_expenditure_infrequent),
    total_expenditure = if_else(exp_unknown, NA_real_,
                                rowSums(across(all_of(freq_expenditure_cols)), na.rm = TRUE) +
                                  (rowSums(across(all_of(infreq_expenditure_cols)), na.rm = TRUE) / 6)), # infrequesnt expenditures are asked for 6 month so we've to divide
    .before = cm_income_sources_reduced_1) %>%
  select(-exp_unknown)

#  -----------------------------------------------------------------------------
# Percentage of HHs for which total HH expenditure > total monetary HH income over the last 30 days

hh_data <- hh_data %>%
  mutate(
    expenditure_gt_income = case_when(
      is.na(total_expenditure) | is.na(cm_income_total) ~ NA,
      total_expenditure > cm_income_total ~ "Yes",
      TRUE ~ "No"
    ), .after = total_expenditure
  )

#  -----------------------------------------------------------------------------
# Income source Category

hh_data <- hh_data %>%
  mutate(
    income_source_category = case_when(
      cm_income_sources_reduced_1 %in% c("regular", "own_business", "own_production") ~ "Stable",
      cm_income_sources_reduced_1 %in% c("seasonal", "casual") ~ "Seasonal/Unstable",
      cm_income_sources_reduced_1 %in% c("social_benefits", "rent", "remittances") ~ "Unstable",
      cm_income_sources_reduced_1 %in% c("assistance", "donations_support", "loans") ~ "Emergency",
      TRUE ~ NA
    ), .after = other_cm_income_sources_reduced_1
  )

# -----------------------------------------------------------------------------
# Income source Severity Ranking

hh_data <- hh_data %>%
  mutate(
    income_stability_1 = case_when(
      cm_income_sources_reduced_1 %in% c(
        "regular", "own_business", "own_production"
      ) ~ "stable",
      
      cm_income_sources_reduced_1 %in% c(
        "seasonal", "casual"
      ) ~ "seasonal_unstable",
      
      cm_income_sources_reduced_1 %in% c(
        "social_benefits", "rent", "remittances"
      ) ~ "unstable",
      
      cm_income_sources_reduced_1 %in% c(
        "assistance", "donations_support", "loans"
      ) ~ "emergency",
      
      cm_income_sources_reduced_1 %in% c(
        "other", "no_further_income_sources"
      ) ~ NA,
      
      is.na(cm_income_sources_reduced_1) ~ NA,
      
      TRUE ~ NA
    ),
    
    income_stability_2 = case_when(
      cm_income_sources_reduced_2 %in% c(
        "regular", "own_business", "own_production"
      ) ~ "stable",
      
      cm_income_sources_reduced_2 %in% c(
        "seasonal", "casual"
      ) ~ "seasonal_unstable",
      
      cm_income_sources_reduced_2 %in% c(
        "social_benefits", "rent", "remittances"
      ) ~ "unstable",
      
      cm_income_sources_reduced_2 %in% c(
        "assistance", "donations_support", "loans"
      ) ~ "emergency",
      
      cm_income_sources_reduced_2 %in% c(
        "other", "no_further_income_sources"
      ) ~ NA,
      
      is.na(cm_income_sources_reduced_2) ~ NA,
      
      TRUE ~ NA
    ), .after = other_cm_income_sources_reduced_2
  ) %>%
  mutate(
    n_su = (income_stability_1 %in% c("seasonal_unstable", "unstable")) +
      (income_stability_2 %in% c("seasonal_unstable", "unstable")),
    income_severity = case_when(
      #' is.na(cm_income_total) ~ NA_character_,    *double check this---------------------------------------------------------------------*   
      # cm_income_total <= 0 ~ "Severity 4 - No income or only emergency income sources",
      income_stability_1 %in% "stable" | income_stability_2 %in% "stable" ~
        "Severity 1 - One or more stable income sources",
      n_su >= 2 ~ "Severity 2 - Multiple seasonal or unstable income sources, no stable source reported",
      n_su == 1 ~ "Severity 3 - Only one seasonal or unstable income source, no stable source reported",
      income_stability_1 %in% "emergency" | income_stability_2 %in% "emergency" ~
        "Severity 4 - No income or only emergency income sources",
      TRUE ~ NA   # only "other" sources
    ), .after = income_stability_2
  ) %>% select(-n_su)
      
#  -----------------------------------------------------------------------------
# Travel time  - one hour 

hh_data <- hh_data %>%
  mutate(
    time_healthcare_onehour = case_when(
      is.na(health_facility_time) | health_facility_time == "" ~ NA,
      health_facility_time < 60 ~ "Yes",
      TRUE ~ "No"
    ), .after = health_facility_time
  )

#  -----------------------------------------------------------------------------
# Barriers to healthcare
# Create new dichotomous columns 
# "Yes" if one of the column listed is 1
# "No" if all of the column listed is 0
# Leave blank if all are NA/blank/dnk/pnta

hh_data <- hh_data %>%
  mutate(
    healthcare_barrier.physical = case_when( 
      `health_barriers.no_facility_nearby` == 1 | 
        `health_barriers.facility_too_far` == 1 | 
        `health_barriers.no_transport_means` == 1 | 
        `health_barriers.disability_access_barrier` == 1 ~ "Yes",
      `health_barriers.no_facility_nearby` == 0 & 
        `health_barriers.facility_too_far` == 0 & 
        `health_barriers.no_transport_means` == 0 & 
        `health_barriers.disability_access_barrier` == 0 ~ "No",
      TRUE ~ NA 
    ),
    healthcare_barrier.financial = case_when(
      `health_barriers.cant_afford_consult` == 1 | 
        `health_barriers.cant_afford_treatment` == 1 | 
        `health_barriers.cant_afford_transport` == 1 ~ "Yes",
      `health_barriers.cant_afford_consult` == 0 & 
        `health_barriers.cant_afford_treatment` == 0 & 
        `health_barriers.cant_afford_transport` == 0 ~ "No",
      TRUE ~ NA
    ),
    healthcare_barrier.availability = case_when(
      `health_barriers.service_unavailable` == 1 | 
        `health_barriers.staff_not_trained` == 1 ~ "Yes",
      `health_barriers.service_unavailable` == 0 & 
        `health_barriers.staff_not_trained` == 0 ~ "No",
      TRUE ~ NA
    ),
    healthcare_barrier.safety = case_when(
      `health_barriers.unsafe_at_facility` == 1 | 
        `health_barriers.unsafe_during_travel` == 1 ~ "Yes",
      `health_barriers.unsafe_at_facility` == 0 & 
        `health_barriers.unsafe_during_travel` == 0 ~ "No",
      TRUE ~ NA
    ),
    healthcare_barrier.quality = case_when(
      `health_barriers.long_wait_time` == 1 | 
        `health_barriers.wrong_medication` == 1 | 
        `health_barriers.staff_shortage` == 1 ~ "Yes",
      `health_barriers.long_wait_time` == 0 & 
        `health_barriers.wrong_medication` == 0 & 
        `health_barriers.staff_shortage` == 0 ~ "No",
      TRUE ~ NA
    ),
    healthcare_barrier.cultural = case_when(
      `health_barriers.wait_and_see` == 1 |
        `health_barriers.fear_distrust_healthcare` == 1 |
        `health_barriers.no_time_work_or_care` == 1 |
        `health_barriers.language_barrier` == 1 |
        `health_barriers.no_female_staff` == 1 |
        `health_barriers.stigmatization` == 1 ~ "Yes",
      `health_barriers.wait_and_see` == 0 & 
        `health_barriers.fear_distrust_healthcare` == 0 & 
        `health_barriers.no_time_work_or_care` == 0 ~ "No",
      TRUE ~ NA
    ), .after = other_health_barriers
  )

#  -----------------------------------------------------------------------------
# Displacement Status BASIC DECISION TREE  

forced_reasons <- c(
  "natural_disaster",
  "manmade_disaster",
  "armed_conflict",
  "intercommunal_violence",
  "persecution",
  "general_insecurity",
  "social_tension",
  "econ_reason_conflict_related",
  "limited_services_conflict"
)

hh_data <- hh_data %>%
  mutate(
    has_forced_reason = str_detect(
      coalesce(dis_reasons, ""),
      paste(forced_reasons, collapse = "|")
    )
  ) %>%
  mutate(
    displacement_status = case_when(
      
      # Never displaced
      dis_forced == "no_never" ~
        "Non-displaced",
      
      # Don't know / prefer not to answer
      dis_forced %in% c("dnk", "pnta") ~
        NA_character_,
    
      # HOUSEHOLDS THAT ARE CURRENTLY STILL DISPLACED
      dis_forced == "yes_still_displaced" &
        dis_origin_location == "in_this_area" &
        has_forced_reason ~
        "Infra-settlement IDP",
      
      dis_forced == "yes_still_displaced" &
        dis_origin_location == "elsewhere_in_country" &
        has_forced_reason ~
        "IDP",
      
      dis_forced == "yes_still_displaced" &
        dis_origin_location == "another_country" &
        has_forced_reason ~
        "Refugee",
      
      # HOUSEHOLDS THAT HAVE RETURNED
      dis_forced == "yes_but_back" &
        dis_origin_location == "in_this_area" &
        has_forced_reason ~
        "Returnee - infra-settlement",
      
      dis_forced == "yes_but_back" &
        dis_origin_location == "elsewhere_in_country" &
        has_forced_reason ~
        "Returnee IDP",
        
      # HOUSEHOLDS THAT SAY THEY WERE DISPLACED BUT DO NOT HAVE A FORCED-DISPLACEMENT REASON
      dis_forced %in% c(
        "yes_still_displaced",
        "yes_but_back"
      ) &
        !has_forced_reason ~
        "Non-displaced (economic/voluntary mover)",
      
      # The Displacement guidance defines returnees as "a refugee or internally displaced person who has returned" and lists "Returnees (IDP or Refugees)"
      dis_forced == "yes_but_back" & dis_origin_location == "another_country" & 
        has_forced_reason ~ "Returnee - Refugee",
      
      TRUE ~ NA_character_
    ), .after = dis_forced
  ) %>%
  select(-has_forced_reason)


#  -----------------------------------------------------------------------------
# WASH indicators
# Mandatory WASH indicators as per the "2026 MSNA Sectoral Guidance - WASH"
# (MSNA_WASH_Mandatory_Indicators_Summary), variable names from the 2026 MSNA Indicator Bank.
# Composites are computed with humind (v2026.4.0).

#  -----------------------------------------------------------------------------
# 1. Drinking water

hh_data <- hh_data %>%
  # % HHs with access to an improved/unimproved/surface water source
  add_drinking_water_source_cat() %>%
  # % HHs by time-range category taken to fetch water (premises / under_30_min / 30min_1hr / more_than_1hr)
  # the Kobo tool skips the time question for ALL piped sources on/near premises, not only
  # piped_dwelling (humind default), so they are classified as "premises"
  add_drinking_water_time_cat(
    skipped_drinking_water_source_premises = c("piped_dwelling", "piped_compound", "piped_neighbour")
  ) %>%
  # % HHs by time threshold category (premises / under_30min / above_30min)
  add_drinking_water_time_threshold_cat() %>%
  # % HHs by drinking water JMP Service Ladder (basic / limited / unimproved / surface_water)
  add_drinking_water_quality_jmp_cat() %>%
  relocate(wash_drinking_water_source_cat, wash_drinking_water_time_cat,
           wash_drinking_water_time_30min_cat, wash_drinking_water_quality_jmp_cat,
           .after = wash_drinking_water_time_sl)

#  -----------------------------------------------------------------------------
# HWISE (Household water insecurity experience) Scale
# never = 0, rarely = 1, sometimes = 2, often = 3, always = 3 (collapsed), dnk/pnta = no score
# Worry + Plans + Hands + Drink = HWISE-4 score (0-12); water insecure if score >= 4

hh_data <- hh_data %>%
  add_hwise() %>%
  mutate(
    hwise4_score = as.numeric(hwise4_score),
    hwise_score = hwise4_score,
    hwise_score_cat = case_when(
      hwise_score >= 4 ~ "water_insecure",
      hwise_score >= 0 ~ "not_water_insecure",
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(hwise_score, hwise_score_cat, .after = hwise4_score)

#  -----------------------------------------------------------------------------
# 2. Sanitation facility

hh_data <- hh_data %>%
  # % HHs by type of sanitation facility (improved / unimproved / none)
  add_sanitation_facility_cat() %>%
  # % HHs sharing sanitation facility (shared / not_shared / not_applicable)
  add_sharing_sanitation_facility_cat() %>%
  # % HHs by sanitation JMP Service Ladder (basic / limited / unimproved / open_defecation)
  add_sanitation_facility_jmp_cat() %>%
  relocate(wash_sanitation_facility_cat, wash_sharing_sanitation_facility_cat,
           wash_sanitation_facility_jmp_cat,
           .after = wash_sanitation_facility_sharing_n)

#  -----------------------------------------------------------------------------
# 3. Handwashing
# % HHs by handwashing JMP Service Ladder (basic / limited / no_facility), based on the observed
# facility, falling back to the reported version when observation was not possible (no permission/remote)

hh_data <- hh_data %>%
  add_handwashing_facility_cat() %>%
  relocate(wash_handwashing_facility_jmp_cat, .after = wash_soap_reported_type)


##################################################
# Roster Dataset (Individual level) Disaggregation
##################################################
# add disaggregation from hh data to roster

disagg_cols <- c("MSNA-wide", "State", "LGA",
                 "Gender of HoH", "State - Gender of HoH", "LGA - Gender of HoH",
                 "Age of HoH", "State - Age of HoH","LGA - Age of HoH",
                 "Age & Gender HoH", "State - Age & Gender HoH", "LGA - Age & Gender HoH",
                 "Setting", "State - Setting", "LGA - Setting", "pop_group","State - Population Group",
                 "State - Population Group & Gender of HoH", "State - Population Group & Age of HoH",
                 "State - Population Group & Setting", "State - Population Group & Age & Gender HoH",
                 
                 "Strata", "Strata - Gender of HoH", "Strata - Age of HoH", "Strata - Setting",
                 "Strata - Age & Gender HoH", "displacement_status") 

roster <- roster %>%
  left_join(select(hh_data, uuid, all_of(disagg_cols)), by = "uuid")

roster <- roster %>%
  relocate(all_of(disagg_cols), .after = person_id)


##################################################
# WGQ Dataset (Individual level) Disaggregation
##################################################
disagg_cols <- c("MSNA-wide", "State", "LGA",
                 "Gender of HoH", "State - Gender of HoH", "LGA - Gender of HoH",
                 "Age of HoH", "State - Age of HoH","LGA - Age of HoH",
                 "Age & Gender HoH", "State - Age & Gender HoH", "LGA - Age & Gender HoH",
                 "Setting", "State - Setting", "LGA - Setting", "pop_group","State - Population Group",
                 "State - Population Group & Gender of HoH", "State - Population Group & Age of HoH",
                 "State - Population Group & Setting", "State - Population Group & Age & Gender HoH",
                 
                 "Strata", "Strata - Gender of HoH", "Strata - Age of HoH", "Strata - Setting",
                 "Strata - Age & Gender HoH", "displacement_status") 


wgq <- wgq %>%
  left_join(select(hh_data, uuid, all_of(disagg_cols)), by = "uuid")


disagg_cols_roster <- c("ind_gender","ind_age")

wgq <- wgq %>%
  left_join(select(roster, person_id, all_of(disagg_cols_roster)), by = "person_id") %>%
  relocate(all_of(disagg_cols_roster), .after = uuid)

# Proportion of females with a disability (L3)
# Proportion of males with a disability (L3)
wgq <- wgq %>%
  mutate(`Female member with disability - Level 3` = 
           ifelse(ind_gender == "female" & 
                    `Member with disability - Level 3` == "Disability reported", "Female - Disability reported",
                  ifelse(ind_gender == "female" & 
                           `Member with disability - Level 3` == "No disability reported", "Female - No disability reported", NA)),
         .after = `Member with disability - Level 3`)

wgq <- wgq %>%
  mutate(`Male member with disability - Level 3` = 
           ifelse(ind_gender == "male" & 
                    `Member with disability - Level 3` == "Disability reported", "Male - Disability reported",
                  ifelse(ind_gender == "male" & 
                           `Member with disability - Level 3` == "No disability reported", "Male - No disability reported", NA)),
         .after = `Female member with disability - Level 3`) 

#####################################################
# Education Dataset (Individual level) Disaggregation
#####################################################

# for education loop, filter to only keep records for ind schooling age between 5 and 18. 

# edu <- edu %>%
#   filter(as.numeric(edu_ind_age) >= 5 & as.numeric(edu_ind_age) <= 18)

#  -----------------------------------------------------------------------------
# add disaggregation from hh data to education 
edu <- edu %>%
  left_join(select(hh_data, uuid, all_of(disagg_cols)), by = "uuid")

edu <- edu %>%
  relocate(all_of(disagg_cols), .after = edu_person_id)

#  -----------------------------------------------------------------------------
#' add the disaggregation level: by age of the child  The age group assume s that children aged @3-5 are in pre-primary, @6-11 in primary, and @12-18 in secondary school.

edu <- edu %>%
  mutate(`Age of Child` = case_when(
    as.numeric(edu_ind_age) >=3 &  as.numeric(edu_ind_age) <= 5 ~ "3-5 years", # pre-primary
    as.numeric(edu_ind_age) >=6 &  as.numeric(edu_ind_age) <= 11 ~ "6-11 years", # primary
    as.numeric(edu_ind_age) >=12 &  as.numeric(edu_ind_age) <= 18 ~ "12-18 years", # secondary
    TRUE ~ NA 
  ), .after = edu_ind_age) %>%
  mutate(`State - Age of Child` = paste0(State, " - ", `Age of Child`),
         `LGA - Age of Child` = paste0(LGA, " - ", `Age of Child`),, .after = `Age of Child`)

#  -----------------------------------------------------------------------------
# add the disaggregation level: by gender of the child
edu <- edu %>%
  mutate(`Gender of Child` = ifelse(edu_ind_gender == "female", "Female", 
                                    ifelse(edu_ind_gender == "male", "Male", "Prefer not to answer")),
         `State - Gender of Child` = paste0(State, " - ", `Gender of Child`),
         `LGA - Gender of Child` = paste0(LGA, " - ", `Gender of Child`), .after = edu_ind_gender,
         `Strata - Age of Child` = paste0(Strata, " - ", `Age of Child`),
         `Strata - Gender Child` = paste0(Strata, " - ", `Gender of Child`)
         )
#  -----------------------------------------------------------------------------
# add the disaggregation level: by gender and age of the child

edu <- edu %>%
  mutate(`Age & Gender Child` = paste0(`Gender of Child`, " - ", `Age of Child`),
         `State - Age & Gender Child` = paste0(State, " - ", `Age & Gender Child`),
         `LGA - Age & Gender Child` = paste0(LGA, " - ", `Age & Gender Child`),, .after = `LGA - Gender of Child`)

#  -----------------------------------------------------------------------------
#' add children aged @6-18 not attending school  *Discussed with the team and the resource found backs 6 years is the age to start grade 1*

# edu <- edu %>%
#   mutate(`children aged 6-18 not attending school` = case_when(
#     as.numeric(edu_ind_age) >= 6 & as.numeric(edu_ind_age) <= 18 & edu_access == "no"  ~ "Yes - all children 6-18 not attending formal school",
#     as.numeric(edu_ind_age) >= 6 & as.numeric(edu_ind_age) <= 18 & edu_access == "yes"  ~ "No - all children 6-18 attending formal school",
#     TRUE ~ NA
#   ), .after = edu_access)

#####################################################
# Nutrition Dataset (Individual level) Disaggregation
#####################################################

#  -----------------------------------------------------------------------------
# add disaggregation from hh data to nutrition  

cols <- c("MSNA-wide", "State", "LGA", "displacement_status", "Strata", "pop_group", "State - Population Group")

nut <- nut %>%
  left_join(select(hh_data, uuid, all_of(cols)), by = "uuid") %>%
  relocate(all_of(cols), .after = nut_person_id)

#  -----------------------------------------------------------------------------
#' IYCF : Exclusive Breastfeeding *EBF belongs to the optional full IYCF module (Module C), because it needs questions on water, other liquids and foods, which this MSNS NGA 2026 tool doesn't ask.*

nut <- nut %>%
  mutate(
    nut_ind_under5_age_months = as.numeric(nut_ind_under5_age_months),
    
    # eligibility flag: 0-5 months (and IYCF module answered)
    ebf_eligible = !is.na(nut_ind_under5_age_months) &
      nut_ind_under5_age_months >= 0 &
      nut_ind_under5_age_months < 6 &
      nut_iycf_proxy_available == "yes",
    
    formula_given = !is.na(nut_iycf_proxy_formula) &
      !str_detect(nut_iycf_proxy_formula, "no_formula") &
      !nut_iycf_proxy_formula %in% c("dnk", "pnta"),
    
    bottle_feeding = case_when(
      nut_iycf_proxy_bottle == "yes" ~ TRUE,
      nut_iycf_proxy_bottle == "no"  ~ FALSE,
      TRUE ~ NA
    ),
   
    
    exclusive_breastfeeding = case_when(
      !ebf_eligible ~ NA_real_,
      nut_iycf_proxy_breastfed_ever == "no" ~ 0,
      nut_iycf_proxy_breastfed_yesterday == "no" ~ 0,
      !nut_iycf_proxy_breastfed_yesterday %in% "yes" ~ NA_real_,   # dnk/pnta/missing
      formula_given | bottle_feeding %in% TRUE ~ 0,
      nut_ind_under5_age_months < 5 & is.na(bottle_feeding) ~ NA_real_,  # asked but dnk/pnta
      TRUE ~ 1   # 5-month-olds: bottle question not asked by Kobo design
    ),
    under6_month_age = case_when(
      is.na(exclusive_breastfeeding) ~ NA_real_,
      nut_ind_under5_age_months < 6 ~ 1,
      nut_ind_under5_age_months >= 6 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-formula_given, -bottle_feeding, -ebf_eligible) %>%
  relocate(exclusive_breastfeeding, .after = nut_ind_under5_deworming) %>%
  relocate(under6_month_age, .after = exclusive_breastfeeding)


#  -----------------------------------------------------------------------------
#' IYCF : Minimum dietary diversity (MDD) Score for children aged @6-23 months

# helper: does the foods string contain a given food group code?
has_food <- function(x, code) {
  str_detect(coalesce(x, ""), paste0("(^|\\s)", code, "(\\s|$)"))
}

nut <- nut %>%
  mutate(
    nut_ind_under5_age_months = as.numeric(nut_ind_under5_age_months),
    
    mdd_eligible = !is.na(nut_ind_under5_age_months) &
      nut_ind_under5_age_months >= 6 &
      nut_ind_under5_age_months < 24 &
      nut_iycf_proxy_available == "yes",
    
    # food groups: computed only for eligible children, NA for everyone else
    mdd_breast_milk  = if_else(mdd_eligible, has_food(nut_iycf_proxy_foods, "breast_milk"), NA),
    mdd_grains_roots = if_else(mdd_eligible, has_food(nut_iycf_proxy_foods, "grains_roots_tubers_plantains"), NA),
    mdd_pulses_nuts  = if_else(mdd_eligible, has_food(nut_iycf_proxy_foods, "pulses_nuts_seeds"), NA),
    mdd_dairy        = if_else(mdd_eligible, has_food(nut_iycf_proxy_foods, "dairy_products"), NA),
    mdd_flesh_foods  = if_else(mdd_eligible, has_food(nut_iycf_proxy_foods, "flesh_foods"), NA),
    mdd_eggs         = if_else(mdd_eligible, has_food(nut_iycf_proxy_foods, "eggs"), NA),
    mdd_vitamin_a    = if_else(mdd_eligible, has_food(nut_iycf_proxy_foods, "vitamin_a_fruits_vegetables"), NA),
    mdd_other_fv     = if_else(mdd_eligible, has_food(nut_iycf_proxy_foods, "other_fruits_vegetables"), NA)
  ) %>%
  mutate(
    # score: NA for non-eligible children (rowSums would otherwise give 0)
    mdd_score = if_else(
      mdd_eligible,
      rowSums(
        across(c(mdd_breast_milk, mdd_grains_roots, mdd_pulses_nuts, mdd_dairy,
                 mdd_flesh_foods, mdd_eggs, mdd_vitamin_a, mdd_other_fv)),
        na.rm = TRUE
      ),
      NA_real_
    ),
    
    mdd = case_when(
      mdd_score >= 5 ~ 1,
      mdd_score < 5  ~ 0,
      TRUE ~ NA_real_
    )
  )%>%
  select(-mdd_eligible) %>%
  relocate(starts_with("mdd_"), mdd, .after = exclusive_breastfeeding)

###################################################
# Health Dataset (Individual level) Disaggregation
###################################################

# add disaggregation from hh data to health  
health <- health %>%
  left_join(select(hh_data, uuid, all_of(disagg_cols)), by = "uuid")

health <- health %>%
  relocate(all_of(disagg_cols), .after = health_person_id)

#-------------------------------------------------------------------------------
# add the disaggregation level: by disability status of the individual

# join data from roster to health 
health <- health %>%
  left_join(select(wgq, health_person_id = person_id, `Member with disability - Level 3`), by = "health_person_id") %>%
  relocate(`Member with disability - Level 3`, .after = health_ind_gender) %>%
  mutate(`State - Member with disability - Level 3` = ifelse(!is.na(`Member with disability - Level 3`), paste0(State, " - ", `Member with disability - Level 3`), NA),
         `LGA - Member with disability - Level 3` = ifelse(!is.na(`Member with disability - Level 3`), paste0(LGA, " - ", `Member with disability - Level 3`), NA), .after = `Member with disability - Level 3`)

#  -----------------------------------------------------------------------------
#' add the disaggregation level: by age of the individual

health <- health %>%
  mutate(`Age of Individual` = case_when(
    as.numeric(health_ind_age) >=0 &  as.numeric(health_ind_age) <= 4 ~ "0-4 years",
    as.numeric(health_ind_age) >=5 &  as.numeric(health_ind_age) <= 17 ~ "5-17 years",
    as.numeric(health_ind_age) >=18 &  as.numeric(health_ind_age) <= 59 ~ "18-59 years",
    as.numeric(health_ind_age) >=60 ~ "60+ years",
    TRUE ~ NA 
  ), .after = health_ind_age) %>%
  mutate(`State - Age of Individual` = paste0(State, " - ", `Age of Individual`),
         `LGA - Age of Individual` = paste0(LGA, " - ", `Age of Individual`), .after = `Age of Individual`)

#  -----------------------------------------------------------------------------
# add the disaggregation level: by gender of the individual
health <- health %>%
  mutate(`Gender of Individual` = ifelse(health_ind_gender == "female", "Female", 
                                         ifelse(health_ind_gender == "male", "Male", "Prefer not to answer")),
         `State - Gender of Individual` = paste0(State, " - ", `Gender of Individual`),
         `LGA - Gender of Individual` = paste0(LGA, " - ", `Gender of Individual`),
         `Strata - Age of Individual` = paste0(Strata, " - ", `Age of Individual`),
         `Strata - Gender of Individual` = paste0(Strata, " - ", `Gender of Individual`), .after = health_ind_gender)
#  -----------------------------------------------------------------------------
# add the disaggregation level: by gender and age of the individual
health <- health %>%
  mutate(`Age & Gender Individual` = paste0(`Gender of Individual`, " - ", `Age of Individual`),
         `State - Age & Gender Individual` = paste0(State, " - ", `Age & Gender Individual`),
         `LGA - Age & Gender Individual` = paste0(LGA, " - ", `Age & Gender Individual`), .after = `LGA - Gender of Individual`)

#  -----------------------------------------------------------------------------
# add Forgone care

# Step 1: Create forgone_care variable
health <- health %>%
  mutate(
    forgone_care = case_when(
      health_ind_healthcare_received == "no" ~ "Yes",
      health_ind_healthcare_received == "yes" | 
        health_ind_healthcare_received == "pnta" | 
        health_ind_healthcare_received == "dnk" | 
        is.na(health_ind_healthcare_received) ~ "No",
      TRUE ~ NA
    ), .after = health_ind_healthcare_received
  )

# Step 2: Create met_health_need variable
health <- health %>%
  mutate(
    met_health_need = case_when(
      health_ind_healthcare_received == "yes" ~ "Yes",
      health_ind_healthcare_received == "no" | 
        health_ind_healthcare_received == "pnta" | 
        health_ind_healthcare_received == "dnk" | 
        is.na(health_ind_healthcare_received) ~ "No",
      TRUE ~ NA
    ), .after = forgone_care
  )


###################################################
# Protection Dataset (5-17 aged children) Disaggregation
###################################################

prot <- prot %>%
  left_join(select(hh_data, uuid, all_of(disagg_cols)), by = "uuid")%>%
  relocate(all_of(disagg_cols), .after = prot_person_id)

###################################################
# Final adjustments to the datasets before saving
#################################################

hh_data <- hh_data %>% rename(strata = Strata) %>% relocate(strata, .after = last_col())
roster  <- roster  %>% rename(strata = Strata) %>% relocate(strata, .after = last_col())
wgq     <- wgq     %>% rename(strata = Strata) %>% relocate(strata, .after = last_col())
edu     <- edu     %>% rename(strata = Strata) %>% relocate(strata, .after = last_col())
nut     <- nut     %>% rename(strata = Strata) %>% relocate(strata, .after = last_col())
health  <- health  %>% rename(strata = Strata) %>% relocate(strata, .after = last_col())
prot    <- prot    %>% rename(strata = Strata) %>% relocate(strata, .after = last_col())


#  -----------------------------------------------------------------------------
# Add weight to the datasets
pop_data_strata <- read_xlsx(cluster_table_file_path, sheet = "population_v2")

# Add stratum population to each household record
hh_dataa <- hh_data %>%
  left_join(
    pop_data_strata
    ,
    by = "strata"
  )

# separate 2-stage cluster sampling dataset
hh_cluster <- hh_dataa %>% filter(!is.na(population)) #'*<<<<<*@here_quota_and_unweighted_statically_insignificant_strata/data_will_be_removed_from_analysis


# weight based on accessible population
hh_cluster <- hh_cluster %>%
  analysistools::add_weights(sample_data = pop_data_strata,
                             strata_column_dataset = "strata",
                             strata_column_sample = "strata",
                             population_column = "population_accessible",
                             weight_column = "weights"
  )


# ################################################################################
# # SAVE COMPUTED DATASETS For Analysis
# ################################################################################

#  -----------------------------------------------------------------------------
#' SAVE *Two Stage Cluster Sampling Dataset*
output_dir <- "./outputs/datasets"

# filter the other datasets based on the uuids in the hh_cluster dataset
uuids_cluster <- hh_cluster$uuid

roster_cluster <- roster %>% filter(uuid %in% uuids_cluster) %>%  
  left_join(select(hh_cluster, uuid, population, population_accessible, weights), by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

wgq_cluster <- wgq %>% filter(uuid %in% uuids_cluster) %>%
  left_join(select(hh_cluster, uuid, population, population_accessible, weights), by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

health_cluster <- health %>% filter(uuid %in% uuids_cluster) %>%
  left_join(select(hh_cluster, uuid, population, population_accessible, weights), by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

nut_cluster <- nut %>% filter(uuid %in% uuids_cluster) %>%
  left_join(select(hh_cluster, uuid, population, population_accessible, weights), by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

edu_cluster <- edu %>% filter(uuid %in% uuids_cluster) %>%
  left_join(select(hh_cluster, uuid, population, population_accessible, weights), by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

prot_cluster <- prot %>% filter(uuid %in% uuids_cluster) %>%
  left_join(select(hh_cluster, uuid, population, population_accessible, weights), by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)
# 
write.xlsx(list("hh data"        = hh_cluster,
                "roster data"    = roster_cluster,
                "wgq"        = wgq_cluster,
                "educ data" = edu_cluster,
                "nut data" = nut_cluster,
                "health data"    = health_cluster,
                "prot data"= prot_cluster),
           file = paste0(output_dir,"/", Sys.Date(), "_pre_analysis_dataset_2-STAGE_CLUSTER_SAMPLING.xlsx"))

#  -----------------------------------------------------------------------------
#' SAVE *Quota SAMPLED (REFUGEE) DATASET*
quota_cols_to_remove <- c(
  "strata",
  "population_accessible",
  "population",
  "weight",
  "weight_note",
  "State - Population Group",
  "State - Population Group & Gender of HoH",
  "State - Population Group & Age of HoH",
  "State - Population Group & Setting",
  "State - Population Group & Household with disability - Level 3",
  "State - Population Group & Age & Gender HoH",
  "Strata - Gender of HoH",
  "Strata - Age of HoH",
  "Strata - Setting",
  "Strata - Household with disability - Level 3",
  "Strata - Age & Gender HoH",
  "Strata - Age of Child",
  "Strata - Gender Child",
  "Strata - Age & Gender Child",
  "Strata - Age of Individual",
  "Strata - Gender of Individual",
  "Strata - Age & Gender Individual"
)

hh_quota <- hh_data %>% filter(pop_group=="refugee" | admin2 %in% c("NG008026", "NG008010", "NG008001")
                               ) %>%  select(-any_of(quota_cols_to_remove))
uuids_quota <- hh_quota$uuid

roster_quota    <- roster    %>% filter(uuid %in% uuids_quota) %>%  select(-any_of(quota_cols_to_remove))
wgq_quota       <- wgq       %>% filter(uuid %in% uuids_quota) %>%  select(-any_of(quota_cols_to_remove))
edu_quota       <- edu       %>% filter(uuid %in% uuids_quota) %>%  select(-any_of(quota_cols_to_remove))
nut_quota       <- nut       %>% filter(uuid %in% uuids_quota) %>%  select(-any_of(quota_cols_to_remove))
health_quota    <- health    %>% filter(uuid %in% uuids_quota) %>%  select(-any_of(quota_cols_to_remove))
prot_quota      <- prot %>% filter(uuid %in% uuids_quota) %>%  select(-any_of(quota_cols_to_remove))

#  -----------------------------------------------------------------------------
# Save quota dataset
write.xlsx(list("hh data"        = hh_quota,
                "roster data"    = roster_quota,
                "wgq data"            = wgq_quota,
                "health data"    = health_quota,
                "edu data" = edu_quota,
                "nut data" = nut_quota,
                "prot data"= prot_quota),
           file = paste0(output_dir,"/", Sys.Date(), "_pre_analysis_dataset_REFUGEE_QUOTA_SAMPLING.xlsx"))


# ################################################################################
# # SAVE COMPUTED DATASETS For Analysis
# ################################################################################


weight_lookup <- hh_cluster %>%
  select(
    uuid,
    population,
    population_accessible,
    weights
  ) %>%
  distinct(uuid, .keep_all = TRUE)

#  -----------------------------------------------------------------------------
# Replace the original datasets with the computed datasets for analysis

hh_clean_data <- hh_data %>% left_join(weight_lookup, by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

roster_clean_data <- roster %>% left_join(weight_lookup, by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

wgss_clean_data <- wgq %>% left_join(weight_lookup, by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

health_clean_data <- health %>% left_join(weight_lookup, by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

nut_clean_data <- nut %>% left_join(weight_lookup, by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

edu_clean_data <- edu %>% left_join(weight_lookup, by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

prot_clean_data <- prot %>% left_join(weight_lookup, by = "uuid") %>%
  relocate(population, population_accessible, weights, .after = strata)

#  -----------------------------------------------------------------------------
# openxlsx::write.xlsx(
#   x = list(
#     "hh data"     = hh_clean_data,
#     "roster data" = roster_clean_data,
#     "wgq data"    = wgss_clean_data,
#     "health data" = health_clean_data,
#     "edu data"    = edu_clean_data,
#     "nut data"    = nut_clean_data,
#     "prot data"   = prot_clean_data
#   ),
#   file = file.path(
#     output_dir,
#     paste0(
#       Sys.Date(),
#       "_IMPACT_NGA_Dataset_MSNA-2026-Round1_October-2026.xlsx"
#     )
#   ),
#   overwrite = TRUE
# )
# ################################################################################
# # CHECK THE SAVED DATASET AGAINST THE DATASET THAT WAS READ
# ################################################################################
# Same number of rows in every sheet, and every column read still present.
# Writes <date>_dataset_check_report.xlsx and stops if a check fails.
read_structure <- list(hh = hh_a, roster = roster_a, wgq = wgq_a, health = health_a,
                       nut = nut_a, edu = edu_a, prot = prot_a)

data_final <- list(hh = hh_clean_data, roster = roster_clean_data, wgq = wgss_clean_data, health = health_clean_data,
                   nut = nut_clean_data, edu = edu_clean_data, prot = prot_clean_data)

check_saved_dataset(
  snapshot   = read_structure,
  data_final = data_final,
  report_dir = if (exists("output_dir")) output_dir else "./outputs/datasets"
)

# Print dropped columns in green to the console
cat(red(nrow(hh_a) - (nrow(hh_cluster)+nrow(hh_quota))), "statistically insignificant Strata data dropped from hh dataset\n")

##################################################################################
#' Note that *94* columns are dropped from the quota dataset, as they are not relevant for any statistical analysis
##################################################################################