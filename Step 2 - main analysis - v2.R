#  -----------------------------------------------------------------------------
#
# Title : Analysis Generator
#    By : IMPACT NGA
#  Date : 09/2026
#
#  -----------------------------------------------------------------------------

rm(list=ls())

# Resources path -------------------------------------------------------------
kobo_tool <- "./resources/kobo_for_analysis.xlsx"
dap <- "resources/data_analysis_plan.xlsx"
samping_type <- "quota"  # "2scs"  "quota"

if (samping_type == "quota") {
  clean_data_path <- "./outputs/datasets/2026-10-09_pre_analysis_dataset_REFUGEE_QUOTA_SAMPLING.xlsx"
} else if (samping_type == "2scs") {
  clean_data_path <- "./outputs/datasets/2026-10-09_pre_analysis_dataset_2-STAGE_CLUSTER_SAMPLING.xlsx"
} else {
  stop("Invalid sampling type. Please choose either 'quota' or '2scs'.")
}

# Load src scripts -------------------------------------------------------------
source("src/init.R")

# fastmsna: fast replacement for analysistools::create_analysis (same results) ---
# https://github.com/Tomas201513/fastmsna  (installed automatically the first time)
if (!requireNamespace("fastmsna", quietly = TRUE)) {
  remotes::install_github("Tomas201513/fastmsna", upgrade = "never")
}
library(fastmsna)

#############################################################################################################
### LOAD DATASET & ANALYSIS PLAN
#############################################################################################################
clean_sheets <- grep("data", excel_sheets(clean_data_path), value = TRUE, ignore.case = TRUE)

label_map <- c(
  "^hh data"     = "hh data",
  "^roster data" = "roster data",
  "^wgq data"    = "wgq data",
  "^edu data"    = "edu data",
  "^health data" = "health data",
  "^nut data"    = "nut data",
  "^prot data"   = "prot data"
)

labels <- vapply(clean_sheets, \(x) {
  hit <- names(label_map)[sapply(names(label_map), grepl, x = x)]
  if (length(hit) == 0) stop("No label match for sheet: ", x)
  label_map[[hit]]
}, character(1))

data <- setNames(
  lapply(clean_sheets, \(x) read_excel(clean_data_path, sheet = x, col_types = "text")),
  labels
)

sheet <- 7  #' [1-6] @<<<<<<---change_it_per_different_sheet_analysis e.g. *"hh data"  "roster data"   "wgq data"   "health data"  "edu data"  "nut data"  "prot data"*
# --- Load DAP ---
my_loa <- read_excel(dap, sheet = "final") %>%
  mutate(level = as.numeric(level))
# %>%
#   filter(analysis_var %in% c("nut_iycf_proxy_available","nut_iycf_proxy_breastfed_ever"))

missing <- find_missing_indicators(kobo_tool, dap, return_details = TRUE)
print(missing)

sheets_to_analyze <- sheets_to_analyze <- c("hh data", "roster data", "wgq data", "health data", "edu data", "nut data", "prot data")
sh <- sheets_to_analyze[sheet]

if (samping_type == "quota") {
disagg <- read_excel(dap, sheet = "disaggregation_quota") %>%
  pull(sheet) %>% na.omit() %>% as.character() 
# %>%
#   .[1]
} else {
disagg <- read_excel(dap, sheet = "disaggregation_2scs") %>%
   pull(sheet) %>% na.omit() %>% as.character() 
# %>%
#   .[1]
}

# --- Load the dataset FIRST: only_nas has to exist before you filter with it ---
data_with_indicators <- data[[sh]]

#' *the nut data sheet has a special age filter for under-5 indicators, so we apply it here so to avoid confusing the analysis with older children*
if(sh == "nut data" | sh == 5) { 
  data_with_indicators <- data_with_indicators %>%
    filter(!is.na(as.numeric(nut_ind_under5_age_months)) & as.numeric(nut_ind_under5_age_months) >= 0 & as.numeric(nut_ind_under5_age_months) <= 59) %>%
    mutate(
      exclusive_breastfeeding = as.numeric(exclusive_breastfeeding),
      under6_month_age        = as.numeric(under6_month_age)
    )
}

#' *very important: the "na" choice in the data caused issues with the analysis, so we recode it to "not_applicable" for the indicator "hesper_clean_women"*
if ("hesper_clean_women" %in% names(data_with_indicators)) {    
  data_with_indicators <- data_with_indicators %>%
    mutate(hesper_clean_women = if_else(hesper_clean_women == "na", "not_applicable", hesper_clean_women))
}

only_nas_lgl <- data_with_indicators %>%
  summarise(across(everything(), ~ sum(is.na(.)) == nrow(data_with_indicators))) %>%
  unlist()

data_main <- data_with_indicators[, !only_nas_lgl]
only_nas  <- names(only_nas_lgl)[only_nas_lgl]


# --- Base LOA for this sheet, NOT expanded across disaggregations.
# run_disaggregated_analysis() does that expansion itself, one disagg at a time.
my_loa_sh <- my_loa %>%
  filter(sheet == sh, !analysis_var %in% only_nas)

# convert mean/median analysis vars to numeric upstream (good call keeping this)
numeric_vars <- my_loa_sh %>%
  filter(analysis_type %in% c("mean", "median")) %>%
  pull(analysis_var)

data_main <- data_main %>%
  mutate(across(all_of(numeric_vars), as.numeric))
  

if (samping_type == "quota") {
  my_design <- srvyr::as_survey_design(data_main)
} else if (samping_type == "2scs") {
  data_main = data_main %>%
    mutate(weights = as.numeric(weights))
  
  my_design <- srvyr::as_survey_design(data_main, weights = "weights", strata = "strata")
} else {
  stop("Invalid sampling type. Please choose either 'quota' or '2scs'.")
}

# fastmsna: prepare the design once (weights, strata, PSU counts) and reuse it
my_fast_design <- as_fast_design(my_design)
print(my_fast_design)

##################################################################################
if (samping_type == "quota"){
  output_dir  <- paste0("./outputs/analysis/quota/results table/", sh, "/")
  } else if (samping_type == "2scs") {
  output_dir  <- paste0("./outputs/analysis/2scs/results table/", sh
, "/")
}  

##################################################################################

cat(green(paste0("Running disaggregated analysis for sheet: ", sh, "\n","\n",
                 "Number of analysis variables: ", nrow(my_loa_sh), "\n","\n",
                 "Disaggregation levels: ", paste(disagg, collapse = ", "), "\n","\n",
                 "Output directory: ", output_dir, "\n","\n",
                 "Missing indicators: ", paste(only_nas, collapse = ", "), "\n"
                 
)))

##################################################################################
#Run the disaggregated analysis (fastmsna: same arguments as run_disaggregated_analysis)
#' Indicators that cannot be computed are skipped and logged in <output_dir>/_logs/
run_disaggregated_analysis_fast(
  sh           = sh,
  loa_sheet    = my_loa_sh,      # <- base loa, one row per analysis_var, group_var = NA
  disagg_vars  = disagg,
  # disagg_index = c(24,1),      # or 1:5, c(3,1), 2, etc. # select which disaggregations to run, in order. Default is all.
  design       = my_fast_design,
  output_dir   = output_dir,   #paste0("./outputs/analysis/results table/", today(), "/", sh, "/"),
  save_format  = "xlsx"
)
##################################################################################
# Ratio analysis for exclusive breastfeeding

# if (sh == "nut data" | sh == 5) {
#   
#   ratio <- map_dfr(
#     disagg,
#     function(g) {
#       create_analysis_ratio(
#         design = my_design,
#         group_var = g,
#         analysis_var_numerator   = "exclusive_breastfeeding",
#         analysis_var_denominator = "under6_month_age",
#         numerator_NA_to_0 = TRUE,
#         filter_denominator_0 = TRUE,
#         level = 0.9
#       )
#     }
#   )
#   
#   out_dir <- "./outputs/analysis/results table/"
#   dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
#   
#   write_xlsx(
#     ratio,
#     file.path(out_dir, paste0(sh, "/nut data_exclusive_breastfeeding_results.xlsx"))
#   )
# }

##################################################################################

# --- Script 2: Column/indicator-specific re-run -------------------------
# Use when ONE indicator had an error and you don't want to rerun the sheet.
# 
# # 1) List the analysis_var(s) you want to fix and rerun
# vars_to_rerun <- c("ind_age")   # <- one or more analysis_var values
# # 2) Subset the LOA to just those vars, same filtering logic as script 1
# my_loa_fix <- my_loa %>%
#   filter(sheet == sh,
#          analysis_var %in% vars_to_rerun,
#          !analysis_var %in% only_nas)
# 
# if (nrow(my_loa_fix) == 0) {
#   stop("No matching analysis_var found in my_loa for sheet '", sh,
#        "'. Check spelling or the sheet filter.")
# }
# 
# # --- Load the dataset FIRST: only_nas has to exist before you filter with it ---
# data_with_indicators <- read_excel(
#   "./outputs/datasets/2026-09-26_pre_analysis_dataset.xlsx",
#   sheet = sh, guess_max = 500000
# )
# 
# only_nas_lgl <- data_with_indicators %>%
#   summarise(across(everything(), ~ sum(is.na(.)) == nrow(data_with_indicators))) %>%
#   unlist()
# 
# data_main <- data_with_indicators[, !only_nas_lgl]
# 
# # 3) Re-apply numeric conversion in case the fixed var is mean/median
# numeric_vars_fix <- my_loa_fix %>%
#   filter(analysis_type %in% c("mean", "median")) %>%
#   pull(analysis_var)
# 
# data_main <- data_main %>%
#   mutate(across(all_of(numeric_vars_fix), as.numeric))
# 
# my_design <- srvyr::as_survey_design(data_main, strata = "strata")
# 
# cat(green(paste0(
#   "Re-running analysis for ", nrow(my_loa_fix), " indicator(s) in sheet: ", sh, "\n",
#   "Indicator(s): ", paste(vars_to_rerun, collapse = ", "), "\n",
#   "Output directory: ", paste0(output_dir, "/"), "\n"
# )))
# 
# run_disaggregated_analysis_fast(
#   sh           = sh,
#   loa_sheet    = my_loa_fix,
#   disagg_vars  = disagg,
#   design       = as_fast_design(my_design),
#   output_dir   = paste0(output_dir,"/","sppecific_rerun/"),
#   save_format  = "xlsx"
# )