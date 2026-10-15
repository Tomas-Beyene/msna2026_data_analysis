#  -----------------------------------------------------------------------------
#
# Title : Formatted Analysis v2
#    By : IMPACT NGA
#  Date : 09/2026
# 
#  -----------------------------------------------------------------------------

rm(list=ls())

# install.packages("remotes")
remotes::install_github("Tomas201513/fastpresentresults")

library(fastpresentresults)
parse_analysis_key(presentresults_resultstable)


# Load src scripts -------------------------------------------------------------
source("src/init.R")



data <- read_multisheet_raw_data("./outputs/analysis/quota/results table/IMPACT_NGA_Data_Analysis_MSNA_September-2026-Quota.xlsx",
                                 sheets = "hh_clean_data")[[1]]

my_results <- read_multisheet_raw_data( "./outputs/analysis/quota/results table/merged/2026-10-09_merged_results.xlsx")

my_result <- my_results$`hh data`
View(my_result)


ipc <- fastpresentresults::create_ipc_table(
  results_table = my_result,
  dataset = data,
  # cluster_name = "cluster_id",
  hhs_cat_values = c("None", "No or Little", "Little", "Moderate", "Severe", "Very Severe"),
  
  with_hdds = FALSE,
  # variable names default to the MSNA 2024 'fsl_' template; override as needed
)

create_xlsx_group_x_variable(ipc, table_name = "ipc_table", file_path = "ipc_table.xlsx")
