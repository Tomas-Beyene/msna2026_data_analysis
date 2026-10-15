#  -----------------------------------------------------------------------------
#
# Title : Analysis - Merge Results Table
#    By : IMPACT NGA
#  Date : 09/2026
# 
#  -----------------------------------------------------------------------------

rm(list=ls())

# --- Config -----------------------------------------------------------
kobo_tool <- "./resources/kobo_for_analysis.xlsx"

samping_type <- "quota"  # "2scs"  "quota"


if (samping_type == "quota") {
  results_root <- "./outputs/analysis/quota/results table"
  merged_path  <- paste0(
    "./outputs/analysis/quota/results table/merged/",
    today(),
    "_merged_results.xlsx"
  )
  dir.create(dirname(merged_path),
             recursive = TRUE,
             showWarnings = FALSE)
} else if (samping_type == "2scs") {
  results_root <- "./outputs/analysis/2scs/results table"
  merged_path  <- paste0(
    "./outputs/analysis/2scs/results table/merged/",
    today(),
    "_merged_results.xlsx"
  )
  dir.create(dirname(merged_path),
             recursive = TRUE,
             showWarnings = FALSE)
} else {
  stop("Invalid sampling type. Please choose either 'quota' or '2scs'.")
}

# Load src scripts -------------------------------------------------------------
source("src/init.R")

# Add a column recording which file each row came from? (recommended)
add_source_col <- TRUE

# --- Merge one folder's files vertically into a single data frame ------
merge_folder_vertically <- function(folder_path, add_source_col = TRUE) {
  
  # Read from inside the folder: some full paths exceed Windows' 260-char limit
  # and read_excel() then reports "path does not exist"
  old_wd <- setwd(folder_path)
  on.exit(setwd(old_wd), add = TRUE)

  files <- list.files(pattern = "\\.xlsx$")
  if (length(files) == 0) return(NULL)

  dfs <- map(files, function(f) {
    df <- read_excel(f, guess_max = 500000)
    if (add_source_col) {
      df <- df %>% 
        mutate(source_file = tools::file_path_sans_ext(basename(f)), .before = 1) # Add source file name as first column (no use except for debugging)
    }
    df
  })
  
  # bind_rows aligns by column name and fills missing cols with NA,
  # so this is safe even if a couple of files have slightly different columns
  bind_rows(dfs)
}

# --- Run for every subfolder, write one sheet per folder -----------------
folders <- list.dirs(results_root, recursive = FALSE)
folders <- folders[!grepl("merged$", folders)]

wb <- createWorkbook()

for (folder_path in folders) {
  folder_name <- basename(folder_path)
  cat("Merging folder:", folder_name, "\n")
  
  merged_df <- merge_folder_vertically(folder_path, add_source_col)
  if (is.null(merged_df)) {
    cat("  (skipped, no xlsx files)\n")
    next
  }
  
  sheet_name <- substr(folder_name, 1, 31)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, merged_df)
  setColWidths(wb, sheet_name, cols = 1:ncol(merged_df), widths = "auto")
}

saveWorkbook(wb, merged_path, overwrite = TRUE)
cat("Saved merged workbook to:", merged_path, "\n")
