#' Find Kobo indicators missing from the data analysis plan
#'
#' An "indicator" in the Kobo tool is any row in the survey sheet where
#' `kobo_indicator_col` is filled in. Its `kobo_name_col` value (the
#' variable/question name) is expected to appear somewhere in the analysis
#' plan, in one of the columns listed in `plan_var_cols` (by default
#' 'analysis_var' and 'group_var' on the 'final' sheet).
#'
#' @param kobo_path Path to the Kobo tool workbook (e.g. kobo_for_analysis.xlsx)
#' @param plan_path Path to the data analysis plan workbook (e.g. data_analysis_plan.xlsx)
#' @param kobo_sheet Sheet name in the Kobo tool holding the questions (default "survey")
#' @param plan_sheet Sheet name in the analysis plan to check against (default "final")
#' @param kobo_name_col Column in the Kobo survey sheet with the variable name (default "name")
#' @param kobo_indicator_col Column flagging a row as an indicator (default "indicator"). Any non-empty value counts.
#' @param plan_var_cols Columns in the plan sheet to search for the variable name (default c("analysis_var", "group_var"))
#' @param return_details If TRUE, return a data frame with name/type/indicator text instead of just names
#'
#' @return A character vector of missing variable names, or a data frame if return_details = TRUE
find_missing_indicators <- function(kobo_path,
                                    plan_path,
                                    kobo_sheet = "survey",
                                    plan_sheet = "final",
                                    kobo_name_col = "name",
                                    kobo_indicator_col = "indicator",
                                    plan_var_cols = c("analysis_var", "group_var"),
                                    return_details = FALSE) {
  
  kobo_df <- read_excel(kobo_path, sheet = kobo_sheet)
  plan_df <- read_excel(plan_path, sheet = plan_sheet)
  
  for (col in c(kobo_name_col, kobo_indicator_col)) {
    if (!(col %in% names(kobo_df))) {
      stop(sprintf("Column '%s' not found in '%s' sheet of %s", col, kobo_sheet, kobo_path))
    }
  }
  
  existing_plan_cols <- plan_var_cols[plan_var_cols %in% names(plan_df)]
  if (length(existing_plan_cols) == 0) {
    stop(sprintf("None of %s found in '%s' sheet of %s",
                 paste(plan_var_cols, collapse = ", "), plan_sheet, plan_path))
  }
  
  # Every variable name referenced anywhere in the plan
  plan_vars <- unique(trimws(unlist(lapply(existing_plan_cols, function(col) {
    vals <- plan_df[[col]]
    as.character(vals[!is.na(vals)])
  }))))
  
  # Rows in the Kobo tool flagged as indicators (indicator column not empty)
  indicator_rows <- kobo_df[!is.na(kobo_df[[kobo_indicator_col]]), ]
  
  var_names <- trimws(as.character(indicator_rows[[kobo_name_col]]))
  is_missing <- nzchar(var_names) & !(var_names %in% plan_vars)
  
  if (return_details) {
    result <- indicator_rows[is_missing, ]
    result <- result %>%
      transmute(
        name = var_names[is_missing],
        type = if ("type" %in% names(indicator_rows)) type[is_missing] else NA,
        indicator = .data[[kobo_indicator_col]][is_missing]
      )
    return(result)
  }
  
  var_names[is_missing]
}


###############################################################################################################################################################################
###############################################################################################################################################################################

#' Run create_analysis() row-by-row, skipping (not stopping on) failures
#'
#' Drop-in replacement for `analysistools::create_analysis()`. It dispatches
#' each LOA row to the same internal functions the original uses
#' (`create_analysis_mean`, `create_analysis_median`,
#' `create_analysis_prop_select_one`, `create_analysis_prop_select_multiple`,
#' `create_analysis_ratio`) but wraps each call in `tryCatch`. A row that
#' errors (typically: not enough observations/PSUs to disaggregate) is
#' skipped instead of killing the whole run, and gets logged -- with the
#' number of complete observations it had -- in `$skipped`.
#'
#' @param design       A `tbl_svy` survey design object (as from
#'                      `srvyr::as_survey_design()`), same as `create_analysis()`.
#' @param loa          List of analysis, same as `create_analysis()`.
#' @param group_var    Same as `create_analysis()`; ignored (with a warning)
#'                      if `loa` is also supplied.
#' @param sm_separator Same as `create_analysis()`. Default ".".
#' @param verbose       If TRUE (default), prints a one-line message for every
#'                      skipped row as it happens.
#'
#' @return A list with:
#'   \item{results_table}{rbind of all rows that succeeded (same shape as
#'     `create_analysis()$results_table`)}
#'   \item{skipped}{data frame: row_index, analysis_type, analysis_var,
#'     group_var, n_obs (complete-case count for the variables involved),
#'     error_message}
#'   \item{dataset}{`design$variables`, same as the original}
#'   \item{loa}{the (checked/expanded) LOA that was run, same as the original}
kobo_or_na <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x)) || (length(x) == 1 && !nzchar(as.character(x)))) {
    "NA"
  } else {
    as.character(x)
  }
}

create_analysis_safe <- function(design, loa = NULL, group_var = NULL,
                                 sm_separator = ".", verbose = TRUE) {
  
  if (!"tbl_svy" %in% attributes(design)$class) {
    stop("It seems object design is not a design, did you use srvyr::as_survey ?")
  }
  
  if (!is.null(loa) & !is.null(group_var)) {
    warning("You have provided a list of analysis and group variable, group variable will be ignored")
  }
  
  if (!is.null(loa)) {
    loa <- analysistools:::check_loa(loa, design)
  }
  if (is.null(loa)) {
    loa <- analysistools:::create_loa(design = design, group_var = group_var,
                                       sm_separator = sm_separator)
  }
  
  loa_rows <- loa %>% split(seq_len(nrow(loa)))
  
  results_list <- vector("list", length(loa_rows))
  skipped <- vector("list", length(loa_rows))
  
  for (i in seq_along(loa_rows)) {
    
    row <- loa_rows[[i]]
    
    result <- tryCatch({
      
      switch(
        row[["analysis_type"]],
        "mean" = analysistools:::create_analysis_mean(
          design, group_var = row[["group_var"]],
          analysis_var = row[["analysis_var"]], level = row[["level"]]
        ),
        "median" = analysistools:::create_analysis_median(
          design, group_var = row[["group_var"]],
          analysis_var = row[["analysis_var"]], level = row[["level"]]
        ),
        "prop_select_one" = analysistools:::create_analysis_prop_select_one(
          design, group_var = row[["group_var"]],
          analysis_var = row[["analysis_var"]], level = row[["level"]]
        ),
        "prop_select_multiple" = analysistools:::create_analysis_prop_select_multiple(
          design, group_var = row[["group_var"]],
          analysis_var = row[["analysis_var"]], level = row[["level"]],
          sm_separator = sm_separator
        ),
        "ratio" = analysistools:::create_analysis_ratio(
          design, group_var = row[["group_var"]],
          analysis_var_numerator = row[["analysis_var_numerator"]],
          analysis_var_denominator = row[["analysis_var_denominator"]],
          numerator_NA_to_0 = row[["numerator_NA_to_0"]],
          filter_denominator_0 = row[["filter_denominator_0"]],
          level = row[["level"]]
        ),
        stop(sprintf("Unknown analysis_type: '%s'", row[["analysis_type"]]))
      )
      
    }, error = function(e) {
      
      # Work out how many complete observations this indicator had,
      # so the log tells you *why* it likely failed (too few obs/PSUs).
      vars_involved <- c(row[["analysis_var"]],
                         row[["analysis_var_numerator"]],
                         row[["analysis_var_denominator"]])
      vars_involved <- unique(vars_involved[!is.na(vars_involved) & nzchar(as.character(vars_involved))])
      vars_present  <- intersect(vars_involved, names(design$variables))
      
      n_obs <- if (length(vars_present) > 0) {
        sum(stats::complete.cases(design$variables[, vars_present, drop = FALSE]))
      } else {
        NA_integer_
      }
      
      skipped[[i]] <<- data.frame(
        row_index     = i,
        analysis_type = kobo_or_na(row[["analysis_type"]]),
        analysis_var  = kobo_or_na(row[["analysis_var"]]),
        group_var     = kobo_or_na(row[["group_var"]]),
        n_obs         = n_obs,
        error_message = conditionMessage(e),
        stringsAsFactors = FALSE
      )
      
      if (verbose) {
        message(sprintf(
          "  ! Skipped row %d (%s / %s, group_var = %s, n = %s): %s",
          i, kobo_or_na(row[["analysis_type"]]), kobo_or_na(row[["analysis_var"]]),
          kobo_or_na(row[["group_var"]]), n_obs, conditionMessage(e)
        ))
      }
      
      NULL
    })
    
    results_list[[i]] <- result
  }
  
  results_list <- Filter(Negate(is.null), results_list)
  results_table <- if (length(results_list) > 0) do.call(rbind, results_list) else NULL
  
  skipped <- Filter(Negate(is.null), skipped)
  skipped_df <- if (length(skipped) > 0) {
    do.call(rbind, skipped)
  } else {
    data.frame(row_index = integer(), analysis_type = character(), analysis_var = character(),
               group_var = character(), n_obs = integer(), error_message = character())
  }
  
  message(sprintf("create_analysis_safe: %d succeeded, %d skipped.",
                  length(results_list), nrow(skipped_df)))
  
  list(
    results_table = results_table,
    skipped       = skipped_df,
    dataset       = design$variables,
    loa           = loa
  )
}


# -----------------------------------------------------------------------
# Example: swap into your existing script
# -----------------------------------------------------------------------
#   my_results <- create_analysis_safe(
#     my_design,
#     loa = my_loa_sh,
#     group_var = NULL,
#     sm_separator = "."
#   )
#
#   my_results_table <- my_results$results_table   # only the successful rows
#   my_results$skipped                              # what got skipped, and why
#
#   # e.g. save the skip log alongside your results:
#   writexl::write_xlsx(my_results$skipped, "./outputs/results/skipped_indicators.xlsx")
###############################################################################################################################################################################


#' Run analysis one disaggregation at a time, saving after each one
#'
#' Runs `create_analysis()` separately for each disaggregation variable in
#' `disagg_vars`, in the exact order given by `disagg_index`, and writes the
#' result to disk immediately after each run -- before moving on to the next
#' disaggregation. This lets you monitor progress (each save/message tells you
#' which disaggregation just finished) and re-run a subset without redoing
#' everything.
#'
#' @param sh             Name of the sheet being analysed (used only for
#'                        messages and output file names).
#' @param loa_sheet       The LOA already filtered to this sheet, BEFORE the
#'                        disaggregation expansion (i.e. `df` in your script:
#'                        `my_loa %>% filter(sheet == sh, !analysis_var %in% only_nas)`).
#' @param disagg_vars     Full ordered character vector of disaggregation
#'                        variables (i.e. `disagg` in your script, from
#'                        `read_excel(dap, sheet = "disaggregation")`).
#' @param disagg_index    Which elements of `disagg_vars` to run, and in what
#'                        order. Examples: `1` -> only the first disagg var;
#'                        `2` -> only the second; `1:5` -> the first five, in
#'                        order; `c(3, 1)` -> third then first. Defaults to
#'                        all of them, in order.
#' @param design          The survey design object for this sheet's dataset
#'                        (i.e. `my_design` in your script).
#' @param sm_separator    Passed straight to `create_analysis()`. Default ".".
#' @param output_dir      Folder to save each disaggregation's results into.
#'                        Created if it doesn't exist.
#' @param file_prefix     Prefix for output file names. Defaults to `sh`.
#' @param save_format     "rds" (default, fast/exact) or "xlsx" (needs the
#'                         writexl package).
#' @param stop_on_error   If FALSE (default), a failed disaggregation is
#'                        logged with a warning and skipped, and the loop
#'                        continues to the next one. If TRUE, it stops the
#'                        whole run.
#'
#' @return Invisibly, a named list of results tables keyed by disaggregation
#'         variable name (only the ones that ran and succeeded).
run_disaggregated_analysis <- function(sh,
                                       loa_sheet,
                                       disagg_vars,
                                       disagg_index = seq_along(disagg_vars),
                                       design,
                                       sm_separator = ".",
                                       output_dir = "./outputs/results",
                                       file_prefix = sh,
                                       save_format = c("rds", "xlsx"),
                                       stop_on_error = FALSE) {
  
  save_format <- match.arg(save_format)
  
  if (any(disagg_index < 1 | disagg_index > length(disagg_vars))) {
    stop(sprintf(
      "disagg_index must contain values between 1 and %d (length of disagg_vars). Got: %s",
      length(disagg_vars), paste(disagg_index, collapse = ", ")
    ))
  }
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  n_total <- length(disagg_index)
  results_list <- list()
  
  for (i in seq_along(disagg_index)) {
    
    idx  <- disagg_index[i]
    gvar <- disagg_vars[idx]
    
    message(sprintf("[%s] (%d/%d) Running disaggregation index %d: '%s'",
                    sh, i, n_total, idx, gvar))
    
    # Same expansion logic as your script, but for one disagg var at a time
    loa_sub <- loa_sheet %>%
      dplyr::mutate(group_var = gvar) %>%
      dplyr::filter(analysis_var != group_var)
    
    run_result <- tryCatch(
      {
        create_analysis(
          design,
          loa = loa_sub,
          group_var = NULL,
          sm_separator = sm_separator
        )
      },
      error = function(e) {
        message(sprintf("  ! Failed on '%s' (index %d): %s", gvar, idx, conditionMessage(e)))
        if (stop_on_error) stop(e)
        NULL
      }
    )
    
    if (is.null(run_result)) next  # skip saving, move to next disaggregation
    
    results_table <- run_result$results_table
    
    # out_file <- file.path(
    #   output_dir,
    #   sprintf("%s_disagg_%02d_%s.%s", file_prefix, idx, gvar, save_format)
    # )
    # Sanitise the disaggregation name for Windows filenames
    # Replace & with "and" and sanitise invalid Windows filename characters
    safe_gvar <- gsub("&", "and", gvar, fixed = TRUE)
    safe_gvar <- gsub('[<>:"/\\\\|?*]', "_", safe_gvar)

    # Windows MAX_PATH is 260 chars (incl. terminator); the project already lives
    # under a long OneDrive path, so truncate the disagg name if the full path is too long
    full_len <- nchar(file.path(
      normalizePath(output_dir, winslash = "/", mustWork = FALSE),
      sprintf("%s_disagg_%02d_%s.%s", file_prefix, idx, safe_gvar, save_format)
    ))
    if (full_len > 259) {
      safe_gvar <- trimws(substr(safe_gvar, 1, nchar(safe_gvar) - (full_len - 259)))
      message(sprintf("  ! Path too long for Windows; filename shortened to '%s'", safe_gvar))
    }

    out_file <- file.path(
      output_dir,
      sprintf("%s_disagg_%02d_%s.%s",
              file_prefix, idx, safe_gvar, save_format)
    )
    
    # Ensure the output directory exists
    dir.create(
      dirname(out_file),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    # if (save_format == "rds") {
    #   saveRDS(results_table, out_file)
    # } else {
    #   writexl::write_xlsx(results_table, out_file)
    # }
    
    save_status <- tryCatch(
      {
        if (save_format == "rds") {
          saveRDS(results_table, out_file)
        } else {
          writexl::write_xlsx(results_table, out_file)
        }
        
        TRUE
      },
      error = function(e) {
        message(sprintf(
          "  ! Failed to save '%s': %s",
          out_file,
          conditionMessage(e)
        ))
        FALSE
      }
    )
    
    if (!save_status) {
      if (stop_on_error) {
        stop("Failed to save output file: ", out_file)
      }
      next
    }
    
    message(sprintf("  -> saved: %s", out_file))
    results_list[[gvar]] <- results_table
    
    
    message(sprintf("  -> saved: %s", out_file))
    
    results_list[[gvar]] <- results_table
  }
  
  invisible(results_list)
}



#########################################################################################
#########################################################################################

# --- Function: format + export one sheet's results ----------------------
format_and_export_results <- function(sh,
                                      my_results_table,
                                      dictionary,
                                      long_dir = "outputs/analysis/long format/",
                                      wide_dir = "outputs/analysis/wide format/") {
  
  dir.create(long_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(wide_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Add labels
  label_results <- add_label_columns_to_results_table(
    results_table = my_results_table,
    dictionary = dictionary
  )
  
  ## ---- long format ----
  label_results_final <- label_results %>%
    create_table_variable_x_group(
      analysis_key = "label_analysis_key",
      value_columns = c("stat", "stat_low", "stat_upp", "n", "n_total", "n_w", "n_w_total")
    )
  
  xml_results_final <- label_results %>%
    create_table_variable_x_group(
      analysis_key = "analysis_key",
      value_columns = c("stat", "stat_low", "stat_upp", "n", "n_total", "n_w", "n_w_total")
    )
  
  create_xlsx_variable_x_group(
    label_results_final,
    value_columns = c("stat", "stat_low", "stat_upp"),
    total_columns = c("n", "n_total", "n_w", "n_w_total"),
    file_path = paste0(long_dir, Sys.Date(), "_MSNA2026_analysis_label_", sh, ".xlsx"),
    overwrite = TRUE
  )
  
  create_xlsx_variable_x_group(
    xml_results_final,
    value_columns = c("stat", "stat_low", "stat_upp"),
    total_columns = c("n", "n_total", "n_w", "n_w_total"),
    file_path = paste0(long_dir, Sys.Date(), "_MSNA2026_analysis_xml_", sh, ".xlsx"),
    overwrite = TRUE
  )
  
  ## ---- wide format ----
  label_results_final <- label_results %>%
    create_table_group_x_variable(
      analysis_key = "label_analysis_key",
      value_columns = c("stat", "stat_low", "stat_upp", "n", "n_total", "n_w", "n_w_total")
    )
  
  xml_results_final <- label_results %>%
    create_table_group_x_variable(
      analysis_key = "analysis_key",
      value_columns = c("stat", "stat_low", "stat_upp", "n", "n_total", "n_w", "n_w_total")
    )
  
  create_xlsx_group_x_variable(
    label_results_final,
    file_path = paste0(wide_dir, Sys.Date(), "_MSNA2026_analysis_label_", sh, ".xlsx"),
    overwrite = TRUE
  )
  
  create_xlsx_group_x_variable(
    xml_results_final,
    file_path = paste0(wide_dir, Sys.Date(), "_MSNA2026_analysis_xml_", sh, ".xlsx"),
    overwrite = TRUE
  )
  
  cat("Done:", sh, "\n")
}

# --- Wrapper: run over one, several, or all sheets -----------------------
run_formatting <- function(results_table,
                           dictionary,
                           sheets = NULL,           # NULL = run all sheets
                           long_dir = "outputs/analysis/long format/",
                           wide_dir = "outputs/analysis/wide format/") {
  
  available_sheets <- names(results_table)
  
  if (is.null(sheets)) {
    sheets_to_run <- available_sheets
  } else {
    missing_sheets <- setdiff(sheets, available_sheets)
    if (length(missing_sheets) > 0) {
      warning("These sheets were not found and will be skipped: ",
              paste(missing_sheets, collapse = ", "))
    }
    sheets_to_run <- intersect(sheets, available_sheets)
  }
  
  if (length(sheets_to_run) == 0) {
    stop("No matching sheets to run. Available sheets: ",
         paste(available_sheets, collapse = ", "))
  }
  
  walk(sheets_to_run, function(sh) {
    format_and_export_results(
      sh = sh,
      my_results_table = results_table[[sh]],
      dictionary = dictionary,
      long_dir = long_dir,
      wide_dir = wide_dir
    )
  })
}
