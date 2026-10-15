#-------------------------------------------------------------------------------
# Generic functions 
#-------------------------------------------------------------------------------


#' Read every sheet of a multi-sheet dataset and convert the column types
#'
#' Same output as the previous version (one data frame per sheet, all columns
#' read as text, Excel date-times converted, `_id`/`_uuid` renamed, Kobo
#' integer/decimal columns made numeric, select-multiple names standardised).
#' It is faster because:
#'   * date-times are converted once per DISTINCT value (household timestamps
#'     repeat on every roster / health / nut ... row) instead of once per row;
#'   * optional parallel reading (`workers`; off by default, see below);
#'   * the processed sheets are cached locally (Parquet, in your AppData
#'     folder, never in SharePoint). Re-reading the SAME file (same size, same
#'     modification time, same numeric-column list) takes seconds. Any change
#'     to the file is detected and the file is read again.
#'
#' @param filename     Path of the Excel file.
#' @param sheets       Sheets to read. Default: all sheets (as before).
#' @param workers      Number of parallel R workers. Default 1 (sequential): on the NGA
#'                     dataset (285 MB) parallel reading was SLOWER (147 s vs 124 s), because
#'                     readxl re-parses the shared strings of the whole workbook per sheet.
#' @param use_cache    Use / write the local cache (default TRUE). Needs arrow;
#'                     without arrow the cache is skipped.
#' @param cols_numeric Kobo integer/decimal column names (default: the global
#'                     `cols_numeric` created by src/init.R, as before).
#' @param cache_dir    Cache folder (local, outside SharePoint).
#' @return Named list of data frames (tibbles), one per sheet.
read_multisheet_raw_data <- function(filename,
                                     sheets = NULL,
                                     workers = 1,
                                     use_cache = TRUE,
                                     cols_numeric = get0("cols_numeric", envir = globalenv(),
                                                         ifnotfound = character(0)),
                                     cache_dir = file.path(tools::R_user_dir("msna_analysis", "cache"),
                                                           "raw_data")) {
  t0 <- proc.time()[["elapsed"]]
  if (!file.exists(filename)) stop("File not found: ", filename)
  all_sheets <- readxl::excel_sheets(filename)
  if (is.null(sheets)) sheets <- all_sheets
  bad <- setdiff(sheets, all_sheets)
  if (length(bad)) stop("Sheets not in the file: ", paste(bad, collapse = ", "))

  # ---- cache -----------------------------------------------------------------
  reader_version <- "fast_reader_v1"
  info <- file.info(filename)
  cache_key <- paste(normalizePath(filename, winslash = "/"), info$size,
                     format(as.numeric(info$mtime), digits = 15),
                     paste(sort(unique(cols_numeric)), collapse = "|"), reader_version, sep = "\n")
  use_cache <- isTRUE(use_cache) && requireNamespace("arrow", quietly = TRUE)
  cache_sub <- file.path(cache_dir, gsub("[^A-Za-z0-9_.-]", "_", basename(filename)))
  sheet_file <- function(s) file.path(cache_sub, paste0(sprintf("%03d", match(s, all_sheets)), ".parquet"))
  key_file <- file.path(cache_sub, "cache_key.txt")

  if (use_cache && file.exists(key_file) &&
      identical(paste(readLines(key_file, warn = FALSE), collapse = "\n"), cache_key) &&
      all(file.exists(vapply(sheets, sheet_file, character(1))))) {
    sheet_data <- lapply(sheets, function(s) {
      df <- arrow::read_parquet(sheet_file(s))
      attr(df, "sm_separator") <- "."
      df
    })
    names(sheet_data) <- sheets
    for (s in sheets) {
      cat(crayon::green(paste0("--> CACHE --> ", basename(filename), " --> Sheet: ", s, " --> ",
                               nrow(sheet_data[[s]]), " entries, ", ncol(sheet_data[[s]]), " columns \n")))
    }
    cat(crayon::green(sprintf("--> Read %d sheets from the local cache in %.1f s (file unchanged since it was cached)\n",
                              length(sheets), proc.time()[["elapsed"]] - t0)))
    return(sheet_data)
  }

  # ---- read + convert one sheet (self-contained: runs in parallel workers) ----
  process_sheet <- function(sheet_name, filename, cols_numeric) {
    df <- readxl::read_excel(filename, sheet = sheet_name, col_types = "text")
    msgs <- character(0)

    submission_col <- intersect(c("_submission_time", "_submission__submission_time"), colnames(df))
    if (length(submission_col) > 0) {
      df[["_submission_time"]] <- df[[submission_col[1]]]
    } else {
      msgs <- c(msgs, "WARNING: No submission time column found!")
    }

    datetime_cols <- intersect(c("start", "end", "today", "_submission_time", "dis_duration", "ind_under5_date",
                                 "ind_under5_event", "dis_arrival", submission_col[1]), colnames(df))
    for (cc in datetime_cols) {
      x <- df[[cc]]
      ok <- !is.na(x)
      if (!any(ok)) {
        df[[cc]] <- rep(NA, length(x))   # as ifelse(): an all-missing column stays logical NA
      } else {
        u <- unique(x[ok])               # convert each distinct value once
        conv <- as.character(suppressWarnings(openxlsx::convertToDateTime(as.numeric(u))))
        out <- rep(NA_character_, length(x))
        out[ok] <- conv[match(x[ok], u)]
        df[[cc]] <- out
      }
    }

    rename_map <- c("_id" = "id", "_uuid" = "uuid")
    hit <- names(df) %in% names(rename_map)
    names(df)[hit] <- rename_map[names(df)[hit]]

    for (cc in intersect(cols_numeric, colnames(df))) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))

    # Standard internal naming: question.choice (as standardise_sm_column_names())
    names(df) <- gsub("/", ".", names(df), fixed = TRUE)
    attr(df, "sm_separator") <- "."
    names(df)[names(df) == "_submission__uuid"] <- "uuid"
    list(df = df, msgs = msgs)
  }

  # ---- read all sheets (in parallel when worthwhile) --------------------------
  if (is.null(workers)) workers <- 1L
  workers <- max(1L, min(as.integer(workers), length(sheets)))
  cat(crayon::green(sprintf("--> Reading %d sheets of %s with %d worker(s) ...\n",
                            length(sheets), basename(filename), workers)))
  if (workers > 1) {
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    res <- parallel::parLapplyLB(cl, sheets, process_sheet, filename = filename, cols_numeric = cols_numeric)
  } else {
    res <- lapply(sheets, process_sheet, filename = filename, cols_numeric = cols_numeric)
  }
  sheet_data <- lapply(res, `[[`, "df")
  names(sheet_data) <- sheets
  for (i in seq_along(sheets)) {
    cat(crayon::green(paste0("--> RAW --> ", filename, " --> Sheet: ", sheets[i], " --> ",
                             nrow(sheet_data[[i]]), " entries, ", ncol(sheet_data[[i]]), " columns \n")))
    for (m in res[[i]]$msgs) cat(crayon::red(paste0("--> ", m, " \n")))
  }

  # ---- write the cache -----------------------------------------------------------
  if (use_cache) {
    ok_cache <- tryCatch({
      dir.create(cache_sub, recursive = TRUE, showWarnings = FALSE)
      unlink(key_file)
      for (s in sheets) arrow::write_parquet(sheet_data[[s]], sheet_file(s))
      writeLines(cache_key, key_file)
      TRUE
    }, error = function(e) {
      cat(crayon::red(paste0("--> Cache not written: ", conditionMessage(e), "\n")))
      FALSE
    })
    if (ok_cache) cat(crayon::green(paste0("--> Cached in ", cache_sub, "\n")))
  }
  cat(crayon::green(sprintf("--> Read %d sheets in %.1f s\n", length(sheets), proc.time()[["elapsed"]] - t0)))
  sheet_data
}

#' Delete the local cache of read_multisheet_raw_data() (all files, or one file).
clear_raw_data_cache <- function(filename = NULL,
                                 cache_dir = file.path(tools::R_user_dir("msna_analysis", "cache"), "raw_data")) {
  target <- if (is.null(filename)) cache_dir else
    file.path(cache_dir, gsub("[^A-Za-z0-9_.-]", "_", basename(filename)))
  unlink(target, recursive = TRUE)
  invisible(target)
}


read_loop_data <- function(filename, sheet, loop_name){
  
  # fix dates
  df <- read_excel(filename, sheet, col_types="text")
  cat(green(paste0("--> RAW LOOP --> ", filename, " --> ", nrow(df), " entries, ", ncol(df), " columns \n")))
  cat(green("--> convert dates \n"))
  df <- df %>%
    mutate(submission_date = `_submission__submission_time`) %>%
    mutate_at(c("submission_date"),
              ~ifelse(is.na(.), NA, as.character(as.Date(convertToDate(as.numeric(.)))))) %>%
    mutate_at(c("_submission__submission_time"),
              ~ifelse(is.na(.), NA, as.character(convertToDateTime(as.numeric(.)))))
  
  #---------------------------------------------------------------------------
  cat(green("--> rename uuid, concate loop count and uuid \n"))
  df <- df %>%
    mutate(uuid = paste0(!!sym(names(df)[startsWith(names(df), "_index")]), "_", `_submission__uuid`)) %>%
    mutate(df_name = loop_name)
  #-----------------------------------------------------------------------------
  # convert numeric columns
  cat(green("--> convert integer columns to numeric \n"))
  df <- df %>% mutate_at(intersect(cols_numeric, colnames(df)), as.numeric)
  # Standard internal naming: question.choice
  df <- standardise_sm_column_names(df)
  
  return(df)
}


# updated this function to handle type conversion, if the collumn is in the expected format it skips and if the column has unexpected type it display col/row where the error happened
convertColumnType <- function(df, col, tool_survey, tool_choices){
  if (col %in% tool_survey$name){
    q <- tool_survey[tool_survey$name %in% col, ]
    cat("\nConverting column:", col)
    
    if (q$type %in% c("integer", "decimal")) {
      # Try conversion
      converted <- suppressWarnings(as.numeric(df[[col]]))
      
      # Find problematic positions
      bad_idx <- which(!is.na(df[[col]]) & is.na(converted))
      if (length(bad_idx) > 0) {
        warning("Column ", col, " has non-numeric values at rows: ",
                paste(bad_idx, collapse = ", "))
        print(data.frame(
          row    = bad_idx,
          column = col,
          value  = df[[col]][bad_idx]
        ))
      }
      
      return(converted)
      
    } else if (q$type %in% c("date", "today")) {
      converted <- suppressWarnings(as.Date(df[[col]], 
                                            tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y")))
      bad_idx <- which(!is.na(df[[col]]) & is.na(converted))
      if (length(bad_idx) > 0) {
        warning("Column ", col, " has invalid dates at rows: ",
                paste(bad_idx, collapse = ", "))
        print(data.frame(
          row    = bad_idx,
          column = col,
          value  = df[[col]][bad_idx]
        ))
      }
      return(as.character(converted))
      
    } else {
      return(df[[col]])
    }
  } else {
    return(df[[col]])
  }
}

#-------------------------------------------------------------------------------
# Data merge functions
#-------------------------------------------------------------------------------

# label and percentage for admin levels
select_all <- function(analysis_results, question_name){
  
  perc <- analysis_results %>%
    select(label_analysis_var_value, stat) %>%
    pivot_wider(names_from = "label_analysis_var_value", values_from = "stat")
  
  n <- analysis_results %>%
    select(label_analysis_var_value, n) %>%
    pivot_wider(names_from = "label_analysis_var_value", values_from = "n")
  
  n_total <- analysis_results %>%
    select(label_analysis_var_value, n_total) %>%
    pivot_wider(names_from = "label_analysis_var_value", values_from = "n_total")
  
  final_df <- rbind(perc, n, n_total)
  
  result <- data.frame()
  combine_result <- data.frame()
  
  columns <- as.character(apply(final_df[1, ], 1, function(x) names(sort(x, decreasing = TRUE))))
  values_stat <- as.character(final_df[1,columns])
  values_n <- as.character(final_df[2,columns])
  values_n_total <- as.character(final_df[3,columns])
  
  for(a in 1:length(columns)){
    result <- result %>%
      mutate(!! paste0(question_name,"_",a) := NA,
             !! paste0(question_name,"_",a,"_stat") := NA,
             !! paste0(question_name,"_",a,"_n") := NA
             #!! paste0(question_name,"_",a,"_n_total") := NA
      )
    
    combine_result <- data.frame(c(combine_result,
                                   columns[a],
                                   values_stat[a],
                                   values_n[a]
                                   # values_n_total[a]
    ))
  }
  
  names(combine_result) <- names(result)
  combine_result <- cbind(combine_result, n_total = values_n_total[1]) %>%
    rename(!!paste0(question_name,"_n_total") := n_total)
  
  return(combine_result)
}

#-------------------------------------------------------------------------------
# Top  label and percentage for both select_one and select_multiple type questions
select_top_n <- function(analysis_results, question_name, top = 3){
  
  perc <- analysis_results %>%
    select(label_analysis_var_value, stat) %>%
    pivot_wider(names_from = "label_analysis_var_value", values_from = "stat")
  
  n <- analysis_results %>%
    select(label_analysis_var_value, n) %>%
    pivot_wider(names_from = "label_analysis_var_value", values_from = "n")
  
  n_total <- analysis_results %>%
    select(label_analysis_var_value, n_total) %>%
    pivot_wider(names_from = "label_analysis_var_value", values_from = "n_total")
  
  final_df <- rbind(perc, n, n_total)
  
  # add to output top n labels and percentages 
  result <- data.frame()
  combine_result <- data.frame()
  
  top_n_columns <- as.character(head(apply(final_df[1, ], 1, function(x) names(sort(x, decreasing = TRUE))), top))
  top_n_stat <- as.character(final_df[1,top_n_columns])
  top_n_n <- as.character(final_df[2,top_n_columns])
  top_n_n_total <- as.character(final_df[3,top_n_columns])
  
  for(a in 1:top){
    result <- result %>%
      mutate(!! paste0(question_name,"_",a) := NA,
             !! paste0(question_name,"_",a,"_stat") := NA,
             !! paste0(question_name,"_",a,"_n") := NA
             #!! paste0(question_name,"_",a,"_n_total") := NA
      )
    
    combine_result <- data.frame(c(combine_result,
                                   top_n_columns[a],
                                   top_n_stat[a],
                                   top_n_n[a]
                                   # top_n_n_total[a]
    ))
  }
  names(combine_result) <- names(result)
  combine_result <- cbind(combine_result, n_total = top_n_n_total[1]) %>%
    rename(!!paste0(question_name,"_n_total") := n_total)
  
  return(combine_result)
}

#-------------------------------------------------------------------------------
select_mean <- function(analysis_results, question_name){
  df <- analysis_results %>%
    filter(analysis_var == question_name)%>%
    select(analysis_var, mean = stat, n_total) %>%
    pivot_wider(values_from = c("mean", "n_total"), names_from = "analysis_var")
}

# ------------------------------------------------------------------------------
# PROJECT-WIDE SELECT-MULTIPLE COLUMN STANDARD
# ------------------------------------------------------------------------------
# Kobo exports expanded select-multiple columns as question/choice.
# All project reader functions standardise these names to question.choice.
# Downstream checking and editing code therefore uses a single separator: ".".
SM_SEPARATOR <- "."

standardise_sm_column_names <- function(df, sm_separator = SM_SEPARATOR) {
  if (!is.data.frame(df)) {
    stop("df must be a data frame.")
  }
  if (!identical(sm_separator, ".")) {
    stop("This project standard requires sm_separator = '.'.")
  }
  names(df) <- gsub("/", sm_separator, names(df), fixed = TRUE)
  attr(df, "sm_separator") <- sm_separator
  df
}
# ------------------------------------------------------------------------------
# CHECK THE SAVED DATASET AGAINST THE DATASET THAT WAS READ
# ------------------------------------------------------------------------------

#' Record the structure (row count + column names) of the sheets that were read
#'
#' Call it right after reading the data, before any sheet is modified. Only row
#' counts and column names are kept (a few KB), not a copy of the data.
#'
#' @param data   Named list of data frames (e.g. from read_multisheet_raw_data()).
#' @param sheets Sheets to record. Default: all data frames in `data`.
#' @return A named list: one list(n_rows, columns) per sheet.
snapshot_dataset_structure <- function(data, sheets = NULL) {
  if (is.null(sheets)) sheets <- names(data)[vapply(data, is.data.frame, logical(1))]
  missing_sheets <- setdiff(sheets, names(data))
  if (length(missing_sheets) > 0) {
    stop("These sheets are not in the data: ", paste(missing_sheets, collapse = ", "))
  }
  setNames(lapply(sheets, function(s) list(n_rows = nrow(data[[s]]), columns = names(data[[s]]))), sheets)
}

#' Check that the dataset to be saved is consistent with the dataset that was read
#'
#' For every sheet of the snapshot:
#'   * the number of rows must be the same (e.g. a left_join must not add rows);
#'   * every column that was read must still exist in the saved sheet.
#' Added columns and rows without a weight are reported for information.
#' The report is written to an Excel file (sheets "summary", "missing_columns",
#' "added_columns") and printed in the console.
#'
#' @param snapshot      The sheets as read: either the output of
#'                      snapshot_dataset_structure(), or a list of the data frames
#'                      themselves (e.g. list(hh = hh_a, roster = roster_a)).
#' @param data_final    List of the data frames that are saved.
#'                      Sheets are matched by NAME when both lists are named,
#'                      otherwise by POSITION (then both lists must have the
#'                      same length and order). Name your lists to get readable
#'                      sheet names in the report.
#' @param report_dir    Folder for the report (created if needed).
#' @param report_prefix File name prefix of the report.
#' @param weight_column Column to count missing weights in (skipped if absent).
#' @param stop_on_fail  If TRUE (default), stop with an error after writing the
#'                      report when a check fails.
#' @return Invisibly, a list with the summary, missing_columns, added_columns
#'         tables and the report file path.
check_saved_dataset <- function(snapshot,
                                data_final,
                                report_dir = "./outputs/datasets",
                                report_prefix = "dataset_check_report",
                                weight_column = "weights",
                                stop_on_fail = TRUE) {
  # --- normalise inputs ------------------------------------------------------
  # a single data frame -> list of one sheet
  if (is.data.frame(snapshot)) snapshot <- list(snapshot)
  if (is.data.frame(data_final)) data_final <- list(data_final)
  if (!is.list(snapshot) || length(snapshot) == 0) stop("`snapshot` is empty: nothing to check.")
  if (!is.list(data_final) || length(data_final) == 0) stop("`data_final` is empty: nothing to check.")

  # data frames given instead of a snapshot -> take their structure
  snapshot <- lapply(snapshot, function(x) {
    if (is.data.frame(x)) list(n_rows = nrow(x), columns = names(x))
    else if (is.list(x) && all(c("n_rows", "columns") %in% names(x))) x
    else stop("Each element of `snapshot` must be a data frame or come from snapshot_dataset_structure().")
  })

  has_names <- function(l) !is.null(names(l)) && all(nzchar(names(l)))
  if (has_names(snapshot) && has_names(data_final)) {
    # match by name
    not_found <- setdiff(names(snapshot), names(data_final))
    if (length(not_found) == length(snapshot)) {
      stop("None of the sheet names match between `snapshot` and `data_final`.\n",
           "  snapshot:   ", paste(names(snapshot), collapse = ", "), "\n",
           "  data_final: ", paste(names(data_final), collapse = ", "))
    }
  } else {
    # match by position
    if (length(snapshot) != length(data_final)) {
      stop("`snapshot` has ", length(snapshot), " sheets and `data_final` has ", length(data_final),
           ". Unnamed lists are matched by position, so they must have the same length and order.")
    }
    nm <- if (has_names(snapshot)) names(snapshot) else if (has_names(data_final)) names(data_final) else
      paste0("sheet_", seq_along(snapshot))
    names(snapshot) <- nm
    names(data_final) <- nm
  }

  summary_rows <- list()
  missing_cols <- list()
  added_cols   <- list()

  for (s in names(snapshot)) {
    read_rows <- snapshot[[s]]$n_rows
    read_cols <- snapshot[[s]]$columns
    in_final  <- s %in% names(data_final) && is.data.frame(data_final[[s]])

    final_rows <- if (in_final) nrow(data_final[[s]]) else NA_integer_
    final_cols <- if (in_final) names(data_final[[s]]) else character(0)
    miss  <- setdiff(read_cols, final_cols)
    added <- setdiff(final_cols, read_cols)
    n_missing_weight <- if (in_final && weight_column %in% final_cols) {
      sum(is.na(data_final[[s]][[weight_column]]))
    } else NA_integer_

    rows_ok <- in_final && identical(as.integer(read_rows), as.integer(final_rows))
    cols_ok <- in_final && length(miss) == 0

    summary_rows[[s]] <- data.frame(
      sheet               = s,
      sheet_saved         = in_final,
      rows_read           = read_rows,
      rows_saved          = final_rows,
      rows_difference     = final_rows - read_rows,
      rows_same           = rows_ok,
      columns_read        = length(read_cols),
      columns_saved       = length(final_cols),
      columns_missing     = length(miss),
      columns_added       = length(added),
      all_columns_kept    = cols_ok,
      rows_missing_weight = n_missing_weight,
      status              = if (rows_ok && cols_ok) "OK" else "FAIL",
      stringsAsFactors    = FALSE
    )
    if (length(miss))  missing_cols[[s]] <- data.frame(sheet = s, column = miss, stringsAsFactors = FALSE)
    if (length(added)) added_cols[[s]]   <- data.frame(sheet = s, column = added, stringsAsFactors = FALSE)
  }

  summary_tab <- do.call(rbind, summary_rows)
  rownames(summary_tab) <- NULL
  missing_tab <- if (length(missing_cols)) do.call(rbind, missing_cols) else
    data.frame(sheet = character(), column = character())
  added_tab <- if (length(added_cols)) do.call(rbind, added_cols) else
    data.frame(sheet = character(), column = character())
  rownames(missing_tab) <- NULL
  rownames(added_tab) <- NULL

  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  report_file <- file.path(report_dir, paste0(Sys.Date(), "_", report_prefix, ".xlsx"))
  openxlsx::write.xlsx(
    list(summary = summary_tab, missing_columns = missing_tab, added_columns = added_tab),
    file = report_file, overwrite = TRUE
  )

  cat("\n- Dataset check (read vs saved):\n")
  print(summary_tab[, c("sheet", "rows_read", "rows_saved", "columns_read", "columns_saved",
                        "columns_missing", "columns_added", "rows_missing_weight", "status")],
        row.names = FALSE)
  cat("  Report written to: ", report_file, "\n", sep = "")

  all_ok <- all(summary_tab$status == "OK")
  if (all_ok) {
    cat(crayon::green("Dataset check passed: same rows and all read columns kept in every sheet.\n"))
  } else {
    msg <- paste0("Dataset check FAILED for: ",
                  paste(summary_tab$sheet[summary_tab$status != "OK"], collapse = ", "),
                  ". See ", report_file)
    if (stop_on_fail) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  invisible(list(summary = summary_tab, missing_columns = missing_tab,
                 added_columns = added_tab, report_file = report_file, passed = all_ok))
}
