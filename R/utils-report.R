.affirm_report_gt_stylings <- function(x) {
  x |>
    gt::cols_width("status_color" ~ gt::px(6)) |>
    gt::cols_label(
      status_color = "",
      id = gt::md("**ID**"),
      label = gt::md("**Affirmation**"),
      priority = gt::md("**Priority**"),
      data_frames = gt::md("**Data Frames**"),
      columns = gt::md("**Columns**"),
      error_n = gt::md("**No. Errors**"),
      total_n = gt::md("**Total No. Checks**"),
      error_rate = gt::md("**Error Rate**"),
      csv_download_link = gt::md("**Listing Download**")
    ) |>
    gt::fmt_markdown(columns = "csv_download_link") |>
    gt::cols_align(align = "center") |>
    gt::cols_align(align = "left", columns = any_of(c("id", "label", "priority", "data_frames", "columns"))) |>
    gt::fmt_percent(
      columns = .data$error_rate,
      decimals = 1
    ) |>
    gt::tab_options(table.font.size = 15, data_row.padding = gt::px(1),
                    summary_row.padding = gt::px(1), grand_summary_row.padding = gt::px(1),
                    footnotes.padding = gt::px(1), source_notes.padding = gt::px(1),
                    row_group.padding = gt::px(1)) |>
    gt::sub_missing() |>
    gt::sub_missing(columns = "status_color", missing_text = "") |>
    gt::tab_style(
      style = gt::cell_fill(color = "#D61F1F"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = .data$priority %in% 1L | dplyr::between(.data$error_rate, 0.50, 1)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#FFD301"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = !.data$priority %in% 1L & dplyr::between(.data$error_rate, 0.10, 0.50)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#7BB662", alpha = 0.5),
      locations = gt::cells_body(
        columns = "status_color",
        rows = !.data$priority %in% 1L & !.data$error_rate %in% 0 & dplyr::between(.data$error_rate, 0, 0.10)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#006B3D"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = .data$error_rate %in% 0
      )
    )
  # # this hides optional columns that are all NA
  # # this code does not work in gt 0.9.0, because it errors if no columns are selected.
  # # I submitted a PR to gt to allow for no columns being selected and should be in the next release
  # gt::cols_hide(columns = where(\(x) all(is.na(x))))
}


# converts a data frame to a clickable CSV download link
.as_csv_encoded_html_download_link <- function(data,
                                               output_file_name = "extract.csv") {
  if (is.null(data) || nrow(data) == 0) return("&mdash;")

  temp_file <-
    tempfile(pattern = paste0("csv_file"), fileext = ".csv")

  readr::write_csv(data, file = temp_file)
  on.exit(unlink(temp_file))

  file_encoded <- base64enc::base64encode(temp_file)
  title_text <- "Not sure what title text is for"

  as.character(
    htmltools::a(
      href = paste0("data:text/csv;base64,", file_encoded),
      download = output_file_name,
      htmltools::tags$button(
        `aria-label` = title_text,
        `data-balloon-pos` = "left",
        style = htmltools::css(
          `background-color` = "#02304D",
          color = "#FFFFFF",
          border = "none",
          padding = "5px",
          `font-weight` = "bold",
          cursor = "pointer",
          `border-radius` = "4px",
          `vertical-align` = "middle",
          margin = "0"
        ),
        "CSV"
      )
    )
  )
}

# creates expandable details for gt table with full-width summary and data table
.create_gt_expandable_details_fullwidth <- function(data, id, label, priority, data_frames, columns, error_n, total_n, error_rate, csv_download_link) {
  # Build the summary information
  summary_parts <- c()
  if (!is.na(id) && id != "") {
    summary_parts <- c(summary_parts, paste0("<strong>ID:</strong>&nbsp;", id))
  }
  if (!is.na(label) && label != "") {
    summary_parts <- c(summary_parts, paste0("<strong>Affirmation:</strong>&nbsp;", label))
  }
  if (!is.na(priority) && priority != "") {
    summary_parts <- c(summary_parts, paste0("<strong>Priority:</strong>&nbsp;", priority))
  }
  if (!is.na(data_frames) && data_frames != "") {
    summary_parts <- c(summary_parts, paste0("<strong>Data Frames:</strong>&nbsp;", data_frames))
  }
  if (!is.na(columns) && columns != "") {
    summary_parts <- c(summary_parts, paste0("<strong>Columns:</strong>&nbsp;", columns))
  }
  summary_parts <- c(
    summary_parts,
    paste0("<strong>No. Errors:</strong>&nbsp;", error_n),
    paste0("<strong>Total No. Checks:</strong>&nbsp;", total_n),
    paste0("<strong>Error Rate:</strong>&nbsp;", sprintf("%.1f%%", error_rate * 100)),
    paste0("<strong>Listing Download:</strong>&nbsp;", csv_download_link)
  )
  
  summary_html <- paste0(
    '<div style="display: flex; flex-wrap: wrap; gap: 15px; align-items: center; margin-bottom: 10px; font-size: 13px;">',
    paste(paste0('<span style="display: inline-flex; align-items: center;">', summary_parts, '</span>'), collapse = ' | '),
    '</div>'
  )
  
  # Create the data table HTML if there are errors
  if (is.null(data) || nrow(data) == 0) {
    data_section <- '<div style="color: #666; font-style: italic; font-size: 13px;">No validation failures</div>'
  } else {
    table_html <- data |>
      gt::gt() |>
      gt::tab_options(
        table.font.size = 12,
        data_row.padding = gt::px(2),
        column_labels.font.weight = "bold"
      ) |>
      gt::as_raw_html()
    
    data_section <- as.character(htmltools::tags$details(
      htmltools::tags$summary(
        style = "cursor: pointer; color: #0066cc; font-size: 13px; user-select: none; font-weight: 500;",
        paste0("View ", nrow(data), " validation failure", if(nrow(data) > 1) "s" else "")
      ),
      htmltools::tags$div(
        style = "margin-top: 8px; padding: 8px; background-color: #ffffff; border: 1px solid #dee2e6; border-radius: 4px; overflow-x: auto;",
        htmltools::HTML(table_html)
      )
    ))
  }
  
  # Combine summary and data sections
  as.character(htmltools::div(
    style = "padding: 8px; background-color: #f8f9fa; border-radius: 4px; margin: -1px;",
    htmltools::HTML(summary_html),
    htmltools::HTML(data_section)
  ))
}



#' Utils-report: compute column widths for excel exports
#' @noRd
#' @param data a data frame of individual affirmation results
#' @param min_width minimum column width for excel export
#' @param max_width maximum column width for excel export
#' @param pad the number of characters to pad the width; so if there are two
#' characters you would pad by an additional set amount for some breathing room
#' @return a named numeric vector
#'
.compute_col_width <- function(data, min_width = 8, max_width = 50, pad = 3){
  # create dummy data frame with column names and variable values
  # in order to compute max length for setting column widths
  vec_lengths <- data.frame(t(names(data))) |>
    stats::setNames(names(data)) |>
    # add data as character values
    dplyr::bind_rows(data |> lapply(as.character)) |>
    # find character length of all entries in each column
    lapply(nchar) |>
    # pad the character length for extra space
    lapply(\(x) {x + pad}) |>
    # find max character length with a set minimum value
    lapply(max, min_width, na.rm = TRUE) |>
    # now set maximum width
    lapply(min, max_width, na.rm = TRUE) |>
    unlist()

  # set a width for Comment column
  vec_lengths[["Comment"]] <- 30

  return(vec_lengths)
}

#' Utils-report: extract variable labels from a data frame
#' @noRd
#' @param data a data frame of individual affirmation results
#' @return a data frame with one row of variable labels
#'
.retrieve_labels <- function(data){
  lapply(data, attr, "label") |>
    lapply(\(x) {ifelse(is.null(x), NA_character_, x)}) |>
    data.frame()
}


#' Utils-report: identify non-NA data to output in excel report.
#'
#' If an argument is not supplied to affirm_ # and all values of column are NA,
#' then remove that column from the excel export.
#' @noRd
#' @param df_summary a data frame of the overall affirmation report
#' @return a data frame for the excel export
#'
.identify_keep_data <- function(df_summary){
  # identify which columns have affirmation fields entered
  vec_present <- lapply(df_summary, \(x) {!all(is.na(x))})
  # identify which columns to keep
  vec_keep_cols <- names(unlist(vec_present[unlist(vec_present)]))
  # create a data frame with columns to keep
  df_keep <- df_summary |>
    dplyr::select(dplyr::all_of(c("assigned_to", vec_keep_cols))) |>
    dplyr::select(-"data")

  return(df_keep)
}

#' Utils-report:  add first sheet with summary of affirmations to excel workbook
#' @noRd
#' @param wb a workbook object
#' @param df_export a data frame summarizing the affirmations to export
#' @return a workbook object
#'
.add_summary_sheet <- function(wb, df_export){
  wb |>
    # add front page with summary information ----
  openxlsx2::wb_add_worksheet("Summary") |>
    openxlsx2::wb_add_data_table(
      x = df_export,
      na.strings = "",
      table_style = "TableStyleLight8"
    ) |>
    openxlsx2::wb_set_col_widths(
      cols = 1:ncol(df_export),
      widths = .compute_col_width(df_export)
    )
}

#' Utils-report:  add sheet for an individual affirmation
#' @noRd
#' @param wb a workbook object
#' @param df_summary_row a data frame with a single row from the affirmation
#' summary table
#' @return a workbook object
#'
.add_affirmation_sheet <- function(wb, df_summary_row, prev_exists){


  df_affirmation <-
    df_summary_row[["data"]][[1]]

  empty_df <- nrow(df_affirmation) == 0

  if(empty_df){
    df_affirmation[1,] <- NA
  }

  if(prev_exists){

    # labels of the data frame of single affirmation results
    # data frame of single affirmation results
    vec_widths <- .compute_col_width(df_affirmation)
    df_labels <- .retrieve_labels(df_affirmation)

  } else{
    df_affirmation <-
      df_summary_row[["data"]][[1]] |>
      dplyr::mutate(
        Status = NA,
        Comment = NA
      )

    # data frame of single affirmation results
    vec_widths <- .compute_col_width(df_affirmation)
    df_labels <- .retrieve_labels(df_affirmation)

  }


  wb <-
    suppressWarnings(
      wb |>
        openxlsx2::wb_add_worksheet(df_summary_row[["affirmation_name"]]) |>
        # add affirmation label on first row
        openxlsx2::wb_add_data(
          x = df_summary_row[["label"]][[1]],
          na.strings = "",
          start_row = 1
        ) |>
        # style affirmation label
        openxlsx2::wb_add_font(
          dims = "A1:A1",
          bold = TRUE
        ) |>
        # merge cells on affirmation label
        openxlsx2::wb_merge_cells(
          dims = "A1:P1"
        ) |>
        # wrap text on affirmation label
        openxlsx2::wb_add_cell_style(
          dims = "A1:A1",
          wrap_text = TRUE
        ) |>
        # add variable labels above variable names
        openxlsx2::wb_add_data(
          x = df_labels,
          na.strings = "",
          start_row = 3,
          col_names = FALSE
        ) |>
        # style variable labels
        openxlsx2::wb_add_font(
          dims = openxlsx2::wb_dims(x = df_labels, from_row = 3, col_names = FALSE),
          italic = TRUE,
          bold = TRUE,
          color = openxlsx2::wb_color(hex = "#000000")
        ) |>
        # wrap text on variable labels
        openxlsx2::wb_add_cell_style(
          dims = openxlsx2::wb_dims(x = df_labels, from_row = 3, col_names = FALSE),
          wrap_text = TRUE
        ) |>
        # add data on lower row
        openxlsx2::wb_add_data_table(
          x = df_affirmation,
          na.strings = "",
          table_style = "TableStyleLight8",
          start_row = 4
        ) |>
        openxlsx2::wb_set_col_widths(
          cols = seq_len(ncol(df_affirmation)),
          widths = .compute_col_width(df_affirmation)
        )
    )

  return(wb)
}

#' Utils-report:  update current summary sheet with previous assignment, status, and comment information
#' @noRd
#' @param df_summary_current The summary data frame from the current affirm session
#' @param prev_wb a workbook object. The previous workbook that is being updated
#' @return an updated summary data frame
#'
.update_sheets <- function(df_summary_current, prev_wb){

  # Establish the current affirmations in the environment#
  vec_new_affirmation_names <-
    df_summary_current |>
    dplyr::pull("affirmation_name")

  #============================================================================#
  # Previous Summary Data-------------------------------------------------------
  #============================================================================#

  # Pull previous wb into the environment
  prev_wb <- openxlsx2::wb_load(prev_wb)

  # Find where the "Summary" sheet is
  prev_summary_sheet <- which(prev_wb[["sheet_names"]] == "Summary")

  # Determine if there are any "other" sheets present before the summary sheet
  other_present <- prev_summary_sheet - 1 != 0

  # If "other" sheets were found, throw an error that they need to be removed
  if(other_present){
    c(
      "x" = "Extra sheets were found before the 'Summary' sheet in the previous Excel workbook we attempted to use for updating.",
      "i" = "Please remove any additional sheets and ensure the 'Summary' sheet is the first sheet in the workbook, followed by the affirmation sheets."
    ) |>
      cli::cli_abort(call = sys.call(-1))
  }

  # Pull the previous affirmation names
  vec_prev_affirmation_names <- prev_wb[["sheet_names"]][-prev_summary_sheet]

  # Update the prev affirmations to only include those that match in the new affirmations
  vec_prev_affirmation_names <- vec_prev_affirmation_names[vec_prev_affirmation_names %in% vec_new_affirmation_names]

  # Pull out the old summary's affirmation names, assigned to, status, and comments columns
  # This will be use to update these columns in the current summary sheet
  df_summary_prev <-
    openxlsx2::wb_to_df(
      prev_wb,
      sheet = "Summary",
      skip_empty_cols = TRUE
    ) |>
    dplyr::select(
      "assigned_to",
      "affirmation_name",
      "Status",
      "Comment"
    ) |>
    # Only keep previous affirmations that match the current affirmations
    # This allows the removal of previous affirmations that aren't needed
    dplyr::filter(.data$affirmation_name %in% vec_prev_affirmation_names)

  #============================================================================#
  # Current Summary Data to be updated------------------------------------------
  #============================================================================#
  # Join old info into new summary sheet
  df_summary_updated_init <-
    df_summary_current |>
    dplyr::left_join(
      df_summary_prev,
      by = "affirmation_name"
    ) |>
    dplyr::select(
      c("assigned_to", "affirmation_name", "data_frames", "id", "columns", "error_n",
        "total_n", "error_rate", "label", "Status", "Comment", "data")
    )

  # Pull out initial affirmation dfs to account for correct previous columns
  # This is only used for checks and not joining data
  lst_init_new_affirmation_dfs <- list()

  for (i in seq_len(nrow(df_summary_current))){
    lst_init_new_affirmation_dfs[[i]] <-
      df_summary_current[[i, "data"]][[1]]
  }

  # Pull the names into the list
  names(lst_init_new_affirmation_dfs) <- vec_new_affirmation_names

  # Ensure all affirmation names are in order
  lst_init_new_affirmation_dfs <- lst_init_new_affirmation_dfs[sort(names(lst_init_new_affirmation_dfs))]

  #============================================================================#
  # Previous Affirmation Dataframe Extractions----------------------------------
  #============================================================================#
  # Initialize a list to pull out old affirmation dfs
  lst_prev_affirmation_dfs <- list()

  for (affirmation_name in vec_prev_affirmation_names){
    # Find the sheet index for this affirmation name
    sheet_index <- which(prev_wb[["sheet_names"]] == affirmation_name)

    # Find the corresponding row in df_summary_current for this affirmation
    current_row_index <- which(df_summary_current$affirmation_name == affirmation_name)

    # If a current affirmation is empty (no issues found)
    # Fill it with placeholders so it can be joined into the final workbook anyway
    if(length(current_row_index) > 0 && df_summary_current[[current_row_index, "data"]][[1]] |> nrow() == 0){
      lst_prev_affirmation_dfs[[affirmation_name]] <-
        dplyr::tibble(
          join_key = character(),
          Status = NA,
          Comment = NA
        )

    } else{
      # Otherwise, pull in the previous report#
      lst_prev_affirmation_dfs[[affirmation_name]] <-
        openxlsx2::wb_to_df(
          prev_wb,
          sheet = sheet_index,
          start_row = 4,
          skip_empty_cols = TRUE
        ) |>
        dplyr::mutate(
          # Create a join key that's used to link previous rows to current rows
          # Remove status and comment as we wont use those for joining.
          join_key = do.call(
            paste,
            c(dplyr::select(
              openxlsx2::wb_to_df(
                prev_wb,
                sheet = sheet_index,
                start_row = 4,
                skip_empty_cols = TRUE
              ),dplyr::everything(), -c("Status", "Comment")),
              list(sep = " ")
            )
          )
        )
    }
  }

  # Order the previous affirmations just in case they come in out of order
  lst_prev_affirmation_dfs <- lst_prev_affirmation_dfs[sort(names(lst_prev_affirmation_dfs))]

  #============================================================================#
  # Mismatched Affirmation Work-------------------------------------------------
  #============================================================================#

  # Account for newly added affirmations
  # Assess if any affirmations are missing from the previous affirmation
  # By getting the total affirmations present in new and previous reports
  new_length <- vec_new_affirmation_names |> length()
  prev_length <- vec_prev_affirmation_names |> length()
  any_missing <- abs(new_length - prev_length) > 0

  # If a difference in length is found between the 2 reports...
  if(any_missing){

    # Determine which one is "smaller" (missing dfs)
    smaller_report <- which.min(c("new" = new_length, "prev" = prev_length)) |> names()

    # Grab the names of the missing affirmatons
    missing_dfs <- setdiff(vec_new_affirmation_names, vec_prev_affirmation_names)

    if(smaller_report == "prev"){
      # If the previous report is missing dfs, that means affirmations were newly added
      # This means there's nothing to compare to, so create placeholders for the upcoming joins
      # We don't have to worry about this if previous affirmations were dropped in the new affirmations...
      # ...as dropped affirmations should not appear in the updated report
      for (df_name in missing_dfs){
        lst_prev_affirmation_dfs[[df_name]] <-
          lst_init_new_affirmation_dfs[[df_name]] |>
          mutate(
            join_key = do.call(
              paste,
              c(dplyr::across(dplyr::everything()),
                list(sep = " ")
              )),
            Status = NA,
            Comment = NA
          )
      }
    }
  }

  # Pull out prev affirmation column names for mismatch checking
  lst_prev_affirmation_cols <-
    lapply(
      lst_prev_affirmation_dfs,
      function(x) x |> names() |> dplyr::setdiff(c("Status", "Comment", "join_key"))
    )

  # Order the column names for the check
  lst_prev_affirmation_cols <- lst_prev_affirmation_cols[sort(names(lst_prev_affirmation_cols))]

  # Prev affirmation column amendment before joining...
  # This list will be joined with the "new" affirmation data.
  # We only need the join key, status, and comment fields for this
  for (affirmation_name in names(lst_prev_affirmation_dfs)){
    lst_prev_affirmation_dfs[[affirmation_name]] <-
      # Ensure only applicable columns are carried forward
      lst_prev_affirmation_dfs[[affirmation_name]] |>
      dplyr::select(
        "join_key", "Status", "Comment"
      )
  }

  # Sort the names of the affirmations for consistency
  lst_prev_affirmation_dfs <- lst_prev_affirmation_dfs[sort(names(lst_prev_affirmation_dfs))]

  #============================================================================#
  # Mismatched Affirmation Columns Check----------------------------------------
  #============================================================================#
  # Create an initial list of new affirmation columns#
  lst_new_affirmation_cols <-
    lapply(
      lst_init_new_affirmation_dfs,
      names
    )

  # Sort the names of the affirmations for consistency
  lst_new_affirmation_cols <- lst_new_affirmation_cols[sort(names(lst_new_affirmation_cols))]

  # Error messaging for fatal column mismatches
  .missing_cols_message <- function(affirmations, missing_columns) {
    error_affirmation <- cli::combine_ansi_styles(cli::style_bold, cli::col_yellow)
    error_col <- cli::combine_ansi_styles(cli::style_bold)

    cli::cli_inform(
      c("x" = "The current affirm report could not be updated due to missing columns in the
      following current {cli::qty(affirmations)} affirmation{?s}:"))
    cli::cli_text()

    cli::cli_ol()  # Start the main ordered list
    for (i in seq_along(affirmations)) {
      cli::cli_li(paste("Affirmation:", "{.code {error_affirmation(affirmations[[i]])}}"))
      cli::cli_ul()
      cli::cli_li("{.var {error_col(missing_columns[[i]])}}\n\n")
      cli::cli_end()  # End the unordered list
      cli::cli_end()  # End the current list item
    }
    cli::cli_text()
    cli::cli_end()  # End the main ordered list
    cli::cli_inform(c("i" = "Please add the missing columns to the current affirm session before attempting to update the current report."))
    cli::cli_text()
    cli::cli_abort(c('i' = "affirm Excel Report was not updated."), call = sys.call(-2))
  }

  # Create an empty list to store checks for each affirmations#
  lst_affirmation_col_match_checks <- list()

  # Identify which affirmations are newly added (not in previous report)
  vec_newly_added <- setdiff(vec_new_affirmation_names, vec_prev_affirmation_names)

  # Flag any affirmations that have columns not present in the previous affirmations...#
  for (affirmation_name in vec_new_affirmation_names){
    # ... but skip column checking for newly added affirmations
    if (affirmation_name %in% vec_newly_added) {
      lst_affirmation_col_match_checks[[affirmation_name]] <- FALSE
    } else {
      # For existing affirmations, check if previous columns are missing from new columns
      prev_cols <- lst_prev_affirmation_cols[[affirmation_name]]
      new_cols <- lst_new_affirmation_cols[[affirmation_name]]

      # Check if any previous columns are missing from new columns
      missing_cols <- setdiff(prev_cols, new_cols)
      lst_affirmation_col_match_checks[[affirmation_name]] <- length(missing_cols) > 0
    }
  }

  # Check to see if any affirmation columns were flagged for mismatches#
  col_match_check <- lst_affirmation_col_match_checks |> unlist() |> any()
  vec_col_match_indices <- sapply(lst_affirmation_col_match_checks, function(x) any(x))

  if(col_match_check){
    # If so, build out the error for the console#
    vec_bad_match_affirmations <- names(vec_col_match_indices)[vec_col_match_indices]

    lst_match_missing_columns <- list()

    for (affirmation_name in vec_bad_match_affirmations){
      lst_match_missing_columns[[affirmation_name]] <-
        # Pull out "prev" columns that are missing from the new columns#
        dplyr::setdiff(lst_prev_affirmation_cols[[affirmation_name]], lst_new_affirmation_cols[[affirmation_name]])
    }

    # Abort and send the message to the console
    .missing_cols_message(vec_bad_match_affirmations, lst_match_missing_columns)
  }

  #============================================================================#
  # Current Affirmation Dataframe Extractions-----------------------------------
  #============================================================================#
  # Pull out new affirmation dfs
  lst_new_affirmation_dfs <- list()

  for (affirmation_name in vec_new_affirmation_names){
    # Find the row index for this affirmation in df_summary_current
    row_index <- which(df_summary_current$affirmation_name == affirmation_name)

    new_affirmation <- affirmation_name %in% vec_newly_added

    if(new_affirmation){
      # If this is a new affirmation, create the join key now
      lst_new_affirmation_dfs[[affirmation_name]] <-
        lst_init_new_affirmation_dfs[[affirmation_name]] |>
        dplyr::select(dplyr::everything()) |>
        dplyr::mutate(
          join_key = do.call(paste, c(dplyr::across(dplyr::everything()), sep = " "))
        )
    } else{
      # Otherwise, pull the column names from the previous affirmations..
      # ...to select the correct fields from the new affirmation to make the join key
      lst_new_affirmation_dfs[[affirmation_name]] <-
        lst_init_new_affirmation_dfs[[affirmation_name]] |>
        dplyr::select(lst_prev_affirmation_cols[[affirmation_name]], dplyr::everything()) |>
        dplyr::mutate(
          join_key = do.call(
            paste,
            c(dplyr::select(df_summary_current[[row_index, "data"]][[1]], lst_prev_affirmation_cols[[affirmation_name]]),
              list(sep = " ")
            )
          )
        )
    }
  }

  #============================================================================#
  # Affirmation Updating Work---------------------------------------------------
  #============================================================================#
  # Create an empty list to store updated affirmations and join key duplication#
  lst_updated_affirmation_dfs <- list()
  lst_join_key_dupes <- list()

  # Join old and new affirmations by the join key#
  for (affirmation_name in vec_new_affirmation_names){
    lst_updated_affirmation_dfs[[affirmation_name]] <-
      lst_new_affirmation_dfs[[affirmation_name]] |>
      dplyr::left_join(
        lst_prev_affirmation_dfs[[affirmation_name]] |>
          dplyr::select("join_key", "Status", "Comment"),
        by = "join_key",
        # silences left_join warnings if dupes are detected (we'll throw an error later if dupes are present)
        relationship = "many-to-many"
      )

    # Search for potential duplicates in the join keys#
    lst_join_key_dupes[[affirmation_name]] <-
      lst_updated_affirmation_dfs[[affirmation_name]] |>
      dplyr::mutate(
        "row_id" = dplyr::row_number()
      ) |>
      dplyr::mutate(
        .by = "join_key",
        "dupe_count" = dplyr::n(),
        "flag_row" = .data$dupe_count > 1
      ) |>
      dplyr::reframe(
        .by = "join_key",
        "dupe_rows" = ifelse(.data$flag_row, knitr::combine_words(.data$row_id), NA)
      )

    # Remove the join keys#
    lst_updated_affirmation_dfs[[affirmation_name]] <-
      lst_updated_affirmation_dfs[[affirmation_name]] |>
      dplyr::select(-"join_key")
  }

  #============================================================================#
  ## Row Duplication Check------------------------------------------------------
  #============================================================================#
  # If there were duplicates, we'll throw an error...
  # ...as we can't uniquely identify what needs to be joined between the previous and new affirmations
  lst_dupe_check <- list()

  for (affirmation_name in vec_new_affirmation_names) {
    # Check for duplicates in new data
    vec_new_dupes <-
      lst_new_affirmation_dfs[[affirmation_name]] |>
      dplyr::count(.data$join_key) |>
      dplyr::filter(.data$n > 1) |>
      dplyr::pull(.data$join_key)

    # Check for duplicates in previous data
    vec_prev_dupes <-
      lst_prev_affirmation_dfs[[affirmation_name]] |>
      dplyr::count(.data$join_key) |>
      dplyr::filter(.data$n > 1) |>
      dplyr::pull(.data$join_key)

    # Get the row numbers for duplicates in the final joined result
    df_temp_joined <-
      lst_new_affirmation_dfs[[affirmation_name]] |>
      dplyr::left_join(
        lst_prev_affirmation_dfs[[affirmation_name]] |>
          dplyr::select("join_key", "Status", "Comment"),
        by = "join_key",
        relationship = "many-to-many" # silences left_join warnings if dupes are detected
      )

    # Store the dupe info to pull out for the error message
    df_final_dupes <-
      df_temp_joined |>
      dplyr::mutate(row_id = dplyr::row_number()) |>
      dplyr::add_count(.data$join_key, name = "dupe_count") |>
      dplyr::filter(.data$dupe_count > 1) |>
      dplyr::group_by(.data$join_key) |>
      dplyr::summarise(
        rows = list(.data$row_id),
        .groups = "drop"
      )

    # Grab the source of the duplicates if this df is populated
    if (nrow(df_final_dupes) > 0) {
      df_dupe_specs <-
        df_final_dupes |>
        dplyr::mutate(
          source = dplyr::case_when(
            join_key %in% vec_new_dupes & join_key %in% vec_prev_dupes ~ "both",
            join_key %in% vec_new_dupes ~ "new",
            join_key %in% vec_prev_dupes ~ "previous",
            # This should NEVER trigger, but leaving here for dev purposes
            .default = "unknown"
          )
        )

      lst_dupe_check[[affirmation_name]] <- df_dupe_specs
    } else {
      # Otherwise, no dupes were found, so set to NULL
      lst_dupe_check[[affirmation_name]] <- NULL
    }
  }

  # Check if any duplicates were found
  any_dupes <- any(sapply(lst_dupe_check, function(x) !is.null(x)))

  # If any dupes are found, throw an error
  if (any_dupes) {

    # Build out the error to show the duplicate row numbers and affirmation sources
    lst_dupe <- c()

    for (affirmation_name in names(lst_dupe_check)) {
      if (!is.null(lst_dupe_check[[affirmation_name]])) {
        df_dupe_info <- lst_dupe_check[[affirmation_name]]

        # Go through each flagged affirmation and pull out it's name and flagged duplicated rows
        for (i in seq_len(nrow(df_dupe_info))) {
          rows <- unlist(df_dupe_info$rows[i])
          source <- df_dupe_info$source[i]

          # Declare which affirmation has dupes
          source_text <-
            switch(
              source,
              "new" = "new data",
              "previous" = "previous data",
              "both" = "both new and previous data",
              "unknown" = "unknown source"
            )
          # just formatting
          row_text <- paste0("rows ", paste(rows, collapse = ", "))

          # This list will be passed through for the error
          lst_dupe <-
            append(
              lst_dupe, c(
                ">" = paste0("Duplicate data detected in affirmation '", affirmation_name,
                             "' at ", row_text, " (source: ", source_text, ").")
              ))
        }
      }
    }

    # Throw it to the console
    lst_dupe |>
      append(c("\n", "i" = "Please review and remove duplicate data before updating a previous Affirm Excel Report.")) |>
      cli::cli_abort(call = sys.call(-1))

  } else {
    # Otherwise, continue on..
    # Reorder the updated affirmation dfs to match the original order
    lst_ordered_updated_dfs <- lst_updated_affirmation_dfs[vec_new_affirmation_names]

    df_summary_updated <-
      df_summary_updated_init |>
      dplyr::mutate(data = lst_ordered_updated_dfs)
  }

  return(df_summary_updated)

}
