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
  if (is.null(data)) return("&mdash;")

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
          `border-radius` = "4px"
        ),
        "CSV"
      )
    )
  )
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

  # Check if sheet type metadata is in the previous report
  prev_metadata <- !prev_wb[["sheet_types"]] |> is.null()

  # If sheet type metadata was found...
  if(prev_metadata){
    # Find the matching sheet indices
    vec_affirmation_indices <- which(prev_wb[["sheet_types"]] == "affirmation")
    # And pull out the names that way
    vec_prev_affirmation_names <- prev_wb[["sheet_names"]][vec_affirmation_indices]
    # pull out names of any "Other" sheets too
    prev_other_sheets <- prev_wb[["sheet_names"]][-c(prev_summary_sheet, vec_affirmation_indices)]

  } else{

  # Send a warning to the console that sheet type couldn't be determined
    c(
      "i" = "Sheet type metadata was not found in the previous report file.",
      "!" = "Assuming every sheet after the 'Summary' sheet in the previous report file is an affirmation."
    ) |>
    cli::cli_warn()

    # Determine if there are any "other" sheets present before the summary sheet
    other_present <- prev_summary_sheet - 1 != 0

    # If "other sheets were found, save that info
    if(other_present){
      prev_other_sheets <- prev_wb[["sheet_names"]][c(1:(prev_summary_sheet-1))]
      prev_other_indices <- which(prev_wb[["sheet_names"]] %in% prev_other_sheets)
      # Pull all sheets except for any "other" and summary ones (first one - usually)
      vec_prev_affirmation_names <- prev_wb[["sheet_names"]][-c(prev_other_indices, prev_summary_sheet)]
    }

  }

  # Remove any old sheets that are getting dropped if applicable
  vec_prev_affirmation_names <- vec_prev_affirmation_names[vec_prev_affirmation_names %in% vec_new_affirmation_names]

  if(other_present){
    # Determine which sheets are summary and affirmations
    prev_sheets_to_drop <- c("Summary", vec_prev_affirmation_names)

    prev_wb_other <- prev_wb

    for (i in seq_along(prev_sheets_to_drop)){
      # Iteratively, drop each summary/affirmation sheet
      # So we're left with the other sheets

      prev_wb_other <-
        prev_wb_other |>
        openxlsx2::wb_remove_worksheet(prev_sheets_to_drop[i])
    }

    # Remove the "other" sheet from prev_wb to continue to update
      for (i in seq_along(prev_other_sheets)){
      prev_wb <-
        prev_wb |>
        openxlsx2::wb_remove_worksheet(prev_other_sheets[i])
      }
  }

  # Pull out the old summary's affirmation names, assigned to, status, and comments
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
  lst_init_new_affirmation_dfs <- list()

  for (i in seq_len(nrow(df_summary_current))){
    lst_init_new_affirmation_dfs[[i]] <-
      df_summary_current[[i, "data"]][[1]]
  }

  names(lst_init_new_affirmation_dfs) <- vec_new_affirmation_names

  #============================================================================#
  # Previous Affirmation Dataframe Extractions----------------------------------
  #============================================================================#
  # Initialize a list to pull out old affirmation dfs
  lst_prev_affirmation_dfs <- list()

  for (i in seq_len(length(vec_prev_affirmation_names))){
    lst_prev_affirmation_dfs[[i]] <-

      # If a current affirmation is empty
      # Fill it with placeholders#
      if(df_summary_current[[i, "data"]][[1]] |> nrow() == 0){
        dplyr::tibble(
          join_key = character(),
          Status = NA,
          Comment = NA
        )

      } else{
        # Otherwise, pull in the previous report#
        openxlsx2::wb_to_df(
          prev_wb,
          sheet = i + 1,
          start_row = 4,
          skip_empty_cols = TRUE
        ) |>
          dplyr::mutate(
            join_key = do.call(
              paste,
              c(dplyr::select(
                openxlsx2::wb_to_df(
                  prev_wb,
                  sheet = i + 1,
                  start_row = 4,
                  skip_empty_cols = TRUE
                ),dplyr::everything(), -c("Status", "Comment")),
                list(sep = " ")
              )
            )
          )
      }
  }

  names(lst_prev_affirmation_dfs) <- vec_prev_affirmation_names
  #============================================================================#
  #Mismatched Affirmation Work--------------------------------------------------
  #============================================================================#

  # Account for newly added affirmations
  # Assess if any affirmations are missing from the previous affirmation
  # Get the total affirmations present in current and previous reports
  new_length <- vec_new_affirmation_names |> length()
  prev_length <- vec_prev_affirmation_names |> length()
  any_missing <- abs(new_length - prev_length) > 0


  # If a difference in length is found between the 2 reports...
  if(any_missing){

    # Determine which one is "smaller" (missing dfs)
    smaller_report <- which.min(c("new" = new_length, "prev" = prev_length)) |> names()

    missing_dfs <- setdiff(vec_new_affirmation_names, vec_prev_affirmation_names)

    if(smaller_report == "prev"){

      for (i in seq_along(missing_dfs)){
        # Get the name from missing_dfs
        df_to_port <- missing_dfs[i]

        lst_prev_affirmation_dfs[[df_to_port]] <-
          lst_init_new_affirmation_dfs[[df_to_port]] |>
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


  # Pull out prev affirmation columns
  lst_prev_affirmation_cols <-
    lapply(
      lst_prev_affirmation_dfs,
      function(x) x |> names() |> dplyr::setdiff(c("Status", "Comment", "join_key"))
    )



  #Prev df col ammendment before joining
  for (i in seq_len(length(lst_prev_affirmation_dfs))){
    lst_prev_affirmation_dfs[[i]] <-

      # Ensure only applicable columns are carried forward
      lst_prev_affirmation_dfs[[i]] |>
          dplyr::select(
            "join_key", "Status", "Comment"
          )

  }

  #============================================================================#
  # Mismatched Affirmation Columns Check----------------------------------------
  #============================================================================#
  # Create an initial list of new affirmation columns#
  lst_new_affirmation_cols <-
    lapply(
      lst_init_new_affirmation_dfs,
      function(x) x |> names()
    )

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

  # Flag any new affirmations that have columns not present in the previous affirmations#
  for (i in seq_along(lst_new_affirmation_cols)){
    lst_affirmation_col_match_checks[[i]] <-
      # Mismatched columns from old to new is not acceptable
      # So flag as TRUE if any new column names don't appear in previous column names
     !dplyr::setdiff(lst_prev_affirmation_cols[[i]], c("Status", "Comment"))  %in% lst_new_affirmation_cols[[i]]
  }

  # Check to see if any affirmation columns were flagged for mismatches#
  col_match_check <- lst_affirmation_col_match_checks |> unlist() |> any()
  vec_col_match_indices <- sapply(lst_affirmation_col_match_checks, function(x) any(x))

  if(col_match_check){
    # If so, build out the error for the console#
    vec_bad_match_affirmations <- vec_new_affirmation_names[vec_col_match_indices]

    lst_match_missing_columns <- list()

    for (i in seq_along(vec_bad_match_affirmations)){
      lst_match_missing_columns[[i]] <-
        # Pull out "prev" columns that are missing from the new columns#
        dplyr::setdiff(lst_prev_affirmation_cols[[i]], lst_new_affirmation_cols[[i]])
    }

    # Abort and send the message to the console
    .missing_cols_message(vec_bad_match_affirmations, lst_match_missing_columns)

  }

  #============================================================================#
  # Current Affirmation Dataframe Extractions-----------------------------------
  #============================================================================#
  # Pull out new affirmation dfs
  lst_new_affirmation_dfs <- list()

  for (i in seq_len(nrow(df_summary_current))){

    lst_new_affirmation_dfs[[i]] <-
      lst_init_new_affirmation_dfs[[i]] |>
      dplyr::select(lst_prev_affirmation_cols[[i]], dplyr::everything()) |>
      mutate(
        join_key = do.call(
          paste,
          c(dplyr::select(df_summary_current[[i, "data"]][[1]], lst_prev_affirmation_cols[[i]]),
            list(sep = " ")
          )
        )
        )
  }

  #============================================================================#
  # Affirmation Updating Work---------------------------------------------------
  #============================================================================#

  # Create an empty list to store updated affirmations and join key duplication#
  lst_updated_affirmation_dfs <- list()
  lst_join_key_dupes <- list()


  # Join old and new affirmations#
  for (i in seq_len(nrow(df_summary_updated_init))){
    lst_updated_affirmation_dfs[[i]] <-
      lst_new_affirmation_dfs[[i]] |>
      dplyr::left_join(
        lst_prev_affirmation_dfs[[i]] |>
          dplyr::select("join_key", "Status", "Comment"),
        by = "join_key"
      );

    # Search for potential duplicates in the join keys#
    lst_join_key_dupes[[i]] <-
      lst_updated_affirmation_dfs[[i]] |>
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
      );

    # Grab the affirmations names to be used throughout
    names(lst_join_key_dupes)[[i]] <- vec_new_affirmation_names[[i]]

    # Remove the join keys#
    lst_updated_affirmation_dfs[[i]] <-
      lst_updated_affirmation_dfs[[i]] |>
      dplyr::select(-"join_key")
  }

  #============================================================================#
  ## Row Duplication Check------------------------------------------------------
  #============================================================================#
 # Check all join keys for duplications#
  any_dupes <-
    !lst_join_key_dupes |>
    lapply( `[[`, "dupe_rows") |> unlist() |> is.na() |> all()

  # If any dupes are found, create a custom cli message to alert the user#
  if (any_dupes){

  # Create lists to check the join keys by affirmation#
  lst_join_key_check <- list()
  lst_n_dupes <- list()

  # Pull out the rows of known duplicates for error messaging#
  for (i in seq_len(nrow(df_summary_updated_init))){
    lst_join_key_check[[i]] <-
      lst_join_key_dupes[[i]]$dupe_rows[which(!is.na(lst_join_key_dupes[[i]]$dupe_rows))] |>
      unique()

    # Pull out the total unique duplicates found for cli pluralization#
    lst_n_dupes[[i]] <- length(lst_join_key_check[[i]])
  }

  # Set the names of the affirmations to keep it organized
  names(lst_join_key_check) <- vec_new_affirmation_names
  names(lst_n_dupes) <- vec_new_affirmation_names
  names(lst_updated_affirmation_dfs) <- vec_new_affirmation_names

  # Create an empty vector for dupes messaging#
  dupe_list <- c()

    for (i in seq_len(lst_join_key_check |> length())){
      dupe_list[i] <- c(
        ">" = "Duplicate {cli::qty(lst_n_dupes[[1]])} row{?s} detected in affirmation {cli::col_yellow(cli::style_bold(cli::style_italic(names(lst_join_key_check)[1])))} at {cli::qty(lst_n_dupes[[1]])} row{?s} {cli::col_red(lst_join_key_check[[1]])}."
      )
    }

    # Print to the console#
    dupe_list |>
      append(c("\n","i" = "Please review and remove duplicate data before updated a previous Affirm Excel Report.")) |>
      # call needed to have the error reference affirm_report_excel fx
      cli::cli_abort(call = sys.call(-1))

  } else{
    # Otherwise continue on#
    df_summary_updated <-
      df_summary_updated_init |>
      dplyr::mutate(data = lst_updated_affirmation_dfs)
  }

  # If other sheets were found, set it up to be referenced
  # by outputting a list of objects...
  if(other_present){
    lst_output <-
      list(
        "prev_wb_other" = prev_wb_other,
        "df_summary" = df_summary_updated
      )

    return(lst_output)

  } else {
    # Otherwise, Just return the updated data
    lst_output <-
      list(
        "df_summary" = df_summary_updated
      )

    return(lst_output)
  }

}

