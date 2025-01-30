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

  # data frame of single affirmation results
  if(prev_exists){
    df_affirmation <-
      df_summary_row[["data"]][[1]]
  } else{
    df_affirmation <-
      df_summary_row[["data"]][[1]] |>
      dplyr::mutate(
        Status = NA,
        Comment = NA
      )
  }

  # labels of the data frame of single affirmation results

  df_labels <- .retrieve_labels(df_affirmation)
  vec_widths <- .compute_col_width(df_affirmation)

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
          bold = "double"
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
          italic = "italic",
          bold = "bold",
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
.update_summary_sheet <- function(df_summary_current, prev_wb){

  # Pull previous wb into the environment
  prev_wb <- openxlsx2::wb_load(prev_wb)
  prev_affirmation_sheets <- prev_wb$sheet_names[-1]

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
    )

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

  # Pull out new affirmation dfs
  lst_new_affirmation_dfs <- list()

  for (i in seq_len(nrow(df_summary_current))){

    lst_new_affirmation_dfs[[i]] <-

      df_summary_current[[i, "data"]][[1]] |>
      dplyr::mutate(
        join_key = do.call(
          paste,
          c(dplyr::select(df_summary_current[[i, "data"]][[1]], dplyr::everything()),
            list(sep = " ")
          )
        )
      )
  }

  vec_affirmation_names <- df_summary_current |> dplyr::pull("affirmation_name")

  names(lst_new_affirmation_dfs) <- vec_affirmation_names

  # Pull out old affirmation dfs
  lst_prev_affirmation_dfs <- list()

  for (i in seq_len(length(prev_affirmation_sheets))){
    lst_prev_affirmation_dfs[[i]] <-

      # If an affirmation is empty
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
          ) |>
          dplyr::select(
            "join_key", "Status", "Comment"
          )
      }
  }

  # Create an empty list to store updated affirmations#
  lst_updated_affirmation_dfs <- list()
  lst_join_key_dupes <- list()

  # Join old and new affirmations#
  for (i in seq_len(nrow(df_summary_updated_init))){
    lst_updated_affirmation_dfs[[i]] <-
      lst_new_affirmation_dfs[[i]] |>
      dplyr::left_join(lst_prev_affirmation_dfs[[i]], by = "join_key");

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
    names(lst_join_key_dupes)[[i]] <- vec_affirmation_names[[i]]

    # Remove the join keys#
    lst_updated_affirmation_dfs[[i]] <-
      lst_updated_affirmation_dfs[[i]] |>
      dplyr::select(-"join_key")
  }

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

  # Pull out the names of the affirmations to keep it organized
  names(lst_join_key_check) <- vec_affirmation_names
  names(lst_n_dupes) <- vec_affirmation_names
  names(lst_updated_affirmation_dfs) <- vec_affirmation_names

  # Add a check to detect duplicates#
  any_dupes <- !lst_join_key_check |> unlist() |> rlang::is_bare_logical()

  # Create an empty vector for possible dupes messaging#
  dupe_list <- c()

  # If any dupes are found, create a custom cli message to alert the user#
  if (any_dupes){
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

  return(df_summary_updated)

}

