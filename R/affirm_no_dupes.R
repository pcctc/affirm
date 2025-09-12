#' Affirm Range
#'
#' A wrapper for `affirm_true()`.
#' The columns argument specifies which columns to check for duplicates. The function
#' creates a record ID, `record_id` for each row, then identifies whether each row represents
#' the first occurrence of a unique combination of values in the specified columns.
#' The resulting logical vector, `flag_duplicate` is passed to `affirm_true()`.
#'
#' @inheritParams affirm_true
#' @param columns columns to check duplicates among
#' @param id,priority,data_frames Optional additional information that will be passed to affirmation report.
#' - `id` must be an integer, e.g. `id = 1L`
#' - `priority` must be an integer, e.g. `priority = 1L`
#' - `data_frames` string of data frame names used in affirmation, e.g. `data_frames = "RAND, DM"`
#'
#' @return data frame
#' @export
#' @family Data Affirmations
#'
#' @section Using `affirm_no_dupes()` to detect duplicate values in specified columns:
#' `affirm_no_dupes()` adds two columns to the output data:
#'
#' \itemize{
#'   \item **`record_id`:** The row number from the original data frame.
#'   \item **`flag_duplicate`:** A Boolean (`TRUE`/`FALSE`) that indicates whether a row
#'   represents the first occurrence of a unique combination. The first instance
#'   of each unique combination is flagged as `TRUE`, while subsequent duplicates
#'   are flagged as `FALSE`.
#' }
#'
#' @examples
#' affirm_init(replace = TRUE)
#'
#' dplyr::as_tibble(mtcars) |>
#' dplyr::select(-c(am, vs)) |>
#' dplyr::arrange(cyl) |>
#'  affirm_no_dupes(
#'    label = "No duplicates in the number of cylinders",
#'    columns = cyl
#'  )
#'
#' affirm_close()
#'
affirm_no_dupes <- function(data,
                            label,
                            columns,
                            id = NA_integer_,
                            priority = NA_integer_,
                            data_frames = NA_character_,
                            report_listing = NULL,
                            data_action = NULL,
                            error = getOption("affirm.error", default = FALSE)) {
  # check and process inputs ---------------------------------------------------
  if (missing(data) || missing(columns) || missing(label)) {
    cli::cli_abort("Arguments {.code data}, {.code label}, and {.code columns} are required.")
  }
  columns <- dplyr::select(data, {{ columns }}) |> colnames()
  if (rlang::is_empty(columns)) {
    cli::cli_abort("The {.code columm} argument must select at least one column from {.code data}.")
  }
  data_action <- rlang::enquo(data_action)
  report_listing <- rlang::enquo(report_listing)
  if (.is_quo_null(report_listing))
    report_listing <-
    rlang::quo(
      dplyr::filter(., lgl_condition) |>
        dplyr::select(all_of(!!columns)) |>
        # Adds dupe info to the actual report listing
        dplyr::mutate(
          flag_duplicate = dplyr::n() > 1,
          record_id = dplyr::row_number()
        )) |>
    structure(.Environment = rlang::caller_env())

  # construct `condition=` argument --------------------------------------------
  quo_condition <-
    rlang::quo(
      dplyr::mutate(., record_id = dplyr::row_number()) |>
        dplyr::select(all_of(!!columns), "record_id") |>
        dplyr::mutate(
          .by = c(all_of(!!columns)),
          row_num = dplyr::row_number(),
          flag_duplicate = .data$row_num == 1
        ) |>
        dplyr::pull("flag_duplicate")
    ) |>
    structure(.Environment = rlang::caller_env())

  # Add dupe info to the actual data output ------------------------------------
  data_out <-
    data |>
    dplyr::mutate(record_id = dplyr::row_number()) |>
    dplyr::mutate(
      .by = c(all_of(!!columns)),
      row_num = dplyr::row_number(),
      flag_duplicate = .data$row_num != 1
    ) |>
    dplyr::select(-"row_num")

  # pass arguments to affirm_true() --------------------------------------------
  affirm_true(data = data_out,
              label = label,
              condition = !!quo_condition,
              id = id,
              priority = priority,
              data_frames = data_frames,
              columns = paste(columns, collapse = ", "),
              report_listing = !!report_listing,
              data_action = !!data_action,
              error = error)
}
