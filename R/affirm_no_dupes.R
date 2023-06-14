#' Affirm Range
#'
#'A wrapper for `affirm_true()`.
#' The columns argument is used to construct the
#' `affirm_true(condition = select(., all_of(columns)) |> duplicated())` argument.
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
#' @examples
#' affirm_init(replace = TRUE)
#'
#' as_tibble(mtcars) |>
#'  affirm_no_dupes(
#'    label = "No duplicates in the number of cylinders",
#'    columns = cyl
#'  )
#'
#' affirm_close()
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
    rlang::quo(filter(., lgl_condition) |> select(all_of(!!columns)) |> dplyr::distinct()) |>
    structure(.Environment = rlang::caller_env())

  # construct `condition=` argument --------------------------------------------
  quo_condition <-
    rlang::quo(select(., all_of(!!columns)) |> Negate(f = duplicated)()) |>
    structure(.Environment = rlang::caller_env())

  # pass arguments to affirm_true() --------------------------------------------
  affirm_true(data = data,
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
