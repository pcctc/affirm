#' Affirm Clean Join
#'
#' A wrapper for `affirm_true()`.
#' Reports `columns` whose names end with `".x"` or `".y"`, indicating
#' a sloppy merge.
#'
#' @inheritParams affirm_true
#' @param id,priority,data_frames Optional additional information that will be passed to affirmation report.
#' - `id` must be an integer, e.g. `id = 1L`
#' - `priority` must be an integer, e.g. `priority = 1L`
#' - `data_frames` string of data frame names used in affirmation, e.g. `data_frames = "RAND, DM"`
#'
#' @return data frame
#' @name affirm_na
#' @family Data Affirmations
#'
#' @export
#' @examples
#' affirm_init(replace = TRUE)
#'
#' df <-
#'   dplyr::tibble(lgl = c(NA, TRUE, NA, FALSE, NA)) |>
#'   dplyr::mutate(id = dplyr::row_number())
#'
#' affirm_clean_join(
#'   dplyr::full_join(df, df, by = "id"),
#'   label = "Checking for clean merge"
#' )
#'
#' affirm_close()
affirm_clean_join <- function(data,
                              label,
                              id = NA_integer_,
                              priority = NA_integer_,
                              data_frames = NA_character_,
                              report_listing = NULL,
                              data_action = NULL,
                              error = getOption("affirm.error", default = FALSE)) {
  # check and process inputs ---------------------------------------------------
  if (missing(data)) {
    cli::cli_abort("Arguments {.code data} and {.code label} are required.")
  }
  data_action <- rlang::enquo(data_action)
  report_listing <- rlang::enquo(report_listing)
  if (.is_quo_null(report_listing)) {
    report_listing <-
      rlang::quo(select(., ends_with(".x") | ends_with(".y"))) |>
      structure(.Environment = rlang::caller_env()) # add the calling env as the quo env attribute
  }

  # identify bad column names --------------------------------------------------
  bad_class_cols <-
    select(data, ends_with(".x") | ends_with(".y")) |>
    colnames()

  # construct condition quo ----------------------------------------------------
  quo_condition <-
    if (rlang::is_empty(bad_class_cols))
      condition <- rlang::quo(FALSE)
  else
    condition <- rlang::quo(FALSE)
  condition <- condition |>
    structure(.Environment = rlang::caller_env()) # add the calling env as the quo env attribute

  # pass arguments to affirm_true() --------------------------------------------
  affirm_true(data = data,
              label = label,
              condition = !!quo_condition,
              id = id,
              priority = priority,
              data_frames = data_frames,
              columns = "All Columns",
              report_listing = !!report_listing,
              data_action = !!data_action,
              error = error)
}
