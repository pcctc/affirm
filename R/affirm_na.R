#' Affirm NA/Not NA
#'
#' A wrapper for `affirm_true()`.
#' The columns argument is used to construct the
#' `affirm_true(condition = is.na(column))` argument.
#'
#' @inheritParams affirm_true
#' @param column column to check NA values against
#' @param id,priority,data_frames Optional additional information that will be passed to affirmation report.
#' - `id` must be an integer, e.g. `id = 1L`
#' - `priority` must be an integer, e.g. `priority = 1L`
#' - `data_frames` string of data frame names used in affirmation, e.g. `data_frames = "RAND, DM"`
#'
#' @return data frame
#' @name affirm_na
#' @family Data Affirmations
#'
#' @examples
#' affirm_init(replace = TRUE)
NULL

#' @export
#' @rdname affirm_na
affirm_na <- function(data,
                      label,
                      column,
                      id = NA_integer_,
                      priority = NA_integer_,
                      data_frames = NA_character_,
                      report_listing = NULL,
                      data_action = NULL,
                      error = getOption("affirm.error", default = FALSE)) {
  # check and process inputs ---------------------------------------------------
  if (missing(data) || missing(column) || missing(label)) {
    cli::cli_abort("Arguments {.code data}, {.code label}, and {.code column} are required.")
  }
  column <- dplyr::select(data, {{ column }}) |> colnames()
  if (length(column) != 1L)
    cli::cli_abort("The {.code column} argument must select one and only one column.")
  report_listing <- rlang::enquo(report_listing)
  data_action <- rlang::enquo(data_action)

  # construct condition quo ----------------------------------------------------
  quo_condition <-
    rlang::quo(is.na(!!rlang::sym(column))) |>
    structure(.Environment = rlang::caller_env()) # add the calling env as the quo env attribute

  # pass arguments to affirm_true() --------------------------------------------
  affirm_true(data = data,
              label = label,
              condition = !!quo_condition,
              id = id,
              priority = priority,
              data_frames = data_frames,
              columns = column,
              report_listing = !!report_listing,
              data_action = !!data_action,
              error = error)

}

#' @export
#' @rdname affirm_na
affirm_not_na <- function(data,
                          label,
                          column,
                          id = NA_integer_,
                          priority = NA_integer_,
                          data_frames = NA_character_,
                          report_listing = NULL,
                          data_action = NULL,
                          error = getOption("affirm.error", default = FALSE)) {
  # check and process inputs ---------------------------------------------------
  if (missing(data) || missing(column)) {
    cli::cli_abort("Arguments {.code data} and {.code column} are required.")
  }
  column <- dplyr::select(data, {{ column }}) |> colnames()
  if (length(column) != 1L)
    cli::cli_abort("The {.code column} argument must select one and only one column.")
  report_listing <- rlang::enquo(report_listing)
  data_action <- rlang::enquo(data_action)

  # construct condition quo ----------------------------------------------------
  quo_condition <-
    rlang::quo(!is.na(!!rlang::sym(column))) |>
    structure(.Environment = rlang::caller_env()) # add the calling env as the quo env attribute

  # pass arguments to affirm_true() --------------------------------------------
  affirm_true(data = data,
              label = label,
              condition = !!quo_condition,
              id = id,
              priority = priority,
              data_frames = data_frames,
              columns = column,
              report_listing = !!report_listing,
              data_action = !!data_action,
              error = error)
}
