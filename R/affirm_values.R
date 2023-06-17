#' Affirm Values
#'
#'A wrapper for `affirm_true()`.
#' The column and value arguments are used to construct the
#' `affirm_true(condition = column %in% value)` argument.
#'
#' @inheritParams affirm_true
#' @param column a single column to check values of
#' @param values vector of values the `column=` may take on
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
#' dplyr::as_tibble(mtcars) |>
#'  affirm_values(
#'    label = "No. cylinders must be 4, 6, or 8",
#'    column = cyl,
#'    values = c(4, 6, 8)
#'  )
#'
#' affirm_close()
affirm_values <- function(data,
                          label,
                          column,
                          values,
                          id = NA_integer_,
                          priority = NA_integer_,
                          data_frames = NA_character_,
                          report_listing = NULL,
                          data_action = NULL,
                          error = getOption("affirm.error", default = FALSE)) {
  # process column argument ----------------------------------------------------
  column <- dplyr::select(data, {{ column }}) |> colnames()
  if (length(column) != 1L)
    cli::cli_abort("The {.code column} argument must select one and only one column.")
  if (missing(values) || !rlang::is_vector(values) || rlang::is_list(values))
    cli::cli_abort("The {.code values} argument must be a vector.")

  report_listing <- rlang::enquo(report_listing)
  data_action <- rlang::enquo(data_action)

  # construct condition quo() --------------------------------------------------
  quo_condition <-
    rlang::quo(!!rlang::sym(column) %in% !!values) |>
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
