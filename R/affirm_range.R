#' Affirm Range
#'
#'A wrapper for `affirm_true()`.
#' The column, range, and boundaries arguments are used to construct the
#' `affirm_true(condition = column >= range[1] & column <= range[2])` argument.
#'
#' @inheritParams affirm_true
#' @param column a single column to check values of
#' @param range vector of length two indicating the upper and lower bounds of the range.
#' The class of the `range` must be compatible with the `column`, e.g. if `column`
#' is numeric, `range` must also be numeric; if `column` is a date, range must be a
#' date; if `column` is an integer, `range` must be an integer, etc.
#' @param boundaries logical vector of length 2 indicating whether to include UB and LB in
#' the range check. Default is `c(TRUE, TRUE)`
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
#'  affirm_range(
#'    label = "MPG is >0 and <=30",
#'    column = mpg,
#'    range = c(0, 30),
#'    boundaries = c(FALSE, TRUE)
#'  )
#'
#' affirm_close()

affirm_range <- function(data,
                         label,
                         column,
                         range,
                         boundaries = c(TRUE, TRUE),
                         id = NA_integer_,
                         priority = NA_integer_,
                         data_frames = NA_character_,
                         report_listing = NULL,
                         data_action = NULL,
                         error = getOption("affirm.error", default = FALSE)) {
  # check and process inputs ---------------------------------------------------
  if (missing(data) || missing(column) || missing(range)) {
    cli::cli_abort("Arguments {.code data}, {.code column}, and {.code range} are required.")
  }
  if (!rlang::is_vector(range) || rlang::is_list(range) || length(range) != 2L) {
    cli::cli_abort("The {.code range} argument must be a vector of length 2.")
  }
  column <- dplyr::select(data, {{ column }}) |> colnames()
  if (length(column) != 1L)
    cli::cli_abort("The {.code column} argument must select one and only one column.")
  if (!inherits(data[[column]], class(range))) {
    cli::cli_abort(
      c("i" = "The class of column {.val {column}} ({.cls {class(data[[column]])}}) is incompatible with the class of {.code range} ({.cls {class(range)}}).",
        "x" = "Update the {.code range} argument class to match column {.val {column}}")
    )
  }
  if (!is.logical(boundaries)) {
    cli::cli_abort("The {.code boundaries} argument must be class logical.")
  }
  boundaries <- rep_len(boundaries, length.out = 2L)

  report_listing <- rlang::enquo(report_listing)
  data_action <- rlang::enquo(data_action)

  # construct condition quo() --------------------------------------------------
  if (isTRUE(boundaries[1]) && isTRUE(boundaries[2])) {
    quo_condition <-
      rlang::quo(!!rlang::sym(column) >= !!range[1] & !!rlang::sym(column) <= !!range[2])
  }
  else if (isTRUE(boundaries[1]) && isFALSE(boundaries[2])) {
    quo_condition <-
      rlang::quo(!!rlang::sym(column) >= !!range[1] & !!rlang::sym(column) < !!range[2])
  }
  else if (isFALSE(boundaries[1]) && isTRUE(boundaries[2])) {
    quo_condition <-
      rlang::quo(!!rlang::sym(column) > !!range[1] & !!rlang::sym(column) <= !!range[2])
  }
  else if (isFALSE(boundaries[1]) && isFALSE(boundaries[1])) {
    quo_condition <-
      rlang::quo(!!rlang::sym(column) > !!range[1] & !!rlang::sym(column) < !!range[2])
  }
  quo_condition <-
    quo_condition |>
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
