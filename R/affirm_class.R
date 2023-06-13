#' Affirm Class
#'
#' A wrapper for `affirm_true()`.
#' Reports `columns` that do not inherit `class`, e.g.
#' `select(data, all_of(columns) && where(\(x) !inherits(x, class)))`
#'
#' @inheritParams affirm_true
#' @param columns columns to check class
#' @param class character class to affirm
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
#' affirm_class(
#'   as_tibble(iris),
#'   label = "all cols are numeric (but Species really isn't)",
#'   columns = everything(),
#'   class = "numeric"
#' )
#'
#' affirm_close()
affirm_class <- function(data,
                         label,
                         columns,
                         class,
                         id = NA_integer_,
                         priority = NA_integer_,
                         data_frames = NA_character_,
                         report_listing = NULL,
                         data_action = NULL,
                         error = getOption("affirm.error", default = FALSE)) {
  # check and process inputs ---------------------------------------------------
  if (missing(data) || missing(columns) || missing(class)) {
    cli::cli_abort("Arguments {.code data}, {.code columns}, and {.code class} are required.")
  }
  columns <- dplyr::select(data, {{ columns }}) |> colnames()
  if (rlang::is_empty(columns))
    cli::cli_abort("The {.code columns} argument must select at least one column.")
  data_action <- rlang::enquo(data_action)
  report_listing <- rlang::enquo(report_listing)
  if (.is_quo_null(report_listing)) {
    report_listing <-
      rlang::quo(select(., all_of(!!columns) & where(\(x) !inherits(x, !!class)))) |>
      structure(.Environment = rlang::caller_env()) # add the calling env as the quo env attribute
  }

  # identify mis-matched columns -----------------------------------------------
  bad_class_cols <-
    select(data, all_of(columns) & where(\(x) !inherits(x, class))) |>
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
              columns = paste(columns, collapse = ", "),
              report_listing = !!report_listing,
              data_action = !!data_action,
              error = error)
}
