#' Construct Report Listing Call
#'
#' @description
#' Each of the validation functions has a `affirm_*(report_listing)` argument.
#' This call constructs the data frame that is returned in the affirmation report,
#' and can be difficult to construct.
#'
#' The `construct_report_listing()` helps users construct this call.
#' The bang-bang operator (`!!`) must be used in the affirmation function call,
#' e.g. `affirm_*(report_listing = !!construct_report_listing())`.
#' See example below.
#'
#' @param rows expression that evaluates to a logical vector.
#' Default is `!.env$lgl_condition`. This logical vector is passed to `dplyr::filter()`
#' @param columns expression that includes the columns to include in report.
#' @param label logical indicating whether to add a new column with the
#' affirmation label.
#'
#' @return a quosure
#' @export
#'
#' @examples
#' # return columns am and cyl; add new column with affirmation label
#' construct_report_listing(columns = c(am, cyl), label = TRUE)
#'
#' # example how to use this function with an affirmation function
#' affirm_init(replace = TRUE)
#'
#' as_tibble(mtcars) |>
#'  affirm_true(
#'    label = "No. cylinders must be 4 or 6",
#'    condition = cyl %in% c(4, 6),
#'    report_listing = !!construct_report_listing(columns = c(am, cyl), label = TRUE)
#'  ) |>
#'  invisible()
#'
#'  affirm_report_raw_data()$data
#'
#' affirm_close()
construct_report_listing <- function(rows = NULL, columns = NULL, label = FALSE) {
  # capture rows and columns arguments -----------------------------------------
  rows <- rlang::enquo(rows)
  columns <- rlang::enquo(columns)
  if (.is_quo_null(rows))
    rows <- rlang::quo(!.env$lgl_condition)
  if (.is_quo_null(columns))
    columns <- rlang::quo(c(any_of(getOption("affirm.id_cols")), all.vars(condition)))

  # add rows expression to a filter() call -------------------------------------
  rows_expr <- rlang::expr(dplyr::filter(., !!rlang::get_expr(rows)))

  # add columns expression to a select() call ----------------------------------
  columns_expr <- rlang::expr(dplyr::select(!!rlang::get_expr(columns)))

  # create call to create a ..label.. variable ----------------------------------
  if (isTRUE(label)) {
    label_expr <- rlang::call2("mutate", ..label.. = expr(label))
  }
  else label_expr <- NULL

  # construct chained call -----------------------------------------------------
  all_exprs <- list(rows_expr, columns_expr, label_expr)
  # remove NULL elements
  all_exprs <- all_exprs[sapply(all_exprs, function(x) !is.null(x))]

  # construct final expression
  final_expr <-
    Reduce(
      f = function(x, y) expr(!!rlang::get_expr(x) %>% !!rlang::get_expr(y)),
      x = all_exprs
    )

  # convert the expression to a quo
  rlang::quo(!!final_expr) |>
    structure(.Environment = rlang::caller_env())
}
