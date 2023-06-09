#' Affirm True
#'
#' Use this function to affirm an expression is true.
#'
#' @param data a data frame
#' @param label a string used to describe the affirmation
#' @param condition expression to check that evaluates to a logical vector, e.g. `cyl %in% c(4, 6, 8)`.
#' Use the dot (`.`) to reference the passed data frame. If condition results
#' in a missing value, it is interpreted as `FALSE`.
#' @param report_listing an expression selecting/filtering rows from `data=` to return in the
#' issue listing report. The default is to return the result from `create_report_listing()`,
#' which are the rows that do *not* met in `condition=`
#' and columns included in the `condition=` expression along with any columns
#' set in `option('affirm.id_cols')`. The `'affirm.id_cols'` option must be a
#' character vector of column names, where columns will be selected with
#' `select(any_of(getOption('affirm.id_cols')))`.
#' @param data_action this expression is executed at the end of the function call when supplied.
#' - Default is NULL, and the passed data frame in `data=` is returned unaltered.
#' - Perhaps you'll need to remove problematic rows: `data_action = filter(., !(!!condition))`
#' @param error Logical indicating whether to throw an error when condition is not met. Default is `FALSE`.
#' @param id,priority,data_frames,columns Optional additional information that will be passed to affirmation report.
#' - `id` must be an integer, e.g. `id = 1L`
#' - `priority` must be an integer, e.g. `priority = 1L`
#' - `data_frames` string of data frame names used in affirmation, e.g. `data_frames = "RAND, DM"`
#' - `columns` string of column names used in affirmation. default is `all.vars(condition)`
#'
#' @details
#' When passing expressions to arguments `report_listing=` and `data_action=`,
#' there are a few things to keep in mind.
#' - The expression passed in `condition=` can be used, but note that it has
#'   been captured as an expression inside the function. This means that to
#'   use it, you'll need to use `!!` (bang-bang) to pass it inside a function.
#' - In addition to being able to use the `condition=` expression, you can simplify
#'   your code somewhat by referring to `lgl_condition`, which is an evaluated logical
#'   vector of the `condition=` expression.
#'
#'
#' @return data frame
#' @family Data Affirmations
#' @export
#'
#' @examples
#' affirm_init(replace = TRUE)
#'
#' as_tibble(mtcars) |>
#'  affirm_true(
#'    label = "No. cylinders must be 4, 6, or 8",
#'    condition = cyl %in% c(4, 6, 8)
#'  )
#'
#' affirm_close()
affirm_true <- function(data,
                        label,
                        condition,
                        id = NA_integer_,
                        priority = NA_integer_,
                        data_frames = NA_character_,
                        columns = NA_character_,
                        report_listing = NULL,
                        data_action = NULL,
                        error = getOption("affirm.error", default = FALSE)) {
  # check inputs ---------------------------------------------------------------
  .check_affirm_initialized()
  .check_args_affirm_true(data = data, label = label, id = id, priority = priority, data_frames = data_frames, columns = columns)

  # capture inputs as expressions ----------------------------------------------
  condition <- rlang::enquo(condition)
  report_listing <- rlang::enquo(report_listing)
  data_action <- rlang::enquo(data_action)
  columns <- dplyr::coalesce(columns, all.vars(condition) |> paste(collapse = ", "))
  if (.is_quo_null(report_listing))
    report_listing <-
    rlang::quo(filter(., !.env$lgl_condition) |> select(any_of(getOption("affirm.id_cols")), any_of(!!all.vars(condition)))) |>
    structure(.Environment = rlang::caller_env()) # add the calling env as the quo env attribute

  # make affirmation -----------------------------------------------------------
  lgl_condition <- .evaluate_condition_argument(data, condition, label, error)

  # issues export --------------------------------------------------------------
  df_report_listing <-
    rlang::eval_tidy(report_listing, data = list('.' = data, lgl_condition = lgl_condition, label = label))

  if (!is.data.frame(df_report_listing)) {
    cli::cli_abort(c("x" = "The result of evaluated expression of {.code report_listing=} must be a data frame."))
  }

  # append results to affirmation data frame -----------------------------------
  .add_to_affirm_results(
    label = label,
    # using rep_len() in case condition is a single T/F
    error_n =
      rep_len(!lgl_condition, length.out = nrow(data)) |> sum(),
    total_n = nrow(data),
    data = df_report_listing,
    id = id,
    priority = priority,
    data_frames = data_frames,
    columns = columns
  )
  cli::cli_inform(c("*" = label))
  cli::cli_inform(c(" " = "{.val {sum(!lgl_condition)}} issue{?s} identified."))

  # take an additional action if requested -------------------------------------
  data_action_is_null <- .is_quo_null(data_action)
  if (!data_action_is_null) {
    data <-
      rlang::eval_tidy(
        expr = data_action,
        data = as.list(data) |> c(list('.' = data, lgl_condition = lgl_condition, label = label))
      )
  }

  # return data frame ----------------------------------------------------------
  data
}



# this function evaluates the `condition` expression and performs checks on the results
.evaluate_condition_argument <- function(data, condition, label, error) {
  lgl_condition <-
    rlang::eval_tidy(
      expr = condition,
      data = as.list(data) |> c(list('.' = data))
    )

  if (rlang::is_list(lgl_condition) || !rlang::is_vector(lgl_condition) ||
      !inherits(lgl_condition, "logical") || !length(lgl_condition) %in% c(1L, nrow(data))) {
    paste("The {.code condition=} expression must resolve to a vector of class {.cls logical}",
          "with length {.val {1L}} or {.val {nrow(data)}} (size of the data frame).") |>
      cli::cli_abort()
  }

  # any missing values are interpreted as FALSE
  lgl_condition <- dplyr::coalesce(lgl_condition, FALSE)

  if (isTRUE(error) && any(!lgl_condition)) {
    cli::cli_abort(c("x" = "Affirmation {.val {label}} did not pass for {.val {sum(!lgl_condition)}} observation{?s}."))
  }

  lgl_condition
}

.check_args_affirm_true <- function(data, label, id, priority, data_frames, columns) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.code data=} argument must be a data frame.")
  }
  if ("." %in% names(data)) {
    cli::cli_abort("The data frame cannot have a column named {.val .}.")
  }
  if (!rlang::is_string(label) ||
      (!is.na(data_frames) && !rlang::is_string(data_frames)) ||
      (!is.na(columns) && !rlang::is_string(columns))) {
    cli::cli_abort(c("x" = "Arguments {.code label=}, {.code data_frames=}, {.code columns=} must be a string of length one."))
  }
  if (!rlang::is_integerish(id, n = 1) || !rlang::is_integerish(priority, n = 1)) {
    cli::cli_abort("Arguments {.code id=}, {.code priority=} must be a integers of length one.")
  }
}
