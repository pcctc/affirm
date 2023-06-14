#' Affirm False
#'
#' A wrapper for `affirm_true()`.
#' The `condition` argument is process and passed to
#' `affirm_true(condition = !condition)`
#'
#' @inheritParams affirm_true
#'
#' @return data frame
#' @family Data Affirmations
#'
#' @export
#' @examples
#' affirm_init(replace = TRUE)
#'
#' as_tibble(mtcars) |>
#'  affirm_false(
#'    label = "No. cylinders must be 4, 6, or 8",
#'    condition = !cyl %in% c(4, 6, 8)
#'  )
#'
#' affirm_close()
affirm_false <- function(data,
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
  if (missing(data) || missing(label) || missing(condition)) {
    cli::cli_abort("Arguments {.code data}, {.code label}, and {.code condition} are required.")
  }

  # construct condition to pass to `affirm_true()` -----------------------------
  condition <- rlang::enquo(condition)
  rlang::f_rhs(condition) <- rlang::expr(! (!!rlang::f_rhs(condition)))

  # pass arguments to affirm_true() --------------------------------------------
  affirm_true(data = data,
              label = label,
              condition = !!condition,
              id = id,
              priority = priority,
              data_frames = data_frames,
              columns = columns,
              report_listing = {{ report_listing }},
              data_action = {{ data_action }},
              error = error)
}
