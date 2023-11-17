# checks whether the affirm reporting env is empty
.is_affirm_env_empty <- function() {
  rlang::is_empty(ls(envir = env_affirm_logs))
}

.check_affirm_initialized <- function(msg = c("x" = "First run {.code affirm_init()} to begin affirmations.")) {
  if (isTRUE(.is_affirm_env_empty())) {
    cli::cli_abort(msg)
  }
}

# deletes the contents of the affirm reporting env
.del_affirm_env_contents <- function() {
  rm(list = ls(envir = env_affirm_logs), envir = env_affirm_logs)
}

# append results to the affirmation results tibble
.add_to_affirm_results <- function(..., data) {
  # add new issues to existing issues log tibble
  df <-
    affirm_report_raw_data() |>
    dplyr::bind_rows(
      dplyr::tibble(
        ...,
        data = list(data)
      )
    )

  # checking for duplicates in the label
  if (any(duplicated(df$label))) {
    # if interactive, be more forgiving about potentially rerunning the scripts again and again
    if (interactive()) cli::cli_warn(c("!" = "Duplicated labels...I sure hope you're just testing: {.val {df$label[duplicated(df$label)]}}."))
    else cli::cli_abort(c("x" = "Duplicated labels: {.val {unique(df$label[duplicated(df$label)])}}."))
  }

  assign(
    x = "df_affirmations",
    value = df,
    envir = env_affirm_logs
  )
}


# this is somewhat more complex than `rlang::quo_is_null()`
# the rlang function only tests for an EXACT NULL value.
# e.g. rlang::quo(NULL) |> rlang::quo_is_null() IS TRUE
# e.g. test_null <- NULL; rlang::quo(test_null) |> rlang::quo_is_null() IS FALSE
.is_quo_null <- function(x) {
  tryCatch(rlang::eval_tidy(x) |> is.null(), error = function(e) FALSE)
  # tryCatch(rlang::inject(is.null(!!x)), error = function(e) FALSE)
}
