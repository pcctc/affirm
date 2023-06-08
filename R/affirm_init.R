#' Begin Affirmations
#'
#' Run this function to initialize a new affirmation report
#'
#' @param replace logical indicating whether to replace/delete an existing or
#' in-progress affirmation report. Default is `NA`, and user will interactively
#' be asked whether to replace a report if it exists.
#'
#' @return NULL
#' @name affirm_init
#'
#' @examples
#' affirm_init()
#'
#' affirm_close()
NULL

#' @rdname affirm_init
#' @export
affirm_init <- function(replace = NA) {
  # check inputs ---------------------------------------------------------------
  replace <- .check_replace_argument(replace)

  # if replace is FALSE and env not empty, quit
  if (isFALSE(replace) && !.is_affirm_env_empty()) {
    cli::cli_abort("Affirmations are in-progress and will {.strong not} be replaced. New affirmations report {.strong not} initialized.")
  }

  # clean the environment for new affirmations to be made
  affirm_close()

  # add an empty tibble that will we store the check results into
  assign(
    x = "df_affirmations",
    value =
      dplyr::tibble(
        id = integer(),
        label = character(),
        priority = integer(),
        data_frames = character(),
        columns = character(),
        error_n = integer(),
        total_n = integer(),
        data = list()
      ),
    envir = env_affirm_logs
  )

  cli::cli_inform(c("v" = "We're ready to make data affirmations..."))
  return(invisible())
}

#' @rdname affirm_init
#' @export
affirm_close <- function() {
  .del_affirm_env_contents()
}


#' Interactively resolve the 'replace' argument
#'
#' Checks the passed value of the replace argument, and
#' interactively asks whether to replace an in-progress affirmation
#'
#' @param replace logical indicating whether to replace/delete an in-progress affirm report
#'
#' @return logical
#' @export
#'
#' @noRd
.check_replace_argument <- function(replace) {
  # if NA and interactive, ask user if they want to replace contents
  if (is.na(replace) && interactive() && !.is_affirm_env_empty()) {
    lgl <- c(TRUE, FALSE)
    cli::cli_inform(c(">" = "Would you like to {.strong replace} the in-progress {.pkg affirm} report?"))
    replace <- lgl[utils::menu(lgl)]
  }

  # if replace is NA and nothing to replace, then make replace = TRUE
  if (is.na(replace) && .is_affirm_env_empty()) {
    replace <- TRUE
  }

  # check value of replace argument
  if (!rlang::is_scalar_logical(replace)) {
    cli::cli_abort("The {.code replace} argument must be a scalar {.cls logical}.")
  }
  replace
}


