#' Prepend DF Name to Column Names
#'
#' @param data a data frame
#' @param df_name string indicating the data frame name to prepend to the column names.
#' If not supplied, function will try to identify the data frame name.
#' NOTE: We can only get the correct name if the data frame has been piped
#' directly to this function without any other piped function between.
#' @param include tidyselect expression to identify columns to modify name.
#' Default is all columns, except those identified in `options("affirm.id_cols")`.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' DM |>
#'  prepend_df_name()
prepend_df_name <- function(data, df_name = NULL, include = c(everything(), -any_of(getOption("affirm.id_cols")))) {
  include <- rlang::enexpr(include)

  # process df_name argument ---------------------------------------------------
  df_name <-
    df_name %||%
    tryCatch(
      match.call() |>
        as.list() %>%
        `[`("data") |>
        as.character() |>
        setdiff("."),
      error = function(e) NULL
    )

  if (rlang::is_empty(df_name) || !rlang::is_string(df_name)) {
    c("x" = paste("Could not determine the name of the passed data frame.",
                  "Specify the {.code prepend_df_name(df_name)} argument.")) |>
      cli::cli_abort()
  }

  # add data frame name to the variable name -----------------------------------
  dplyr::rename_with(data, .fn = ~paste0(df_name, ".", .), .cols = !!include)
}
