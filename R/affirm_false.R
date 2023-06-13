
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
  browser()
  condition <- rlang::enquo(condition)
}
