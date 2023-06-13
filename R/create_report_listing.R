# create_report_listing <- function(rows = !.env$lgl_condition,
#                                   columns = c(any_of(getOption("affirm.id_cols")), all.vars(condition)),
#                                   flag_name = FALSE) {
#   # capture rows and columns arguments -----------------------------------------
#   rows <- rlang::enexpr(rows)
#   columns <- rlang::enexpr(columns)
#
#   # add rows expression to a filter() call -------------------------------------
#   if (!.is_quo_null(rows)) {
#     rows_expr <- rlang::expr(dplyr::filter(!!rows))
#   }
#   else rows_expr <- NULL
#
#   # add columns expression to a select() call ----------------------------------
#   if (!.is_quo_null(columns)) {
#     columns_expr <- rlang::expr(dplyr::select(!!columns))
#   }
#   else columns_expr <- NULL
#
#   # create call to create a ..flag.. variable ----------------------------------
#   if (isTRUE(flag_name)) {
#     flag_expr <- rlang::call2("mutate", ..flag.. = expr(lgl_condition))
#   }
#   else flag_expr <- NULL
#
#   # construct chained call -----------------------------------------------------
#   all_exprs <- list(rlang::expr(.), rows_expr, columns_expr, flag_expr)
#   # remove NULL elements
#   all_exprs <- all_exprs[sapply(all_exprs, function(x) !is.null(x))]
#
#   Reduce(
#     f = function(x, y) expr(!!x %>% !!y),
#     x = all_exprs
#   )
# }
