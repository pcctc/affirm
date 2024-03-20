# affirm (development version)

* Update `affirm_excel_report()` to allow for affirmations with an `error_rate` of 0 to print in the summary tab of the Excel report output.

* Export utility functions for `affirm_excel_report()`.

* Update `affirm_excel_report()` to depend on `openxlsx2`; now contains front
summary sheet and specific formatting for individual affirmation sheets.

* Remove variable labels arguments and corresponding helper functions.

* Allow for glue syntax in excel report sheet names.

* When no errors are found, return a data frame of zero rows instead of `NULL`.

* No longer re-exporting `tibble()`, `as_tibble()`, `filter()`, `select()`, and `mutate()` from {dplyr}.

* Added `affirm_clean_join()` function to check column names don't end in `".x"` or `".y"`.

# affirm 0.2.0

* Changed function name `affirm_export_excel()` to `affirm_report_excel()`.

* Added a `variable_labels=` argument to `affirm_report_raw_data()`, `affirm_report_gt()` and `affirm_report_excel()`, which adds a row on top with variable labels to the output. (#4)

* Added `affirm_class()` function to check column classes.

* Added `affirm_na()` and `affirm_not_na()` functions to check for NA values.

* Added `affirm_no_dupes()` function to check for duplicate rows.

* Added `affirm_range()` function to check ranges of numeric, date, and other types of columns.

* Added `affirm_false()` function to check conditions expected to be false.

* Updated package internals to use quosures instead of expressions for proper environment handling. (#5)

# affirm 0.1.0

* First release.
