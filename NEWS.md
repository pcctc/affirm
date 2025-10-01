# affirm 0.3.0

## Breaking Changes

* Renamed `affirm_no_dupes()` to `affirm_dupe_free()` for consistency with positive assertion naming (like `affirm_true()` rather than `affirm_not_false()`). Users should update all references to use the new function name.

## New Features / Improvements

* Updated `affirm_dupe_free()` to add `record_id`, `flag_duplicate`, and `duplicate_of` columns to the output, providing better visibility into duplicate detection logic.

# affirm 0.2.1

* Added the `previous_file` argument to `affirm_report_excel()` to enable newly created Excel affirm reports to be updated with data from a previous report. This allows `assigned_to`, `status`, and `comment` columns to be preserved when regenerating reports.

* Updated `affirm_report_excel()` and applicable utility functions to now export affirmations alphabetically in the summary sheet and tabs of Excel report.

* Updated applicable utility functions to introduce small formatting changes in the Excel report output. This includes removing the ‘Notes’ column from individual sheets and replacing it with the ‘Status’ and ‘Comment’ columns. The formatting of variable labels is changed to a bold black font in the Excel report output.

* Updated `affirm_report_excel()` to allow for affirmations with an `error_rate` of 0 to print in the summary tab of the Excel report output. The corresponding affirmation tab is also produced and displays the standard header with zero rows of data.

* Exported utility functions for `affirm_report_excel()`.

* Updated `affirm_report_excel()` to depend on `openxlsx2`; now contains front
summary sheet and specific formatting for individual affirmation sheets.

* Removed variable labels arguments and corresponding helper functions.

* Allowed for glue syntax in excel report sheet names.

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
