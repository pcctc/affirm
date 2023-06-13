# affirm (development version)

* Changed function name `affirm_export_excel()` to `affirm_report_excel()`.

* Added a `variable_labels=` argument to `affirm_report_raw_data()`, `affirm_report_gt()` and `affirm_report_excel()`, which adds a row on top with variable labels to the output. (#5)

* Added `affirm_class()` function to check column classes.

* Added `affirm_na()` and `affirm_not_na()` functions to check for NA values.

* Added `affirm_no_dupes()` function to check for duplicate rows.

* Added `affirm_range()` function to check ranges of numeric, date, and other types of columns.

* Added `affirm_false()` function to check conditions expected to be false.

* Updated package internals to use quosures instead of expressions for proper environment handling. (#5)

# affirm 0.1.0

* First release.
