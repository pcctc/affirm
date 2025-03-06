# affirm_report() works

    Code
      affirm_init(replace = TRUE)
    Message
      v We're ready to make data affirmations...
    Code
      affirm_true(mtcars, label = "leave it all, no actions", condition = mpg > 33,
      data_frames = "mtcars", id = 1)
    Message
      * leave it all, no actions
        31 issues identified.
    Output
                           mpg cyl  disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
      Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
      Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
      Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
      Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
      Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
      Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
      Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
      Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
      Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
      Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
      Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
      Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
      Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
      Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
      Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
      Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
      Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
      Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
      Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
      Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
      Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
      AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
      Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
      Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
      Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
      Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
      Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
      Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
      Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
      Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
      Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    Code
      affirm_true(mtcars, label = "No. cylinders must be 4 or 6", condition = cyl %in%
        c(4, 6), data_frames = "mtcars", id = 2)
    Message
      * No. cylinders must be 4 or 6
        14 issues identified.
    Output
                           mpg cyl  disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
      Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
      Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
      Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
      Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
      Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
      Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
      Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
      Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
      Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
      Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
      Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
      Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
      Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
      Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
      Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
      Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
      Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
      Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
      Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
      Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
      Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
      AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
      Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
      Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
      Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
      Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
      Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
      Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
      Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
      Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
      Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
    Code
      affirm_report_raw_data()$data
    Output
      [[1]]
                           mpg
      Mazda RX4           21.0
      Mazda RX4 Wag       21.0
      Datsun 710          22.8
      Hornet 4 Drive      21.4
      Hornet Sportabout   18.7
      Valiant             18.1
      Duster 360          14.3
      Merc 240D           24.4
      Merc 230            22.8
      Merc 280            19.2
      Merc 280C           17.8
      Merc 450SE          16.4
      Merc 450SL          17.3
      Merc 450SLC         15.2
      Cadillac Fleetwood  10.4
      Lincoln Continental 10.4
      Chrysler Imperial   14.7
      Fiat 128            32.4
      Honda Civic         30.4
      Toyota Corona       21.5
      Dodge Challenger    15.5
      AMC Javelin         15.2
      Camaro Z28          13.3
      Pontiac Firebird    19.2
      Fiat X1-9           27.3
      Porsche 914-2       26.0
      Lotus Europa        30.4
      Ford Pantera L      15.8
      Ferrari Dino        19.7
      Maserati Bora       15.0
      Volvo 142E          21.4
      
      [[2]]
                          cyl
      Hornet Sportabout     8
      Duster 360            8
      Merc 450SE            8
      Merc 450SL            8
      Merc 450SLC           8
      Cadillac Fleetwood    8
      Lincoln Continental   8
      Chrysler Imperial     8
      Dodge Challenger      8
      AMC Javelin           8
      Camaro Z28            8
      Pontiac Firebird      8
      Ford Pantera L        8
      Maserati Bora         8
      

# Test that cli output is as expected if `previous_file` class throws an error. [plain]

    Code
      affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}",
        previous_file = data.frame(path = "a valid path.xlsx"))
    Condition
      Error in `affirm_report_excel()`:
      ! `previous_file` should be of class <character>
      x You've supplied a `previous_file` input of class <data.frame>

# Test that cli output is as expected if `previous_file` class throws an error. [ansi]

    Code
      affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}",
        previous_file = data.frame(path = "a valid path.xlsx"))
    Condition
      [1m[33mError[39m in `affirm_report_excel()`:[22m
      [1m[22m[33m![39m [33m`previous_file`[39m should be of class [34m<character>[39m
      [31mx[39m You've supplied a [33m`previous_file`[39m input of class [31m<data.frame>[39m

# Test that duplicate data throws an error when updating a previous Affirm report. [plain]

    Code
      affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}",
        previous_file = tempxlsx)
    Condition
      Error in `affirm_report_excel()`:
      ! Duplicate rows detected in affirmation mtcars1 at rows 1 and 19, 2 and 20, 3 and 21, 4 and 22, 5 and 23, 6 and 24, 7 and 25, 8 and 26, 9 and 27, 10 and 28, 11 and 29, 12 and 30, 13 and 31, 14 and 32, 15 and 33, 16 and 34, 17 and 35, and 18 and 36.
      
      i Please review and remove duplicate data before updated a previous Affirm Excel Report.

# Test that duplicate data throws an error when updating a previous Affirm report. [ansi]

    Code
      affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}",
        previous_file = tempxlsx)
    Condition
      [1m[33mError[39m in `affirm_report_excel()`:[22m
      [1m[22m[33m![39m Duplicate rows detected in affirmation [1m[3m[33mmtcars1[39m[23m[22m at rows [31m1 and 19[39m, [31m2 and 20[39m, [31m3 and 21[39m, [31m4 and 22[39m, [31m5 and 23[39m, [31m6 and 24[39m, [31m7 and 25[39m, [31m8 and 26[39m, [31m9 and 27[39m, [31m10 and 28[39m, [31m11 and 29[39m, [31m12 and 30[39m, [31m13 and 31[39m, [31m14 and 32[39m, [31m15 and 33[39m, [31m16 and 34[39m, [31m17 and 35[39m, and [31m18 and 36[39m.
      
      [36mi[39m Please review and remove duplicate data before updated a previous Affirm Excel Report.

# Test that when columns are removed from old to new affirmations, that the correct error is thrown

    Code
      affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}",
        previous_file = tempxlsx)
    Message
      x The current affirm report could not be updated due to missing columns in the following current affirmations:
      
      1. Affirmation: `mtcars1`
        * `am`, `gear`, and `carb`
      2. Affirmation: `mtcars2`
        * `am`, `gear`, and `carb`
      
      i Please add the missing columns to the current affirm session before attempting to update the current report.
      
    Condition
      Error in `affirm_report_excel()`:
      i affirm Excel Report was not updated.

# Test that when columns are mismatched from old to new affirmations, that the correct error is thrown

    Code
      affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}",
        previous_file = tempxlsx)
    Message
      x The current affirm report could not be updated due to missing columns in the following current affirmations:
      
      1. Affirmation: `mtcars1`
        * `hp`, `drat`, and `wt`
      2. Affirmation: `mtcars2`
        * `hp`, `drat`, and `wt`
      
      i Please add the missing columns to the current affirm session before attempting to update the current report.
      
    Condition
      Error in `affirm_report_excel()`:
      i affirm Excel Report was not updated.

# Test that when sheets are added to the beginning of a previous report, that the correct error is given.

    Code
      affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}",
        previous_file = tempxlsx)
    Condition
      Error in `affirm_report_excel()`:
      x Extra sheets were found before the 'Summary' sheet in the previous Excel workbook we attempted to use for updating.
      i Please remove any additional sheets and ensure the 'Summary' sheet is the first sheet in the workbook, followed by the affirmation sheets.

