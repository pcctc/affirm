# affirm_report() works

    Code
      mtcars2 <- dplyr::as_tibble(mtcars)
      attr(mtcars2$mpg, "label") <- "MPG LABEL"
      affirm_init(replace = TRUE)
    Message
      v We're ready to make data affirmations...
    Code
      affirm_true(mtcars2, label = "leave it all, no actions", condition = mpg > 33)
    Message
      * leave it all, no actions
        31 issues identified.
    Output
      # A tibble: 32 x 11
           mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
       2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
       3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
       4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
       5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
       6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
       7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
       8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
       9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
      10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
      # i 22 more rows
    Code
      affirm_report_raw_data(variable_labels = TRUE)$data
    Output
      [[1]]
      # A tibble: 33 x 1
         mpg      
         <chr>    
       1 MPG LABEL
       2 mpg      
       3 21       
       4 21       
       5 22.8     
       6 21.4     
       7 18.7     
       8 18.1     
       9 14.3     
      10 24.4     
      # i 23 more rows
      

