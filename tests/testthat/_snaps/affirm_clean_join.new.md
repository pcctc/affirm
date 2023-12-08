# affirm_clean_join() works

    Code
      affirm_init(replace = TRUE)
    Message
      v We're ready to make data affirmations...
    Code
      df <- dplyr::mutate(dplyr::tibble(lgl = c(NA, TRUE, NA, FALSE, NA)), id = dplyr::row_number())
      affirm_clean_join(dplyr::full_join(df, df, by = "id"), label = "Checking for clean merge")
    Message
      * Checking for clean merge
        1 issue identified.
    Output
      # A tibble: 5 x 3
        lgl.x    id lgl.y
        <lgl> <int> <lgl>
      1 NA        1 NA   
      2 TRUE      2 TRUE 
      3 NA        3 NA   
      4 FALSE     4 FALSE
      5 NA        5 NA   
    Code
      affirm_report_raw_data()
    Output
      # A tibble: 1 x 10
           id label  priority data_frames columns error_n total_n error_rate error_pct
        <int> <chr>     <int> <chr>       <chr>     <int>   <int>      <dbl> <chr>    
      1    NA Check~       NA <NA>        All Co~       5       5          1 100%     
      # i 1 more variable: data <list>
    Code
      affirm_report_raw_data()$data
    Output
      [[1]]
      # A tibble: 5 x 2
        lgl.x lgl.y
        <lgl> <lgl>
      1 NA    NA   
      2 TRUE  TRUE 
      3 NA    NA   
      4 FALSE FALSE
      5 NA    NA   
      

