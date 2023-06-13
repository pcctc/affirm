# affirm_na() works

    Code
      affirm_init(replace = TRUE)
    Message
      v We're ready to make data affirmations...
    Code
      affirm_na(data = dplyr::tibble(x = c(NA, TRUE, NA, FALSE, NA)), label = "All NA values",
      column = x)
    Message
      * All NA values
        2 issues identified.
    Output
      # A tibble: 5 x 1
        x    
        <lgl>
      1 NA   
      2 TRUE 
      3 NA   
      4 FALSE
      5 NA   
    Code
      affirm_not_na(data = dplyr::tibble(x = c(NA, TRUE, NA, FALSE, NA)), label = "No NA values",
      column = x)
    Message
      * No NA values
        3 issues identified.
    Output
      # A tibble: 5 x 1
        x    
        <lgl>
      1 NA   
      2 TRUE 
      3 NA   
      4 FALSE
      5 NA   
    Code
      affirm_report_raw_data()$data
    Output
      [[1]]
      # A tibble: 3 x 1
        x    
        <lgl>
      1 NA   
      2 NA   
      3 NA   
      
      [[2]]
      # A tibble: 2 x 1
        x    
        <lgl>
      1 TRUE 
      2 FALSE
      

