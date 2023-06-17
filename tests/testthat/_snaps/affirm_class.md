# affirm_class() works

    Code
      affirm_init(replace = TRUE)
    Message
      v We're ready to make data affirmations...
    Code
      affirm_class(dplyr::as_tibble(iris), label = "all cols are numeric (but Species really isn't)",
      columns = everything(), class = "numeric")
    Message
      * all cols are numeric (but Species really isn't)
        1 issue identified.
    Output
      # A tibble: 150 x 5
         Sepal.Length Sepal.Width Petal.Length Petal.Width Species
                <dbl>       <dbl>        <dbl>       <dbl> <fct>  
       1          5.1         3.5          1.4         0.2 setosa 
       2          4.9         3            1.4         0.2 setosa 
       3          4.7         3.2          1.3         0.2 setosa 
       4          4.6         3.1          1.5         0.2 setosa 
       5          5           3.6          1.4         0.2 setosa 
       6          5.4         3.9          1.7         0.4 setosa 
       7          4.6         3.4          1.4         0.3 setosa 
       8          5           3.4          1.5         0.2 setosa 
       9          4.4         2.9          1.4         0.2 setosa 
      10          4.9         3.1          1.5         0.1 setosa 
      # i 140 more rows
    Code
      affirm_report_raw_data()$data
    Output
      [[1]]
      # A tibble: 150 x 1
         Species
         <fct>  
       1 setosa 
       2 setosa 
       3 setosa 
       4 setosa 
       5 setosa 
       6 setosa 
       7 setosa 
       8 setosa 
       9 setosa 
      10 setosa 
      # i 140 more rows
      

