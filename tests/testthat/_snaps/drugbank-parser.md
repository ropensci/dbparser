# parse DrugBank DB - default params

    Code
      dvobj$result
    Output
      $drugs
      $drugs$general_information
      # A tibble: 1 × 15
        primary_key other_…¹ type  created updated name  descr…² cas_n…³ unii  avera…⁴
        <chr>       <chr>    <chr> <chr>   <chr>   <chr> <chr>   <chr>   <chr> <chr>  
      1 DB00001     BTD0002… biot… 2005-0… 2018-0… Lepi… Lepiru… 138068… Y43G… <NA>   
      # … with 5 more variables: monoisotopic_mass <chr>, state <chr>,
      #   synthesis_reference <chr>, fda_label <chr>, msds <chr>, and abbreviated
      #   variable names ¹​other_keys, ²​description, ³​cas_number, ⁴​average_mass
      
      
      attr(,"class")
      [1] "dvobject"
      attr(,"original_db_info")
      attr(,"original_db_info")$db_type
      [1] "DrugBank"
      
      attr(,"original_db_info")$db_version
      [1] "5.1"
      
      attr(,"original_db_info")$db_exported_date
      [1] "2018-07-03"
      

