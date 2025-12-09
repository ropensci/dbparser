# parse onside DB - default params

    Code
      expect_error(parseOnSIDES(dataDir = "data_path"))

---

    Code
      show_dvobject_metadata(onside)
    Output
      === BASIC INFO ===
           Class Total_DataFrames Top_Level_Lists Object_Size
      1 dvobject                8               0    845.6 Kb
      
      === DATABASE METADATA ===
       Database    Type Version Export_Date
       Original OnSIDES Unknown     Unknown
      
      === DATA.FRAMES ===
                                     Path  Rows Cols    Size
                            product_label    29    5   13 Kb
                   product_adverse_effect 18742    7  808 Kb
                        product_to_rxnorm    44    2  2.2 Kb
              vocab_meddra_adverse_effect   139    3 13.9 Kb
                  vocab_rxnorm_ingredient     1    3  1.7 Kb
                     vocab_rxnorm_product     3    3  2.2 Kb
       vocab_rxnorm_ingredient_to_product     3    2  1.6 Kb
                          high_confidence     2    2  1.4 Kb

