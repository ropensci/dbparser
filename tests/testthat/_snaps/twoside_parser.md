# parse two side DB - default params

    Code
      expect_error(parseTWOSIDES(twosides_file_path = "data_path"))

---

    Code
      show_dvobject_metadata(twoside)
    Output
      === BASIC INFO ===
           Class Total_DataFrames Top_Level_Lists Object_Size
      1 dvobject                1               0    217.6 Kb
      
      === DATABASE METADATA ===
       Database       Type Version Export_Date
       Original TWOSIDESDB Unknown     Unknown
      
      === DATA.FRAMES ===
                         Path Rows Cols     Size
       drug_drug_interactions 1706   13 216.6 Kb

