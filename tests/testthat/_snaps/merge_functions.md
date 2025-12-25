# merge drugbank and onside

    Code
      show_dvobject_metadata(all_dbs)
    Output
      === BASIC INFO ===
                              Class Total_DataFrames Top_Level_Lists Object_Size
      1 DrugBankOnSIDESDb, dvobject               66               3      3.8 Mb
      
      === DATABASE METADATA ===
              Database     Type Version Export_Date
        First Database DrugBank     5.1  2025-01-02
       Second Database  OnSIDES   3.1.1  April 2025
      
      === DATA.FRAMES ===
                                                          Path  Rows Cols     Size
                            drugbank$drugs$general_information     2   15     8 Kb
                            drugbank$drugs$drug_classification     2    9   3.8 Kb
                                       drugbank$drugs$synonyms    15    4   3.6 Kb
                                   drugbank$drugs$pharmacology     2   12  18.1 Kb
                           drugbank$drugs$international_brands     9    3   2.7 Kb
                                       drugbank$drugs$mixtures     5    3     2 Kb
                                      drugbank$drugs$packagers    35    3   6.2 Kb
                                  drugbank$drugs$manufacturers    13    4   2.9 Kb
                                         drugbank$drugs$prices    16    5   4.6 Kb
                                     drugbank$drugs$categories    58    3   9.2 Kb
                                        drugbank$drugs$dosages   189    4  17.4 Kb
                                      drugbank$drugs$atc_codes     4   10   4.3 Kb
                                        drugbank$drugs$patents    35    6   8.6 Kb
                              drugbank$drugs$drug_interactions  1645    4 514.7 Kb
                                      drugbank$drugs$sequences     1    3   1.3 Kb
                          drugbank$drugs$calculated_properties    26    4   5.6 Kb
                        drugbank$drugs$experimental_properties     5    4   2.4 Kb
                           drugbank$drugs$external_identifiers    30    3   4.7 Kb
                                    drugbank$drugs$pdb_entries    19    2   2.4 Kb
                             drugbank$drugs$affected_organisms     2    2   1.2 Kb
                                         drugbank$drugs$groups     4    2   1.3 Kb
                                 drugbank$drugs$external_links     5    3   2.1 Kb
                                                drugbank$salts     2    8   2.8 Kb
                                             drugbank$products   190   19  68.6 Kb
                            drugbank$references$drugs$articles    13    4   6.3 Kb
                               drugbank$references$drugs$links    11    4   6.1 Kb
                            drugbank$references$carriers$books     1    4   1.6 Kb
                         drugbank$references$carriers$articles    15    4   5.4 Kb
                             drugbank$references$enzymes$books    33    4   8.2 Kb
                          drugbank$references$enzymes$articles  1573    4 485.5 Kb
                             drugbank$references$enzymes$links   615    4 164.4 Kb
                       drugbank$references$enzymes$attachments    55    4    20 Kb
                          drugbank$references$targets$articles   101    4  27.4 Kb
                             drugbank$references$targets$links    14    4   7.1 Kb
                    drugbank$cett$carriers$general_information     1    6   1.9 Kb
                                drugbank$cett$carriers$actions     2    2   1.2 Kb
       drugbank$cett$carriers$polypeptides$general_information     1   20   6.8 Kb
           drugbank$cett$carriers$polypeptides$external_identy     7    3   2.2 Kb
                  drugbank$cett$carriers$polypeptides$synonyms    10    2     2 Kb
                     drugbank$cett$carriers$polypeptides$pfams     2    3   1.5 Kb
                        drugbank$cett$carriers$polypeptides$go     2    3   1.5 Kb
                     drugbank$cett$enzymes$general_information     2    8   2.6 Kb
                                 drugbank$cett$enzymes$actions     7    2   1.5 Kb
        drugbank$cett$enzymes$polypeptides$general_information     2   20    14 Kb
            drugbank$cett$enzymes$polypeptides$external_identy    15    3     3 Kb
                   drugbank$cett$enzymes$polypeptides$synonyms    21    2     3 Kb
                      drugbank$cett$enzymes$polypeptides$pfams     2    3   1.4 Kb
                         drugbank$cett$enzymes$polypeptides$go    21    3   3.6 Kb
                     drugbank$cett$targets$general_information     4    6   2.6 Kb
                                 drugbank$cett$targets$actions    10    2   1.9 Kb
        drugbank$cett$targets$polypeptides$general_information     4   20  14.2 Kb
            drugbank$cett$targets$polypeptides$external_identy    26    3   3.9 Kb
                   drugbank$cett$targets$polypeptides$synonyms    15    2   2.4 Kb
                      drugbank$cett$targets$polypeptides$pfams     5    3   2.1 Kb
                         drugbank$cett$targets$polypeptides$go    37    3   5.6 Kb
                                onsides$product_adverse_effect 52745    7   2.2 Mb
                                       onsides$high_confidence     2    2   1.4 Kb
                                         onsides$product_label    95    5  39.7 Kb
                                     onsides$product_to_rxnorm   160    2   5.5 Kb
                    onsides$vocab_rxnorm_ingredient_to_product    17    2   2.7 Kb
                           onsides$vocab_meddra_adverse_effect   347    3  31.6 Kb
                               onsides$vocab_rxnorm_ingredient     3    3   2.1 Kb
                                  onsides$vocab_rxnorm_product    17    3   4.9 Kb
              integrated_data$vocab_rxnorm_ingredient_enriched     3    4   2.3 Kb
                      integrated_data$high_confidence_enriched     2    3   1.6 Kb
                        integrated_data$DrugBank_RxCUI_Mapping     2    2   1.2 Kb

