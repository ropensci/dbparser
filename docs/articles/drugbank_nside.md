# Integrated Pharmacovigilance: Linking DrugBank, OnSIDES, and TWOSIDES

## Introduction

In modern pharmacovigilance, analyzing drugs in isolation is often
insufficient. Patients frequently take multiple medications, leading to
complex **Drug-Drug Interactions (DDIs)**.

The `dbparser` package now provides a unified framework to analyze the
complete safety profile of a drug:

1.  **Mechanism:** Biological targets and pathways from **DrugBank**.
2.  **Phenotype (Single Drug):** Real-world adverse events from
    **OnSIDES**.
3.  **Polypharmacy (Combinations):** Interaction risks from
    **TWOSIDES**.

This vignette demonstrates the complete workflow: loading all three
databases, merging them into a single `dvobject`, and performing a
scientific case study on the interaction between **Leuprolide** and
**Calcitriol**.

## Prerequisites

``` r
library(dbparser)
library(dplyr)
library(tidyr)
library(canvasXpress)
library(data.table)
```

## 1. Loading and Inspecting Data

We will use the curated sample data files included in the `dbparser`
package to demonstrate the workflow.

### 1.1 Load DrugBank (The Hub)

First, we load the mechanistic data.

``` r
# Load sample DrugBank data included in the package
drugbank_path <- system.file("two_drugs.RDS", package = "dbparser")
drugbank_db <- readRDS(drugbank_path)

# Review the drugbank object structure
show_dvobject_metadata(drugbank_db)
#> === BASIC INFO ===
#>      Class Total_DataFrames Top_Level_Lists Object_Size
#> 1 dvobject               55               3      1.5 Mb
#> 
#> === DATABASE METADATA ===
#>  Database     Type Version Export_Date
#>  Original DrugBank     5.1  2025-01-02
#> 
#> === DATA.FRAMES ===
#>                                            Path Rows Cols     Size
#>                       drugs$general_information    2   15     8 Kb
#>                       drugs$drug_classification    2    9   3.8 Kb
#>                                  drugs$synonyms   15    4   3.6 Kb
#>                              drugs$pharmacology    2   12  18.1 Kb
#>                      drugs$international_brands    9    3   2.7 Kb
#>                                  drugs$mixtures    5    3     2 Kb
#>                                 drugs$packagers   35    3   6.2 Kb
#>                             drugs$manufacturers   13    4   2.9 Kb
#>                                    drugs$prices   16    5   4.6 Kb
#>                                drugs$categories   58    3   9.2 Kb
#>                                   drugs$dosages  189    4  17.4 Kb
#>                                 drugs$atc_codes    4   10   4.3 Kb
#>                                   drugs$patents   35    6   8.6 Kb
#>                         drugs$drug_interactions 1645    4 514.7 Kb
#>                                 drugs$sequences    1    3   1.3 Kb
#>                     drugs$calculated_properties   26    4   5.6 Kb
#>                   drugs$experimental_properties    5    4   2.4 Kb
#>                      drugs$external_identifiers   30    3   4.7 Kb
#>                               drugs$pdb_entries   19    2   2.4 Kb
#>                        drugs$affected_organisms    2    2   1.2 Kb
#>                                    drugs$groups    4    2   1.3 Kb
#>                            drugs$external_links    5    3   2.1 Kb
#>                                           salts    2    8   2.8 Kb
#>                                        products  190   19  68.6 Kb
#>                       references$drugs$articles   13    4   6.3 Kb
#>                          references$drugs$links   11    4   6.1 Kb
#>                       references$carriers$books    1    4   1.6 Kb
#>                    references$carriers$articles   15    4   5.4 Kb
#>                        references$enzymes$books   33    4   8.2 Kb
#>                     references$enzymes$articles 1573    4 485.5 Kb
#>                        references$enzymes$links  615    4 164.4 Kb
#>                  references$enzymes$attachments   55    4    20 Kb
#>                     references$targets$articles  101    4  27.4 Kb
#>                        references$targets$links   14    4   7.1 Kb
#>               cett$carriers$general_information    1    6   1.9 Kb
#>                           cett$carriers$actions    2    2   1.2 Kb
#>  cett$carriers$polypeptides$general_information    1   20   6.8 Kb
#>      cett$carriers$polypeptides$external_identy    7    3   2.2 Kb
#>             cett$carriers$polypeptides$synonyms   10    2     2 Kb
#>                cett$carriers$polypeptides$pfams    2    3   1.5 Kb
#>                   cett$carriers$polypeptides$go    2    3   1.5 Kb
#>                cett$enzymes$general_information    2    8   2.6 Kb
#>                            cett$enzymes$actions    7    2   1.5 Kb
#>   cett$enzymes$polypeptides$general_information    2   20    14 Kb
#>       cett$enzymes$polypeptides$external_identy   15    3     3 Kb
#>              cett$enzymes$polypeptides$synonyms   21    2     3 Kb
#>                 cett$enzymes$polypeptides$pfams    2    3   1.4 Kb
#>                    cett$enzymes$polypeptides$go   21    3   3.6 Kb
#>                cett$targets$general_information    4    6   2.6 Kb
#>                            cett$targets$actions   10    2   1.9 Kb
#>   cett$targets$polypeptides$general_information    4   20  14.2 Kb
#>       cett$targets$polypeptides$external_identy   26    3   3.9 Kb
#>              cett$targets$polypeptides$synonyms   15    2   2.4 Kb
#>                 cett$targets$polypeptides$pfams    5    3   2.1 Kb
#>                    cett$targets$polypeptides$go   37    3   5.6 Kb
```

### 1.2 Load OnSIDES (Single-Drug Labels)

Next, we load the adverse event data extracted from FDA labels.

``` r
# Load sample OnSIDES data included in the package
onsides_path <- system.file("onside.RDS", package = "dbparser")
onsides_db   <- readRDS(onsides_path)

# Review the onsides_db object structure
show_dvobject_metadata(onsides_db)
#> === BASIC INFO ===
#>      Class Total_DataFrames Top_Level_Lists Object_Size
#> 1 dvobject                8               0      2.3 Mb
#> 
#> === DATABASE METADATA ===
#>  Database    Type Version Export_Date
#>  Original OnSIDES   3.1.1  April 2025
#> 
#> === DATA.FRAMES ===
#>                                Path  Rows Cols    Size
#>              product_adverse_effect 52745    7  2.2 Mb
#>                     high_confidence     2    2  1.4 Kb
#>                       product_label    95    5 39.7 Kb
#>                   product_to_rxnorm   160    2  5.5 Kb
#>  vocab_rxnorm_ingredient_to_product    17    2  2.7 Kb
#>         vocab_meddra_adverse_effect   347    3 31.6 Kb
#>             vocab_rxnorm_ingredient     3    3  2.1 Kb
#>                vocab_rxnorm_product    17    3  4.9 Kb
```

### 1.3 Load TWOSIDES (Drug Interactions)

Finally, we load the dataset containing adverse events reported when two
drugs are taken simultaneously.

``` r
# Load sample TWOSIDES data included in the package
twosides_path <- system.file("twoside_raw.zip", package = "dbparser")

if(file.exists(twosides_path)) {
  twosides_db <- parseTWOSIDES(twosides_path)
} else {
  # Fallback for demonstration if file is not yet in the build
  # Mocking structure similar to actual TWOSIDES output
  twosides_db <- list(drug_drug_interactions = data.table(
    drug_1_rxnorm_id = "42375", drug_1_concept_name = "Leuprolide",
    drug_2_rxnorm_id = "1894", drug_2_concept_name = "Calcitriol",
    condition_meddra_id = "10002034", condition_concept_name = "Anaemia",
    PRR = "13.33", mean_reporting_frequency = "0.12"
  ))
  class(twosides_db) <- c("TWOSIDESDb", "dvobject", "list")
  attr(twosides_db, "original_db_info") <- list(type="TWOSIDES", version="2.0")
}

# Review structure
show_dvobject_metadata(twosides_db)
#> === BASIC INFO ===
#>      Class Total_DataFrames Top_Level_Lists Object_Size
#> 1 dvobject                1               0    217.6 Kb
#> 
#> === DATABASE METADATA ===
#>  Database       Type Version Export_Date
#>  Original TWOSIDESDB Unknown     Unknown
#> 
#> === DATA.FRAMES ===
#>                    Path Rows Cols     Size
#>  drug_drug_interactions 1706   13 216.6 Kb
```

## 2. Merging the Databases (Chaining)

We integrate the databases in a chain. First, we merge OnSIDES into
DrugBank, and then we merge TWOSIDES into the result. This creates a
single “Hub and Spoke” object centered on DrugBank.

``` r
# Step 1: Merge DrugBank + OnSIDES
db_plus_onsides <- merge_drugbank_onsides(drugbank_db, onsides_db)

# Step 2: Merge Result + TWOSIDES
# The function is chainable and automatically detects the existing data
final_db <- merge_drugbank_twosides(db_plus_onsides, twosides_db)

# Review final object structure
# Note the presence of 'drugbank', 'onsides', 'twosides', and 'integrated_data'
show_dvobject_metadata(final_db)
#> === BASIC INFO ===
#>                                             Class Total_DataFrames
#> 1 DrugBankTWOSIDESDb, DrugBankOnSIDESDb, dvobject               68
#>   Top_Level_Lists Object_Size
#> 1               4      4.3 Mb
#> 
#> === DATABASE METADATA ===
#>         Database       Type Version Export_Date
#>   First Database   DrugBank     5.1  2025-01-02
#>  Second Database TWOSIDESDB Unknown     Unknown
#> 
#> === DATA.FRAMES ===
#>                                                     Path  Rows Cols     Size
#>                       drugbank$drugs$general_information     2   15     8 Kb
#>                       drugbank$drugs$drug_classification     2    9   3.8 Kb
#>                                  drugbank$drugs$synonyms    15    4   3.6 Kb
#>                              drugbank$drugs$pharmacology     2   12  18.1 Kb
#>                      drugbank$drugs$international_brands     9    3   2.7 Kb
#>                                  drugbank$drugs$mixtures     5    3     2 Kb
#>                                 drugbank$drugs$packagers    35    3   6.2 Kb
#>                             drugbank$drugs$manufacturers    13    4   2.9 Kb
#>                                    drugbank$drugs$prices    16    5   4.6 Kb
#>                                drugbank$drugs$categories    58    3   9.2 Kb
#>                                   drugbank$drugs$dosages   189    4  17.4 Kb
#>                                 drugbank$drugs$atc_codes     4   10   4.3 Kb
#>                                   drugbank$drugs$patents    35    6   8.6 Kb
#>                         drugbank$drugs$drug_interactions  1645    4 514.7 Kb
#>                                 drugbank$drugs$sequences     1    3   1.3 Kb
#>                     drugbank$drugs$calculated_properties    26    4   5.6 Kb
#>                   drugbank$drugs$experimental_properties     5    4   2.4 Kb
#>                      drugbank$drugs$external_identifiers    30    3   4.7 Kb
#>                               drugbank$drugs$pdb_entries    19    2   2.4 Kb
#>                        drugbank$drugs$affected_organisms     2    2   1.2 Kb
#>                                    drugbank$drugs$groups     4    2   1.3 Kb
#>                            drugbank$drugs$external_links     5    3   2.1 Kb
#>                                           drugbank$salts     2    8   2.8 Kb
#>                                        drugbank$products   190   19  68.6 Kb
#>                       drugbank$references$drugs$articles    13    4   6.3 Kb
#>                          drugbank$references$drugs$links    11    4   6.1 Kb
#>                       drugbank$references$carriers$books     1    4   1.6 Kb
#>                    drugbank$references$carriers$articles    15    4   5.4 Kb
#>                        drugbank$references$enzymes$books    33    4   8.2 Kb
#>                     drugbank$references$enzymes$articles  1573    4 485.5 Kb
#>                        drugbank$references$enzymes$links   615    4 164.4 Kb
#>                  drugbank$references$enzymes$attachments    55    4    20 Kb
#>                     drugbank$references$targets$articles   101    4  27.4 Kb
#>                        drugbank$references$targets$links    14    4   7.1 Kb
#>               drugbank$cett$carriers$general_information     1    6   1.9 Kb
#>                           drugbank$cett$carriers$actions     2    2   1.2 Kb
#>  drugbank$cett$carriers$polypeptides$general_information     1   20   6.8 Kb
#>      drugbank$cett$carriers$polypeptides$external_identy     7    3   2.2 Kb
#>             drugbank$cett$carriers$polypeptides$synonyms    10    2     2 Kb
#>                drugbank$cett$carriers$polypeptides$pfams     2    3   1.5 Kb
#>                   drugbank$cett$carriers$polypeptides$go     2    3   1.5 Kb
#>                drugbank$cett$enzymes$general_information     2    8   2.6 Kb
#>                            drugbank$cett$enzymes$actions     7    2   1.5 Kb
#>   drugbank$cett$enzymes$polypeptides$general_information     2   20    14 Kb
#>       drugbank$cett$enzymes$polypeptides$external_identy    15    3     3 Kb
#>              drugbank$cett$enzymes$polypeptides$synonyms    21    2     3 Kb
#>                 drugbank$cett$enzymes$polypeptides$pfams     2    3   1.4 Kb
#>                    drugbank$cett$enzymes$polypeptides$go    21    3   3.6 Kb
#>                drugbank$cett$targets$general_information     4    6   2.6 Kb
#>                            drugbank$cett$targets$actions    10    2   1.9 Kb
#>   drugbank$cett$targets$polypeptides$general_information     4   20  14.2 Kb
#>       drugbank$cett$targets$polypeptides$external_identy    26    3   3.9 Kb
#>              drugbank$cett$targets$polypeptides$synonyms    15    2   2.4 Kb
#>                 drugbank$cett$targets$polypeptides$pfams     5    3   2.1 Kb
#>                    drugbank$cett$targets$polypeptides$go    37    3   5.6 Kb
#>                           onsides$product_adverse_effect 52745    7   2.2 Mb
#>                                  onsides$high_confidence     2    2   1.4 Kb
#>                                    onsides$product_label    95    5  39.7 Kb
#>                                onsides$product_to_rxnorm   160    2   5.5 Kb
#>               onsides$vocab_rxnorm_ingredient_to_product    17    2   2.7 Kb
#>                      onsides$vocab_meddra_adverse_effect   347    3  31.6 Kb
#>                          onsides$vocab_rxnorm_ingredient     3    3   2.1 Kb
#>                             onsides$vocab_rxnorm_product    17    3   4.9 Kb
#>         integrated_data$vocab_rxnorm_ingredient_enriched     3    4   2.3 Kb
#>                 integrated_data$high_confidence_enriched     2    3   1.6 Kb
#>                   integrated_data$DrugBank_RxCUI_Mapping     2    2   1.2 Kb
#>                   integrated_data$drug_drug_interactions  1706   17 300.9 Kb
#>                          twosides$drug_drug_interactions  1706   13 216.6 Kb
```

## 3. Single Drug Analysis: Leuprolide (DB00007)

**Scientific Context:** Leuprolide is a GnRH analog. We first establish
its baseline side effect profile from OnSIDES.

``` r
target_drug_id <- "DB00007" # Leuprolide

# 1. Get the RxNorm ID from the bridge table
mapping_df <- final_db$integrated_data$DrugBank_RxCUI_Mapping %>%
  filter(drugbank_id == target_drug_id)

target_rxcui <- mapping_df$rxcui[1]
print(paste0("DrugBank ID: ", target_drug_id, " maps to RxNorm CUI: ", target_rxcui))
#> [1] "DrugBank ID: DB00007 maps to RxNorm CUI: 42375"
```

### 3.1 Extract Baseline Adverse Events (OnSIDES)

``` r
# 1. Find product labels linked to this ingredient
product_rxcuis <- final_db$onsides$vocab_rxnorm_ingredient_to_product %>%
  filter(ingredient_id == target_rxcui) %>%
  pull(product_id)

target_label_ids <- final_db$onsides$product_to_rxnorm %>%
  filter(rxnorm_product_id %in% product_rxcuis) %>%
  pull(label_id)

# 2. Extract and summarize events
ae_summary <- final_db$onsides$product_adverse_effect %>%
  filter(product_label_id %in% target_label_ids) %>%
  group_by(effect_meddra_id) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  left_join(final_db$onsides$vocab_meddra_adverse_effect, 
            by = c("effect_meddra_id" = "meddra_id")) %>%
  select(effect_meddra_id, meddra_name, Count)

# Fill NAs for demo purposes (if vocab is incomplete in sample)
ae_summary <- ae_summary %>%
  mutate(meddra_name = case_when(
    effect_meddra_id == 10033336 ~ "Pain",
    effect_meddra_id == 10039769 ~ "Rash", 
    effect_meddra_id == 10052995 ~ "Hot flush",
    effect_meddra_id == 10009226 ~ "Constipation",
    effect_meddra_id == 10006068 ~ "Bone pain",
    TRUE ~ meddra_name
  )) %>%
  rename(Adverse_Event = meddra_name)

print(ae_summary)
#> # A tibble: 10 × 3
#>    effect_meddra_id Adverse_Event Count
#>               <int> <chr>         <int>
#>  1         10033336 Pain          10659
#>  2         10029197 Nervous         711
#>  3         10039769 Rash            526
#>  4         10017735 Gas             504
#>  5         10010904 Convulsion      424
#>  6         10019211 Headache        351
#>  7         10052995 Hot flush       350
#>  8         10010914 Convulsions     349
#>  9         10009226 Constipation    262
#> 10         10006068 Bone pain       247
```

## 4. Single Drug Analysis: Calcitriol (DB00136)

We repeat the process for Calcitriol to establish its baseline.

``` r
target_drug_id_2 <- "DB00136"

# Get RxCUI
mapping_df_2 <- final_db$integrated_data$DrugBank_RxCUI_Mapping %>%
  filter(drugbank_id == target_drug_id_2)
target_rxcui_2 <- mapping_df_2$rxcui[1]

# Get Products & Labels
product_rxcuis_2 <- final_db$onsides$vocab_rxnorm_ingredient_to_product %>%
  filter(ingredient_id == target_rxcui_2) %>%
  pull(product_id)
target_label_ids_2 <- final_db$onsides$product_to_rxnorm %>%
  filter(rxnorm_product_id %in% product_rxcuis_2) %>%
  pull(label_id)

# Summarize Events
ae_summary_2 <- final_db$onsides$product_adverse_effect %>%
  filter(product_label_id %in% target_label_ids_2) %>%
  group_by(effect_meddra_id) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  left_join(final_db$onsides$vocab_meddra_adverse_effect, 
            by = c("effect_meddra_id" = "meddra_id")) %>%
  select(effect_meddra_id, meddra_name, Count) %>%
  mutate(meddra_name = ifelse(is.na(meddra_name), paste0("MedDRA_", effect_meddra_id), meddra_name)) %>%
  rename(Adverse_Event = meddra_name)
```

## 5. Polypharmacy Analysis: Drug-Drug Interactions

**New Insight:** What happens when **Leuprolide** and **Calcitriol** are
taken together?

We query the `integrated_data$drug_drug_interactions` table, which was
populated by `merge_drugbank_twosides`. This table allows us to look up
interactions by DrugBank ID, using the enriched columns provided by the
merge.

``` r
# Look for interactions where Drug 1 is Leuprolide AND Drug 2 is Calcitriol (or vice versa)
interaction_data <- final_db$integrated_data$drug_drug_interactions %>%
  filter(
    (drugbank_id_1 == target_drug_id & drugbank_id_2 == target_drug_id_2) |
    (drugbank_id_1 == target_drug_id_2 & drugbank_id_2 == target_drug_id)
  ) %>%
  arrange(desc(as.numeric(PRR))) %>%
  select(drug_name_1, drug_name_2, condition_concept_name, PRR, mean_reporting_frequency)

print(interaction_data)
#>    drug_name_1 drug_name_2   condition_concept_name   PRR
#>         <char>      <char>                   <char> <num>
#> 1:  Leuprolide  Calcitriol Platelet count decreased    15
#> 2:  Leuprolide  Calcitriol Disturbance in attention     5
#> 3:  Leuprolide  Calcitriol              Haemorrhage     5
#>    mean_reporting_frequency
#>                       <num>
#> 1:                0.0909091
#> 2:                0.0303030
#> 3:                0.0303030
```

### 5.1 Visualization of Interaction Risks

We can visualize the specific adverse events associated with this
combination, ranked by their **Proportional Reporting Ratio (PRR)** (a
measure of signal strength).

``` r
if(nrow(interaction_data) > 0) {
  # Prepare data for canvasXpress
  cx_int_data <- data.frame(PRR = as.numeric(interaction_data$PRR))
  rownames(cx_int_data) <- interaction_data$condition_concept_name
  
  canvasXpress(
    data = t(cx_int_data),
    graphType = "Bar",
    title = "Polypharmacy Risks: Leuprolide + Calcitriol",
    subtitle = "Data from TWOSIDES",
    xAxisTitle = "Signal Strength (PRR)",
    yAxisTitle = "Adverse Event",
    showLegend = FALSE
  )
} else {
  print("No interaction data found for this specific pair in the sample dataset.")
}
```

## 6. Comparative Overview

Finally, we can view the single-drug profiles alongside the interaction
profile to see if new risks emerge.

``` r
# 1. Prepare Single Drug Data
s1 <- ae_summary %>% 
  mutate(Type = "Leuprolide (Alone)") %>% 
  select(Adverse_Event, Value=Count, Type)

s2 <- ae_summary_2 %>% 
  mutate(Type = "Calcitriol (Alone)") %>% 
  select(Adverse_Event, Value=Count, Type)

# 2. Prepare Interaction Data (Show Top 5)
# Note: Comparing Frequency (Count) vs PRR (Ratio) directly is tricky, 
# so we visualize them to show relative importance within their own context.
s3 <- interaction_data %>% 
  head(5) %>%
  mutate(Type = "Interaction (PRR)", Value = as.numeric(PRR)) %>% 
  select(Adverse_Event = condition_concept_name, Value, Type)

# 3. Combine
combined_data <- bind_rows(s1, s2, s3) %>%
  group_by(Type) %>%
  slice_max(Value, n = 5) %>%
  ungroup()

# 4. Reshape for Matrix
comparison_matrix <- combined_data %>%
  pivot_wider(names_from = Type, values_from = Value, values_fill = 0)

cx_compare <- as.data.frame(comparison_matrix[, -1])
rownames(cx_compare) <- comparison_matrix$Adverse_Event

# 5. Plot
canvasXpress(
  data = cx_compare,
  graphType = "Bar",
  title = "Safety Profile Comparison",
  subtitle = "Single Drug Frequencies vs. Interaction PRR",
  xAxisTitle = "Value",
  yAxisTitle = "Adverse Event",
  legendPosition = "right"
)
```

## 7. Conclusion

This analysis highlights the power of the `dbparser` integration
ecosystem:

1.  **DrugBank** provided the identity and mechanism of Leuprolide and
    Calcitriol.
2.  **OnSIDES** quantified their individual side effects (e.g., Pain,
    Hot Flushes).
3.  **TWOSIDES** revealed specific risks when the drugs are combined
    (e.g., Anaemia).

By chaining `merge_drugbank_onsides` and `merge_drugbank_twosides`,
researchers can perform comprehensive safety assessments across
mechanistic, real-world, and polypharmacy domains in just a few lines of
R code.
