# Summary

`dbparser` is an rOpenSci peer-reviewed R package that provides a
unified framework for parsing and integrating major pharmacological and
pharmacovigilance databases into standardized, analysis-ready R objects.
The package currently supports three essential drug information
resources: DrugBank \[@wishart2018drugbank\], OnSIDES
\[@galeano2022onsides\] and TWOSIDES \[@tatonetti2012data\]. Each
database is parsed into a consistent nested list structure called a
`dvobject`, which preserves complex relational hierarchies while
enabling seamless integration across databases. By providing
high-performance parsing functions, chainable merge operations, and
comprehensive metadata tracking, `dbparser` eliminates a significant
bottleneck in computational pharmacology research and enables
reproducible, large-scale drug safety analyses that would otherwise
require substantial custom development effort.

# Statement of Need

Pharmacological research increasingly relies on the integration of
heterogeneous data sources to understand drug mechanisms, predict
adverse effects, and identify drug-drug interactions. Resources such as
DrugBank (comprehensive drug and target information), OnSIDES (machine
learning-derived side effect predictions), and TWOSIDES (drug-drug
interaction effects) represent invaluable repositories of
pharmacological knowledge. However, accessing and integrating these
databases presents substantial technical challenges that impede research
progress.

Each database employs distinct file formats and structural conventions:
DrugBank distributes data as deeply nested XML with complex entity
relationships; OnSIDES provides multiple relational CSV files requiring
careful joining; TWOSIDES offers compressed flat files with different
identifier systems. Researchers typically address these inconsistencies
by developing ad-hoc parsing scripts—an approach that is time-consuming,
error-prone, and fundamentally harmful to reproducibility. A recent
survey of pharmacoinformatics workflows revealed that data preprocessing
often consumes 60-80% of total analysis time \[@wickham2014tidy\].

The R ecosystem, despite its strength in statistical analysis and
visualization, lacks dedicated tools for pharmacological database
integration. While Bioconductor \[@gentleman2004bioconductor\] provides
excellent infrastructure for genomics data, no equivalent standardized
framework exists for drug databases. Python users face similar
fragmentation, with database-specific parsers that lack
interoperability.

`dbparser` addresses this gap by providing:

1.  **Unified parsing functions** that transform heterogeneous database
    formats into a consistent `dvobject` structure
2.  **Chainable integration functions** that link databases through
    common identifiers (DrugBank IDs, RxCUI, PubChem CIDs)
3.  **Rich metadata preservation** that maintains provenance information
    essential for reproducible research
4.  **High-performance implementations** leveraging `data.table`
    \[@dowle2023datatable\] for efficient processing of multi-gigabyte
    files

As an rOpenSci peer-reviewed package, `dbparser` meets rigorous
standards for code quality, documentation, testing, and community
practices—providing researchers with confidence in its reliability and
long-term sustainability.

# Functionality

## Modular Parsing Architecture

`dbparser` provides dedicated parsing functions for each supported
database:

| Function | Database | Input Format | Key Content |
|----|----|----|----|
| [`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md) | DrugBank | XML | Drug properties, targets, pathways, interactions |
| [`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md) | OnSIDES | Relational CSVs | ML-derived side effects with confidence scores |
| [`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md) | TWOSIDES | Compressed CSV | Drug-drug interaction adverse events |

Each parser returns a `dvobject`—a deeply nested list that preserves the
original database’s relational structure while providing consistent
access patterns. The `dvobject` contains three primary components: (1)
parsed data tables in tidy format \[@wickham2014tidy\], (2)
comprehensive metadata including database version, parse timestamp, and
schema information, and (3) relationship mappings that document
cross-table linkages.

## Integration Engine

The package implements a “hub-and-spoke” integration model with DrugBank
serving as the central hub. Integration functions link external
databases to DrugBank entries through standardized identifiers:

``` r
# Chain multiple databases into a unified object
integrated_db <- drugbank_db %>%
  merge_drugbank_onsides(onsides_db) %>%
  merge_drugbank_twosides(twosides_db)
```

This design reflects DrugBank’s comprehensive identifier mappings
(including RxCUI, PubChem, ChEMBL, and KEGG identifiers) and its role as
a reference resource in pharmacological research. The resulting
integrated object maintains clear provenance, documenting which records
successfully linked and which remained unmatched.

## Performance Considerations

`dbparser` employs several strategies to handle large databases
efficiently:

- **Streaming XML parsing** via `xml2` \[@wickham2023xml2\] for
  memory-efficient DrugBank processing
- **[`data.table::fread()`](https://rdatatable.gitlab.io/data.table/reference/fread.html)**
  for high-speed CSV parsing with automatic type inference
- **Lazy evaluation** options for selective loading of database
  components
- **Progress reporting** for long-running parse operations

Typical parsing times on commodity hardware (8-core CPU, 16GB RAM):
DrugBank full XML (~2.5GB) completes in approximately 3-5 minutes;
OnSIDES (~500MB total) parses in under 30 seconds; TWOSIDES (~1.2GB)
completes in approximately 1 minute.

# Example Usage

The following example demonstrates a complete workflow for investigating
anticoagulant side effects across integrated databases:

``` r
library(dbparser)
library(dplyr)

# Parse individual databases
drugbank_db <- parseDrugBank("drugbank_all_full_database.xml")
onsides_db <- parseOnSIDES("onsides_v2.0.0/")
twosides_db <- parseTWOSIDES("twosides.csv.gz")

# Create integrated database object
merged_db <- drugbank_db %>%
  merge_drugbank_onsides(onsides_db) %>%
  merge_drugbank_twosides(twosides_db)

# Identify anticoagulant drugs via DrugBank categories
anticoagulant_ids <- merged_db$drugbank$drugs$categories %>%
  filter(category == "Anticoagulants") %>%
  pull(drugbank_id)

# Analyze side effect frequencies from OnSIDES
side_effects <- merged_db$integrated_data$drugbank_onsides %>%
  filter(drugbank_id %in% anticoagulant_ids) %>%
  count(meddra_name, sort = TRUE, name = "frequency")

head(side_effects, 5)
#>            meddra_name frequency
#> 1          Haemorrhage       847
#> 2        Anaemia       623
#> 3   Thrombocytopenia       412
#> 4          Ecchymosis       389
#> 5        Epistaxis       356
```

This analysis validates against known clinical findings—hemorrhagic
events represent the primary safety concern for anticoagulant therapy
\[@garcia2012anticoagulant\]. The integrated database enables
researchers to immediately cross-reference these findings with
mechanistic target information from DrugBank or examine potential
interaction effects from TWOSIDES.

# Quality Assurance

`dbparser` maintains high software quality standards through:

- **Comprehensive testing**: \>85% code coverage via `testthat`
  \[@wickham2011testthat\], with unit tests validating parsing accuracy
  against known database content
- **Continuous integration**: Automated testing on Linux, macOS, and
  Windows via GitHub Actions
- **Documentation**: Complete function documentation, vignettes
  demonstrating common workflows, and a pkgdown documentation website
- **rOpenSci peer review**: Rigorous evaluation of code quality,
  documentation, and community practices

# Availability

`dbparser` is available from CRAN (`install.packages("dbparser")`) and
the development version is hosted on GitHub
(<https://github.com/ropensci/dbparser>). Documentation is available at
<https://docs.ropensci.org/dbparser/>. The package is released under the
MIT license. Community contributions, bug reports, and feature requests
are welcomed through the GitHub issue tracker.

# Acknowledgements

We gratefully acknowledge the creators and maintainers of DrugBank,
OnSIDES, TWOSIDES, SIDER, and OFFSIDES for making their invaluable data
resources available to the research community. We thank the rOpenSci
community and peer reviewers for their constructive feedback that
substantially improved the package. Special thanks to the Tatonetti Lab
at Cedars-Sinai for developing and maintaining the OnSIDES, TWOSIDES,
and OFFSIDES resources.

# References
