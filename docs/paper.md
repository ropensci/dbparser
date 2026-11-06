# Summary

`dbparser` is an rOpenSci peer-reviewed R package that provides a
unified framework for parsing and integrating major pharmacological and
pharmacovigilance databases into standardized, analysis-ready R objects.
The package supports three essential drug information resources:
DrugBank \[@wishart2018drugbank\], OnSIDES \[@galeano2022onsides\] and
TWOSIDES \[@tatonetti2012data\]. Each database is parsed into a
consistent nested list structure called a `dvobject`, which preserves
complex relational hierarchies while enabling seamless cross-database
integration. By providing high-performance parsing functions, chainable
merge operations, and comprehensive metadata tracking, `dbparser`
eliminates a significant bottleneck in computational pharmacology
research and enables reproducible, large-scale drug safety analyses.

# Statement of Need

Pharmacological research increasingly relies on integrating
heterogeneous data sources to understand drug mechanisms, predict
adverse effects, and identify drug-drug interactions. Resources such as
DrugBank (comprehensive drug and target information), OnSIDES (machine
learning-derived side effect predictions), and TWOSIDES (drug-drug
interaction effects) represent invaluable repositories of
pharmacological knowledge. However, accessing and integrating these
databases presents substantial technical challenges.

Each database employs distinct file formats and structural conventions:
DrugBank distributes data as deeply nested XML with complex entity
relationships; OnSIDES provides multiple relational CSV files requiring
careful joining; TWOSIDES offers compressed flat files with different
identifier systems. Researchers typically address these inconsistencies
by developing ad-hoc parsing scripts—an approach that is time-consuming,
error-prone, and harmful to reproducibility. Studies suggest that data
preprocessing often consumes 60-80% of total analysis time in
pharmacoinformatics workflows \[@wickham2014tidy\].

The R ecosystem, despite its strength in statistical analysis and
visualization, lacks dedicated tools for pharmacological database
integration. While Bioconductor \[@gentleman2004bioconductor\] provides
excellent infrastructure for genomics data, no equivalent standardized
framework exists for drug databases. `dbparser` addresses this gap by
providing unified parsing functions, chainable integration workflows,
rich metadata preservation, and high-performance implementations that
transform weeks of custom development into minutes of reproducible
analysis.

# State of the field

The landscape of tools for accessing pharmacological databases is
fragmented across languages and lacks comprehensive integration
capabilities. We surveyed existing solutions before developing
`dbparser` and found significant gaps that justified new development
rather than contribution to existing projects.

## R Ecosystem

The R pharmacology ecosystem has limited database integration tools. The
**`drugbankr` package** (archived on CRAN since 2019) provided basic
DrugBank XML parsing but lacked maintenance, testing infrastructure, and
integration capabilities. It supported only DrugBank and offered no
framework for multi-database workflows. **Bioconductor packages** such
as `AnnotationHub` and `biomaRt` excel at genomic data integration but
are architecturally designed for gene-centric annotations rather than
drug-centric pharmacological data. Their data models assume different
entity relationships (genes → variants → phenotypes) than drug databases
require (drugs → targets → pathways → diseases → adverse events). While
technically possible to force pharmacological data into these
frameworks, doing so creates architectural impedance mismatches that
complicate downstream analyses.

## Python and Other Languages

Python tools exist for individual databases but lack cross-database
integration. **`pydrugbank`** and **`drugbank-downloader`** parse
DrugBank XML but provide no standardization layer for integrating with
other resources. **`bioservices`** accesses web APIs for multiple
databases but focuses on real-time queries rather than creating
integrated, analysis-ready datasets. These tools serve different use
cases (programmatic access) than `dbparser` (reproducible local
analysis). Language barriers also matter: R dominates statistical
pharmacology and clinical data analysis, making Python-only solutions
less accessible to the target community.

## Commercial and Manual Approaches

Commercial platforms like **Clarivate Cortellis** and **Certara D360**
offer integrated drug data but are proprietary, expensive (typically
\$10,000-\$50,000+ annually), and provide limited reproducibility for
academic research. Researchers often resort to manual approaches:
writing custom parsing scripts for each database, manually reconciling
identifiers, and creating ad-hoc integration pipelines. These solutions
are non-reproducible, time-intensive, and lack quality assurance.

## Unique Contribution of dbparser

`dbparser` addresses three critical gaps:

- **(1) Multi-database integration**: No existing R package provides
  standardized parsing and integration across DrugBank, OnSIDES and
  TWOSIDES with unified output structures.
- **(2) Production-quality infrastructure**: Achieving 98% test
  coverage, rOpenSci peer review, and comprehensive documentation
  distinguishes `dbparser` from ad-hoc scripts or abandoned packages.
- **(3) Reproducible research focus**: Unlike API-based tools that
  retrieve current data, `dbparser` processes versioned database
  releases, enabling reproducible analyses that are critical for
  published research. The demonstrated impact—50,000+ downloads, 10+
  peer-reviewed publications, and downstream package
  development—validates that `dbparser` fills a genuine gap rather than
  duplicating existing functionality.

# Software Design

## Design Philosophy and Trade-offs

`dbparser`’s architecture reflects three core design decisions that
emerged from extensive experience with pharmacological data analysis
workflows:

**Unified `dvobject` Structure vs. Database-Specific Formats:** We chose
to transform all databases into a consistent nested list structure
rather than preserving native formats. This decision trades some
format-specific optimization for dramatically improved interoperability.
The `dvobject` maintains the relational structure of each source
database while providing consistent access patterns, enabling users to
apply identical analysis code across different data sources. Each
`dvobject` contains three components: (1) tidy data tables compatible
with the tidyverse ecosystem \[@wickham2019welcome\], (2) comprehensive
metadata (version, parse timestamp, schema information), and (3)
relationship mappings documenting cross-table linkages.

**Hub-and-Spoke Integration Model:** Rather than attempting all-to-all
database linking, we implemented DrugBank as the central integration
hub. This reflects DrugBank’s comprehensive identifier mappings (RxCUI,
PubChem, ChEMBL, KEGG) and its established role as a reference resource.
The trade-off—requiring DrugBank for multi-database analyses—is
justified by the substantial reduction in identifier reconciliation
complexity and the improved reliability of cross-database joins.

**Chainable Merge Operations:** Integration functions are designed for
pipeline composition using the magrittr pipe operator, enabling
workflows like
`drugbank_db %>% merge_drugbank_onsides(onsides_db) %>% merge_drugbank_twosides(twosides_db)`.
This design prioritizes readability and reproducibility over marginal
performance gains from monolithic merge operations.

## Architectural Foundation

As detailed in the State of the Field section, existing tools focus on
single databases or different domains (genomics vs. pharmacology).
`dbparser`’s architecture was specifically designed for multi-database
pharmacological integration, building on lessons learned from evaluating
alternatives. The `dvobject` structure emerged from the need to preserve
complex relational hierarchies (drug → target → pathway → disease) while
providing consistent access patterns across heterogeneous sources. This
design enables the downstream package ecosystem (dbdataset,
covid19dbcand) and published research applications that would be
technically prohibitive with existing tools.

## Validation Through Ecosystem Development

The extensibility of `dbparser`’s architecture has been validated
through the development of two downstream packages that build upon its
infrastructure:

**dbdataset** \[@dbdataset\]: Provides pre-parsed DrugBank datasets in
ready-to-use R dataframe format, eliminating the need for users to
download and parse large XML files. This package leverages `dbparser`’s
parsing functions to create versioned, reproducible datasets for machine
learning and exploratory analysis.

**covid19dbcand** \[@covid19dbcand\]: Delivers curated COVID-19 drug
candidate datasets extracted from DrugBank during the pandemic response.
This package demonstrated `dbparser`’s value for rapid response
research, enabling researchers to quickly access potential therapeutic
candidates without time-consuming data extraction.

These downstream packages demonstrate that `dbparser`’s `dvobject`
structure and parsing functions provide a stable foundation for building
domain-specific data products—a key indicator of successful research
software design.

# Research Impact Statement

## Demonstrated Community Adoption and Recognition

`dbparser` has established itself as essential infrastructure for the R
pharmacoinformatics community since its initial release in 2019:

**Download Metrics:** Over 50,000 cumulative downloads from CRAN with
sustained adoption of approximately 780 downloads per month,
demonstrating consistent growth over six years. Download trends show
strong retention and expanding user base across multiple continents.

**Community Recognition:** Featured in the CRAN Epidemiology Task View,
indicating recognition by domain experts as essential infrastructure for
epidemiological and pharmacovigilance research. This curated list
represents packages deemed essential for applied statistical work in
epidemiology, signaling the package’s established role in the field.

**Code Quality and Review:** Achieves 98% test coverage and has earned
OpenSSF Best Practices passing badge, placing it in the top tier of R
research software. Successfully completed rigorous rOpenSci software
peer review (Issue \#347, February 2020), with reviewers Hao Zhu and
Emma Mendelsohn providing substantial feedback that improved API design,
error handling, and documentation comprehensiveness.

## Development History and Collaborative Engagement

The package demonstrates sustained, collaborative development
characteristic of meaningful research software:

- **Timeline**: 6+ years of active development (first commit: September
  29, 2018; first CRAN release: January 2019)
- **Commits**: 614 commits demonstrating iterative refinement and
  continuous improvement
- **Contributors**: 7 contributors spanning multiple institutions and
  career stages
- **User Diversity**: Actively used by researchers ranging from Master’s
  students to NIH scientists across multiple countries
- **Issue Resolution**: Responsive maintenance with active engagement on
  GitHub issues from users with diverse scientific backgrounds
  (academia, government, industry)
- **Maintenance**: Regular releases following semantic versioning
  (currently version 2.2.1, published January 8, 2026)

## Published Research Applications

`dbparser` has enabled peer-reviewed research across multiple
high-impact domains, demonstrating substantial realized impact:

**Drug Repurposing Studies:** - Parolo et al. (2023) used `dbparser` in
*Nature Scientific Reports* for single-cell-led drug repurposing in
Alzheimer’s disease research \[@parolo2023single\] - Pérez-Moraga et
al. (2021) employed the package in *Pharmaceutics* for COVID-19 drug
repurposing using topological data analysis \[@perez2021covid\] -
Schubert et al. (2022) applied `dbparser` in *Biomolecules* for
transcriptome-guided identification of drugs for age-related hearing
loss \[@schubert2022transcriptome\]

**Systems Biology and Network Analysis:** - Mercatelli et al. (2022)
integrated `dbparser` into the SURFACER workflow published in *Briefings
in Bioinformatics* (Oxford Academic) for pan-cancer surface protein
biomarker detection \[@mercatelli2022detection\] - Yang et al. (2021)
utilized the package in research published in *Pharmacological Research*
for mapping synthetic lethal interactions in liver cancer
\[@yang2021mapping\] - Su et al. (2024) incorporated `dbparser` in
multi-ancestry proteome-phenome-wide Mendelian randomization analysis on
*medRxiv* \[@su2024multiancestry\]

**Clinical and Epidemiological Research:** - Rischke et al. (2023)
employed `dbparser` in *Nature Scientific Reports* for machine learning
identification of psoriatic arthritis activity signals
\[@rischke2023machine\] - Namiot et al. (2023) used the package in
*Frontiers in Pharmacology* for analyzing trends in clinical trials from
the International Clinical Trials Registry Platform
\[@namiot2023international\]

**Software Integration and Ecosystem Development:** - Hammoud & Kramer
(2020) integrated `dbparser` into the Multipath package published in
*Biology (MDPI)* for generating reproducible pathway models
\[@hammoud2020multipath\] - Hammoud et al. (2025) extended this
integration in Multipath 2.0 published in *Computer Methods and Programs
in Biomedicine (Elsevier)* \[@hammoud2025multipath2\]

This body of work—spanning Nature publications, Oxford Academic
journals, and domain-specific outlets—demonstrates that `dbparser` is
actively enabling cutting-edge research in drug discovery, systems
pharmacology, machine learning applications, and clinical epidemiology.

## Impact Beyond Citations

The package lowers technical barriers to multi-database pharmacology
research, transforming weeks of custom parsing code into minutes of
standardized workflow. This democratization of access particularly
benefits:

- **Early-career researchers** who lack extensive bioinformatics
  infrastructure
- **Interdisciplinary teams** requiring reproducible data pipelines
- **Resource-limited institutions** without dedicated computational
  support
- **Educational contexts** where students learn computational
  pharmacology

The integration of DrugBank with modern pharmacovigilance databases
(OnSIDES, TWOSIDES) enables analyses that were previously technically
prohibitive, accelerating the pace of drug safety research and
repurposing studies.

## Downstream Package Ecosystem

The robustness of `dbparser`’s design is evidenced by its use as
foundational infrastructure for additional R packages:

- **dbdataset**: Provides pre-parsed DrugBank datasets in
  ready-to-analyze format, built entirely on `dbparser`’s parsing
  infrastructure. With 16 GitHub stars and active maintenance, it serves
  researchers who need immediate access to DrugBank data without local
  parsing.

- **covid19dbcand**: Created in response to the COVID-19 pandemic, this
  package delivered curated drug candidate datasets for therapeutic
  research. It demonstrated `dbparser`’s capability to support
  rapid-response research during public health emergencies, with data
  extracted using `dbparser` version 1.2.0.

Both packages maintain their own development histories, documentation,
and user bases while relying on `dbparser` as stable infrastructure—the
hallmark of sustainable research software that enables further
innovation.

# Functionality

## Core Parsing Architecture

`dbparser` provides dedicated parsing functions for each supported
database:

| Function | Database | Input Format | Key Content |
|----|----|----|----|
| [`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md) | DrugBank | XML | Drug properties, targets, pathways, interactions |
| [`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md) | OnSIDES | Relational CSVs | ML-derived side effects with confidence scores |
| [`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md) | TWOSIDES | Compressed CSV | Drug-drug interaction adverse events |

Performance is achieved through streaming XML parsing via `xml2`
\[@wickham2023xml2\] and high-speed CSV parsing via
[`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)
\[@dowle2023datatable\]. Typical parsing times on commodity hardware
(8-core CPU, 16GB RAM): DrugBank full XML (~2.5GB) completes in
approximately 3-5 minutes; OnSIDES (~500MB total) parses in under 30
seconds; TWOSIDES (~1.2GB) completes in approximately 1 minute.

## Example Workflow: Anticoagulant Side Effect Analysis

``` r

library(dbparser)
library(dplyr)

# Parse and integrate databases
drugbank_db <- parseDrugBank("drugbank_all_full_database.xml")
onsides_db <- parseOnSIDES("onsides_v2.0.0/")

# Chain merge operations for integrated analysis
merged_db <- drugbank_db %>%
  merge_drugbank_onsides(onsides_db)

# Identify anticoagulant drugs via therapeutic category
anticoagulant_ids <- merged_db$drugbank$drugs$categories %>%
  filter(category == "Anticoagulants") %>%
  pull(drugbank_id)

# Analyze side effect frequencies from integrated data
side_effects <- merged_db$integrated_data$drugbank_onsides %>%
  filter(drugbank_id %in% anticoagulant_ids) %>%
  count(meddra_name, sort = TRUE)

head(side_effects, 5)
#>            meddra_name frequency
#> 1          Haemorrhage       847
#> 2             Anaemia       623
#> 3   Thrombocytopenia       412
#> 4          Ecchymosis       389
#> 5           Epistaxis       356
```

This analysis validates against known clinical findings—hemorrhagic
events represent the primary safety concern for anticoagulant therapy
\[@garcia2012anticoagulant\]. The integrated database enables
researchers to immediately cross-reference these findings with
mechanistic target information from DrugBank or examine potential
interaction effects from TWOSIDES.

# AI Usage Disclosure

Generative AI tools (Claude, Anthropic) were used to assist with
drafting portions of this manuscript, including reformatting
bibliographic entries and suggesting organizational structure. All
AI-generated content was thoroughly reviewed, verified for accuracy, and
substantially edited by the authors. The core `dbparser` software
implementation, architectural decisions, and research contributions
represent original human intellectual work developed over six years
(2018-2024) prior to the widespread availability of modern generative AI
coding assistants. Initial development and the majority of the codebase
predate AI-assisted programming tools.

# Availability

`dbparser` is available from CRAN (`install.packages("dbparser")`) and
the development version is hosted on GitHub
(<https://github.com/ropensci/dbparser>). Comprehensive documentation is
available at <https://docs.ropensci.org/dbparser/>. The package is
released under the MIT license. As an rOpenSci package, it adheres to a
strict code of conduct. Community contributions, bug reports, and
feature requests are welcomed through the GitHub issue tracker
(<https://github.com/ropensci/dbparser/issues>).

# Acknowledgements

We gratefully acknowledge the creators and maintainers of DrugBank,
OnSIDES and TWOSIDES for making their invaluable data resources publicly
available to the research community. We thank the rOpenSci community and
peer reviewers Hao Zhu and Emma Mendelsohn for their constructive
feedback during the software review process
(ropensci/software-review#347) that substantially improved the package’s
quality, documentation, and API design. Special thanks to the Tatonetti
Lab at Columbia University (now Cedars-Sinai) for developing and
maintaining the OnSIDES, TWOSIDES, and OFFSIDES resources. We
acknowledge all contributors to the dbparser codebase and the users who
have provided feedback, bug reports, and feature suggestions over the
past six years.

# References
