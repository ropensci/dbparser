<!-- Draft. Use as the GitHub release notes for tag v2.3.0 once CRAN accepts 2.3.0. -->

# dbparser 2.3.0

## New parsers

- `parseHGNC()` reads the HGNC complete set (`hgnc_complete_set.txt`) and,
  optionally, its withdrawn entries (`withdrawn.txt`). It returns `gene`,
  `gene_name` (every approved, alias and previous symbol and name, one row
  each) and `withdrawn` (one row per withdrawn ID and the ID it was merged into
  or split to).
- `parseHealthCanadaDPD()` reads Health Canada's Drug Product Database data
  extract: the marketed, approved, cancelled and dormant product sets, from
  their zips or unzipped folders. It returns the 13 tables of Health Canada's
  read-me, with an `extract` column naming each row's product set.
- `parseOpenFDA()` reads openFDA's Drugs@FDA and NDC directory downloads,
  zipped or not, without extracting them to disk. It returns one table per
  record level, with field names as in the files.
- `parseHPO()` reads Human Phenotype Ontology gene-phenotype associations as
  distributed by Harmonizome, with `merge_drugbank_hpo()`,
  `search_phenotypes()` and `get_genes_for_phenotype()`.

Every record is kept as the source writes it; the parsers do no status
grouping.

## Source metadata on every parsed object

Every parser now records where its data came from: release, release date,
download date, source URL, licence (SPDX id), licence URL, the attribution or
citation the source asks for, the parsed files with their SHA-256 hashes, and
the dbparser version. Unknown values are `NA`. Read it with the new
`get_source_metadata()`. Merge and subset functions keep the record of every
source. File hashes need R 4.5.0 or later.

New parsers name their version arguments `release` and `release_date`;
existing parsers keep `db_version` and `db_exported_date` for now.

## Bug fixes

- `parseDrugBank()` extracts a zip input into a temporary folder that is
  removed after loading. Earlier versions extracted it next to the input file,
  which for the bundled example is the installed package.
- CETT `actions` and `polypeptides$general_information` tables include a
  `drugbank_id` column (#210).
- The merge functions no longer give tidyselect `.data` deprecation warnings.

## Documentation

- New vignette `vignette("getting_started")`.
- The three vignettes with interactive charts are now articles on the package
  website: https://docs.ropensci.org/dbparser/articles/
- New article listing the tables and columns each parser returns:
  https://docs.ropensci.org/dbparser/articles/dvobject_structures.html
- New vignette `vignette("cett_mechanisms")`.

## Other changes

- The maintainer email is now mohammed.ali@interstellar-egypt.com.
- `jsonlite` is a new dependency.

Full changes: https://docs.ropensci.org/dbparser/news/
