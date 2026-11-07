<!-- Draft. Post in GitHub Discussions, category "Announcements", once CRAN accepts 2.3.0. -->

**Title:** dbparser 2.3.0 is on CRAN

dbparser 2.3.0 is now on CRAN:

```r
install.packages("dbparser")
```

**New parsers**

- `parseHGNC()`: the HGNC gene nomenclature files (complete set and
  withdrawn entries).
- `parseHealthCanadaDPD()`: Health Canada's Drug Product Database data
  extract, all 13 tables.
- `parseOpenFDA()`: openFDA's Drugs@FDA and NDC directory downloads.
- `parseHPO()`: Human Phenotype Ontology gene-phenotype associations from
  Harmonizome.

**Source metadata.** Every parsed object now records the source's release,
licence, licence URL, the citation it asks for, and the SHA-256 hash of each
file read. `get_source_metadata()` returns it as a table.

**Fixed.** `parseDrugBank()` no longer extracts a zip next to the input file.

**Where to start**

- Getting started: https://docs.ropensci.org/dbparser/articles/getting_started.html
- Tables and columns of each parser: https://docs.ropensci.org/dbparser/articles/dvobject_structures.html
- Worked examples:
  [DrugBank](https://docs.ropensci.org/dbparser/articles/dbparser.html),
  [integrated pharmacovigilance](https://docs.ropensci.org/dbparser/articles/drugbank_nside.html),
  [HPO target phenotypes](https://docs.ropensci.org/dbparser/articles/hpo_target_phenotypes.html)
- All changes: https://docs.ropensci.org/dbparser/news/

Questions, problems with a source file, or a database you would like parsed:
reply here or open a new discussion.
