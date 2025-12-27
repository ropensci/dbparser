# Reads **DrugBank** xml database and load it into memory.

`read_drugbank_xml_db` loads **DrugBank** xml database full tree into
memory.

## Usage

``` r
read_drugbank_xml_db(db_path)
```

## Arguments

- db_path:

  **string**, full path for the **DrugBank** xml or zip file.

## Value

loaded DB or NULL

## Details

This functions reads **DrugBank** xml database and load it into memory
for later processing. Hence; this method **must** be called before any
other function in the package and it needs to be called one time only.

It takes one single mandatory argument which is the location of DrugBank
db.
