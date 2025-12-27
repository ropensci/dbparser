# Drug Sequences parser

The amino acid sequence; provided if the drug is a peptide.

## Usage

``` r
drug_sequences()
```

## Value

a tibble with the following variables:

- sequence:

  a textual representation of the sequence

- format:

  Currently, only the FASTA format is used

- *drugbank_id*:

  drugbank id

## Details

Describes peptide sequences of biotech drugs
