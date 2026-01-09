# Drug SNP Adverse Drug Reactions parser

The adverse drug reactions that may occur as a result of the listed
single nucleotide polymorphisms (SNPs)

## Usage

``` r
drug_snp_adverse_reactions()
```

## Value

a tibble with the following variables:

- protein-name:

  Proteins involved in this SNP.

- gene-symbol:

  Genes involved in this SNP.

- uniprot-id:

  Universal Protein Resource (UniProt) identifiers for proteins involved
  in this pathway.

- rs-id:

  The SNP Database identifier for this single nucleotide polymorphism.

- allele:

  The alleles associated with the identified SNP.

- adverse-reaction:

- description:

- pubmed-id :

  Reference to PubMed article.

- *drugbank_id*:

  drugbank id
