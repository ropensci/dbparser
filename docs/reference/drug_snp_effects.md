# Drug SNP Effects parser

A list of single nucleotide polymorphisms (SNPs) relevant to drug
activity or metabolism, and the effects these may have on
pharmacological activity. SNP effects in the patient may require close
monitoring, an increase or

## Usage

``` r
drug_snp_effects()
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

- defining-change:

- description:

  A written description of the SNP effects.

- pubmed-id :

  Reference to PubMed article.

- *drugbank_id*:

  drugbank id
