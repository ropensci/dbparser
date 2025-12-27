# Drugs/ Carriers/ Enzymes/ Targets/ Transporters articles element parser

Return a list of articles that were used as references for drugs
carriers

## Usage

``` r
drugs_articles()

carriers_articles()

enzymes_articles()

targets_articles()

transporters_articles()
```

## Value

a tibble with 4 variables:

- ref-id:

  Identifier for the article being referenced. This is unique across all
  reference types (books, links, article, attachments).

- pubmed-id:

  The PubMed identifier for the article.

- citation:

  Article citation in a standard format.

- *parent_id*:

  drug/carrier/target/enzyme/transporter id
