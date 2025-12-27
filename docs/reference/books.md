# Drugs/ Carriers/ Enzymes/ Targets/ Transporters books element parser

Return a list of text books that were used as references for drugs,
carriers, enzymes, targets or transporters

## Usage

``` r
drugs_textbooks()

carriers_textbooks()

enzymes_textbooks()

targets_textbooks()

transporters_textbooks()
```

## Value

a tibble with 4 variables:

- ref-id:

  Identifier for the article being referenced. This is unique across all
  reference types (books, links, article, attachments).

- isbn:

  ISBN identifying the textbook.

- citation:

  A Textbook citation in a standard format.

- *parent_id*:

  drug/ carrier/ target/ enzyme/ transporter id
