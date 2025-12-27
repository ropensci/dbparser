# Drugs/ Carriers/ Enzymes/ Targets/ Transporters attachments element parser

Return a list of attachment that were used as references for drugs
carriers

## Usage

``` r
drugs_attachments()

carriers_attachments()

enzymes_attachments()

targets_attachments()

transporters_attachments()
```

## Value

a tibble with 4 variables:

- ref-id:

  Identifier for the article being referenced. This is unique across all
  reference types (books, links, article, attachments).

- title:

  The title of the attachment.

- url:

  The url to download the attachment from.

- *parent_id*:

  drug/carrier/target/enzyme/transporter id
