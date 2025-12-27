# Drugs/ Carriers/ Enzymes/ Targets/ Transporters links element parser

Return a list of websites that were used as references for Drugs/
Carriers/ Enzymes/ Targets/ Transporters

## Usage

``` r
drugs_links()

carriers_links()

enzymes_links()

targets_links()

transporters_links()
```

## Value

a tibble with 4 variables:

- ref-id:

  Name of the source website

- title:

  Identifier for this drug in the given resource

- url:

  The url of the website

- *parent_id*:

  drug/ carrier/ target/ enzyme/ transporter id
