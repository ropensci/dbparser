# Drug Mixtures parser

All commercially available products in which this drug is available in
combination with other drug molecules

## Usage

``` r
drug_mixtures()
```

## Value

a tibble with 4 variables:

- name:

  The proprietary name provided by the manufacturer for this combination
  product.

- ingredients:

  A list of ingredients, separated by addition symbols

- supplemental-ingredients:

  List of additional active ingredients which are not clinically
  relevant to the main indication of the product, separated by addition
  symbols.

- *drugbank_id*:

  drugbank id
