# Drug Reactions Parsers

Extract the sequential representation of the metabolic reactions that
this drug molecule is involved in. Depending on available information,
this may include metabolizing enzymes, reaction type, substrates,
products, pharmacological activity of metabolites, and a structural
representation of the biochemical reactions.

## Usage

``` r
drug_reactions()
```

## Value

a tibble with 5 variables:

- sequence:

  Reactions are displayed within a numerical sequence

- left_drugbank_name:

  The substrate of the reaction. Maybe a drug or a metabolite.

- rightt_drugbank_name:

  The product of the reaction. Maybe a drug or a metabolite.

- left_drugbank_id:

- right_drugbank_id:

- parent_id:

  drugbank id
