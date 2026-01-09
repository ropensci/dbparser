# Drug Interactions parser

Drug-drug interactions detailing drugs that, when administered
concomitantly with the drug of interest, will affect its activity or
result in adverse effects. These interactions may be synergistic or
antagonistic depending on the physiological effects and mechanism of
action of each drug.

## Usage

``` r
drug_interactions()
```

## Value

a tibble with the following variables:

- drugbank-id :

  DrugBank ID of the interacting drug.

- name:

  Name of the interacting drug.

- description:

  Textual description of the physiological consequences of the drug
  interaction

- *drugbank_id*:

  parent drugbank id
