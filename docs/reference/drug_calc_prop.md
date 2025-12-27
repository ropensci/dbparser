# Drug Calculated Properties parser

Drug properties that have been predicted by ChemAxon or ALOGPS based on
the imputed chemical structure. Associated links below will redirect to
descriptions of the specific term.

## Usage

``` r
drug_calc_prop()
```

## Value

a tibble with 4 variables:

- kind:

  Name of the property.

- value:

  Predicted physicochemical properties; obtained by the use of
  prediction software such as ALGOPS and ChemAxon.

- source:

  Name of the software used to calculate this property, either ChemAxon
  or ALOGPS.

- *drugbank_id*:

  drugbank id
