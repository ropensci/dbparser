# Carriers/ Enzymes/ Targets/ Transporters parsers

Protein targets of drug action, enzymes that are inhibited/induced or
involved in metabolism, and carrier or transporter proteins involved in
movement of the drug across biological membranes.

## Usage

``` r
carriers()

enzymes()

targets()

transporters()
```

## Value

a tibble with 6 variables (8 for enzymes):

- id:

  Universal Protein Resource (UniProt) Identifier for the record

- name:

  related name

- organism:

  Organism that the protein comes from.

- known_action:

  Whether the pharmacological action of the drug is due to this target
  interaction.

- inhibition-strength:

  Whether the strength of enzyme inhibition is strong, moderate, or
  unknown. **Only applies to enzymes**

- induction-strength:

  Whether the strength of enzyme induction is strong or unknown. **Only
  applies to enzymes**

- position:

  related position

- parent_id:

  drugbank id
