# Drugs General Information parser

A description of the hierarchical chemical classification of the drug;
imported from ClassyFire.

## Usage

``` r
drug_general_information()
```

## Value

a tibble with 15 variables:

- primary_key:

  DrugBank id

- other_keys:

  Other identifiers that may be associated with the drug

- type:

  Either small molecule, or biotech. Biotech is used for any drug that
  is derived from living systems or organisms, usually composed of high
  molecular weight mixtures of protein, while small molecule describes a
  low molecular weight organic compound.

- name:

- created:

  Date that this drug was first added to DrugBank.

- updated:

  Denotes when this drug was last updated in DrugBank.

- description:

  Descriptions of drug chemical properties, history and regulatory
  status.

- cas_number:

  The Chemical Abstracts Service (CAS) registry number assigned to the
  drug.

- *unii*:

  Unique Ingredient Identifier (UNII) of this drug.

- average_mass:

  The weighted average of the isotopic masses of the drug

- state:

  One of solid, liquid, or gas

- monoisotopic_mass:

  The mass of the most abundant isotope of the drug

- synthesis_reference:

  Citation for synthesis of the drug molecule.

- fda_label:

  Contains a URL for accessing the uploaded United States Food and Drug
  Administration (FDA) Monograph for this drug.

- msds:

  Contains a URL for accessing the Material Safety Data Sheet (MSDS) for
  this drug.
