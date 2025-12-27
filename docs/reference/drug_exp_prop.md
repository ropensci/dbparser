# Drug Experimental Properties parser

Drug properties that have been experimentally proven

## Usage

``` r
drug_exp_prop()
```

## Value

a tibble with the following variables:

- kind:

  Name of the property.

- value:

  Drug properties that have been experimentally proven.

- source:

  Reference to the source of this experimental data.

- *drugbank_id*:

  drugbank id

The following experimental properties are provided:

- Water Solubility:

  The experimentally determined aqueous solubility of the molecule.

- Molecular Formula:

  Protein formula of Biotech drugs

- Molecular Weight:

  Protein weight of Biotech drugs.

- Melting Point:

  The experimentally determined temperature at which the drug molecule
  changes from solid to liquid at atmospheric temperature.

- Boiling Point:

  The experimentally determined temperature at which the drug molecule
  changes from liquid to gas at atmospheric temperature.

- Hydrophobicity:

  The ability of a molecule to repel water rather than absorb or
  dissolve water.

- Isoelectric Point:

  The pH value at which the net electric charge of a molecule is zero.

- caco2 Permeability:

  A continuous line of heterogenous human epithelial colorectal
  adenocarcinoma cells, CAC02 cells are employed as a model of human
  intestinal absorption of various drugs and compounds. CAC02 cell
  permeability is ultimately an assay to measure drug absorption.

- pKa:

  The experimentally determined pka value of the molecule

- logP:

  The experimentally determined partition coefficient (LogP) based on
  the ratio of solubility of the molecule in 1-octanol compared to
  water.

- logS:

  The intrinsic solubility of a given compound is the concentration in
  equilibrium with its solid phase that dissolves into solution, given
  as the natural logarithm (LogS) of the concentration.

- Radioactivity:

  The property to spontaneously emit particles (alpha, beta, neutron) or
  radiation (gamma, K capture), or both at the same time, from the decay
  of certain nuclides.
