# Drug Salts parser

Available salt forms of the drug. Ions such as hydrochloride, sodium,
and sulfate are often added to the drug molecule to increase solubility,
dissolution, or absorption.

## Usage

``` r
drug_salts()
```

## Value

a tibble with 1 variables:

- drugbank-id:

  DrugBank identifiers of the available salt form(s).

- name:

  Name of the available salt form(s)

- unii:

  Unique Ingredient Identifier (UNII) of the available salt form(s).

- cas-number:

  Chemical Abstracts Service (CAS) registry number assigned to the salt
  form(s) of the drug.

- inchikey:

  IUPAC International Chemical Identifier (InChi) key identifier for the
  available salt form(s).

- average-mass:

  Average molecular mass: the weighted average of the isotopic masses of
  the salt.

- monoisotopic-mass:

  The mass of the most abundant isotope of the salt

- smiles:

  The simplified molecular-input line-entry system (SMILES) is a line
  notation used for describing the structure of chemical species using
  short ASCII strings; calculated by ChemAxon.

- inchi:

  A prediction of the IUPAC International Chemical Identifier (InChI);
  imported by ChemAxon.

- formula:

  Indicates the simple numbers of each type of atom within the molecule;
  calculated by ChemAxon.

- *drugbank_id*:

  parent drugbank id
