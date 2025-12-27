# Carriers/ Enzymes/ Targets/ Transporters Polypeptide parsers

Extract descriptions of identified polypeptide targets, enzymes,
carriers, or transporters.

## Usage

``` r
carriers_polypeptides()

enzymes_polypeptides()

targets_polypeptides()

transporters_polypeptides()
```

## Value

a tibble with 20 variables:

- id:

  [Universal Protein Resource (UniProt)
  identifier](https://www.uniprot.org/)

- source:

  Specifies whether the identified polypeptide ID is associated with any
  of the following UniProt knowledge bases: Swiss-Prot, which is
  manually annotated and reviewed, or TrEMBL, which is automatically
  annotated and not reviewed.

- name:

- general_function:

  General summary of the physiological function of the polypeptide

- specific_function:

  A more specific description of the polypeptide’s physiological
  function within the cell.

- gene_name:

  The short name commonly associated with the associated gene. Eg.
  PTGS1.

- locus:

  The specific chromosomal location or position of the gene’s sequence
  on a chromosome.

- cellular_location:

  The cellular location of the polypeptide.

- transmembrane_regions:

  Areas of the polypeptide sequence that span a biological membrane.

- signal_regions:

  Location of any signal peptides within the polypeptide sequence.

- theoretical_pi:

  Theoretical isoelectric point.

- molecular_weight:

  The molecular weight of the polypeptide.

- chromosome_location:

  The chromosomal location of the polypeptide gene

- organism:

  The organism in which this polypeptide functions.

- organism_ncbi_taxonomy_id:

- amino_acid_sequence:

  The amino acid sequence of the polypeptide

- amino_acid_format:

- gene_sequence:

  The sequence of the associated gene.

- gene_format:

- parent_key:

  carrier/ target/ enzyme/ transporter id
