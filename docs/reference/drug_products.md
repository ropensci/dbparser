# Drug Products parser

A list of commercially available products in Canada and the United
States that contain the drug.

## Usage

``` r
drug_products()
```

## Value

a tibble with 32 variables:

- name:

  The proprietary name(s) provided by the manufacturer for any
  commercially available products containing this drug.

- labeller:

  The corporation responsible for labelling this product.

- ndc-id:

  The National Drug Code (NDC) identifier of the drug

- ndc-product-code:

  The National Drug Code (NDC) product code from the FDA National Drug
  Code directory.

- dpd-id:

  Drug Product Database (DPD) identification number (a.k.a. DIN) from
  the Canadian Drug Product Database. Only present for drugs that are
  marketed in Canada

- ema-product-code:

  EMA product code from the European Medicines Agency Database. Only
  present for products that are authorized by central procedure for
  marketing in the European Union.

- ema-ma-number:

  EMA marketing authorization number from the European Medicines Agency
  Database. Only present for products that are authorized by central
  procedure for marketing in the European Union.

- started-marketing-on:

  The starting date for market approval.

- ended-marketing-on:

  The ending date for market approval.

- dosage-form :

  The pharmaceutical formulation by which the drug is introduced into
  the body.

- strength:

  The amount of active drug ingredient provided in the dosage

- route:

  The path by which the drug or product is taken into the body

- fda-application-number:

  The New Drug Application \[NDA\] number assigned to this drug by the
  FDA.

- over-the-counter:

  A list of Over The Counter (OTC) forms of the drug.

- generic:

  Whether this product is a generic drug.

- approved:

  Indicates whether this drug has been approved by the regulating
  government.

- country:

  The country where this commercially available drug has been approved.

- source:

  Source of this product information. For example, a value of DPD
  indicates this information was retrieved from the Canadian Drug
  Product Database.

- standing:

  One of good, discordant, or deprecated. Distinguishes products with up
  to date ingredient information (good) from products with conflicting
  information (discordant) or products that have been removed from an
  active label (deprecated).

- standing-updated-on:

  The date on which the standing was last updated

- standing-reason:

  Explains the non-good standing of the product. One of:
  ingredient_change, code_duplication, invalid, or removed.

- jurisdiction-marketing-category :

  The marketing category of this product in its jurisdiction

- branded:

  Whether this product has a named brand

- prescription:

  Whether this product is only available with a prescription

- unapproved:

  Whether this product is not approved in its jurisdiction

- vaccine:

  Whether this product is a vaccine

- allergenic:

  Whether this product is used in allergenic testing

- cosmetic:

  Whether this product is a cosmetic, such as sunscreen

- kit:

  Whether this product is a kit composed of multiple distinct parts

- solo:

  Whether this product has only a single active ingredient

- available:

  Whether this product can be sold in its jurisdiction

- *drugbank_id*:

  drugbank id
