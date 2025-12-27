# Drug Patents parser A property right issued by the United States Patent and Trademark Office (USPTO) to an inventor for a limited time, in exchange for public disclosure of the invention when the patent is granted. Drugs may be issued multiple patents.

Drug Patents parser A property right issued by the United States Patent
and Trademark Office (USPTO) to an inventor for a limited time, in
exchange for public disclosure of the invention when the patent is
granted. Drugs may be issued multiple patents.

## Usage

``` r
drug_patents()
```

## Value

a tibble with the following variables:

- number:

  The patent number(s) associated with the drug.

- country:

  The country that issued the patent rights.

- approved:

  The date that the patent request was filed.

- expires:

  The date that the patent rights expire.

- pediatric-extension:

  Indicates whether or not a pediatric extension has been approved for
  the patent. Granted pediatric extensions provide an additional 6
  months of market protection.

- *drugbank_id*:

  drugbank id
