# Clean a gene list with HGNC: full data
#
# The same steps as the article "Clean a gene list with HGNC"
# (https://docs.ropensci.org/dbparser/articles/hgnc_gene_list.html), run on
# your own download of HGNC's files instead of the 24-gene sample bundled
# with dbparser.
#
# 1. Download hgnc_complete_set.txt and withdrawn.txt from
#    https://www.genenames.org/download/statistics-and-files/
# 2. Run, giving the folder that holds them (default: the current folder):
#      Rscript hgnc_full.R ~/Downloads/hgnc
# 3. Edit `genes` below for your own list. The result is written to
#    gene_list_clean.csv in the current folder.

library(dbparser)
library(dplyr)

args     <- commandArgs(trailingOnly = TRUE)
hgnc_dir <- if (length(args) > 0) args[[1]] else "."

genes <- c("PD1", "PDL1", "HER2", "SEPT1", "MARCH1", "FOX1",
           "RSC1A1", "TP53")

# Set these to your download; they are recorded in the object's metadata.
hgnc <- parseHGNC(file.path(hgnc_dir, "hgnc_complete_set.txt"),
                  withdrawn_path = file.path(hgnc_dir, "withdrawn.txt"),
                  release        = NULL,
                  retrieved_at   = Sys.Date())

glimpse(get_source_metadata(hgnc))

approved <- hgnc$gene |>
  select(hgnc_id, matched = symbol)

name_hits <- hgnc$gene_name |>
  filter(kind %in% c("symbol", "alias_symbol", "prev_symbol")) |>
  inner_join(approved, by = "hgnc_id") |>
  transmute(input = name,
            matched,
            how = recode(kind,
                         symbol       = "approved",
                         alias_symbol = "alias",
                         prev_symbol  = "previous"))

withdrawn_hits <- hgnc$withdrawn |>
  transmute(input   = withdrawn_symbol,
            matched = merged_into_symbol,
            how     = "withdrawn")

hits <- tibble(input = genes) |>
  left_join(bind_rows(name_hits, withdrawn_hits),
            by = "input", relationship = "one-to-many") |>
  distinct()

# One row per input; inputs that fit several genes are flagged, not resolved.
clean <- hits |>
  group_by(input) |>
  summarise(
    needs_choice = n_distinct(na.omit(matched)) > 1,
    how          = if (all(is.na(how))) "no match"
                   else paste(unique(na.omit(how)), collapse = ", "),
    matched      = paste(sort(unique(na.omit(matched))), collapse = ", "),
    .groups      = "drop") |>
  select(input, matched, how, needs_choice) |>
  arrange(match(input, genes))

print(clean, n = Inf)

hgnc$withdrawn |>
  filter(withdrawn_symbol %in% genes) |>
  select(withdrawn_symbol, status, merged_into_symbol, merged_into_status) |>
  print(n = Inf)

write.csv(clean, "gene_list_clean.csv", row.names = FALSE)
