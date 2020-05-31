get_syn_df <- function(rec) {
  if (xmlSize(rec[["synonyms"]]) < 1) return()
  syn <- as_tibble(t(xmlSApply(rec[["synonyms"]], syn_rec)))
  syn[["drugbank-id"]] <- xmlValue(rec[["drugbank-id"]])
  return(syn)

}

syn_rec <- function(rec) {
  c(
    synonym = xmlValue(rec),
    language = xmlGetAttr(rec, name = "language"),
    coder = xmlGetAttr(rec,
                       name = "coder")
  )
}
