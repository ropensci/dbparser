# Extract drug general infromation
drug_row <- function(drug) {
  d_elements <- names(drug)
  ids <- drug[d_elements == "drugbank-id"]
  ids_length <- length(ids)
  ids <- map_chr(ids, xmlValue)
  c(
    primary_key = ids[[1]],
    other_keys = ifelse(ids_length > 1,
                        paste(unlist(ids[2:ids_length]), collapse = ";"), NA),
    type = xmlGetAttr(node = drug, name = "type"),
    created = xmlGetAttr(node = drug, name = "created"),
    updated = xmlGetAttr(node = drug, name = "updated"),
    name = xmlValue(drug[["name"]]),
    description = xmlValue(drug[["description"]]),
    cas_number = xmlValue(drug[["cas-number"]]),
    unii = xmlValue(drug[["unii"]]),
    average_mass = xmlValue(drug[["average-mass"]]),
    monoisotopic_mass = xmlValue(drug[["monoisotopic-mass"]]),
    state = xmlValue(drug[["state"]]),
    synthesis_reference = xmlValue(drug[["synthesis-reference"]]),
    fda_label = xmlValue(drug[["fda-label"]]),
    msds = xmlValue(drug[["msds"]])
  )
}
