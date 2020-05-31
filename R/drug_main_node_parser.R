# Extract drug data
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
    indication = xmlValue(drug[["indication"]]),
    pharmacodynamics = xmlValue(drug[["pharmacodynamics"]]),
    mechanism_of_action = xmlValue(drug[["mechanism-of-action"]]),
    metabolism = xmlValue(drug[["metabolism"]]),
    absorption = xmlValue(drug[["absorption"]]),
    half_life = xmlValue(drug[["half-life"]]),
    protein_binding = xmlValue(drug[["protein-binding"]]),
    route_of_elimination = xmlValue(drug[["route-of-elimination"]]),
    volume_of_distribution = xmlValue(drug[["volume-of-distribution"]]),
    clearance = xmlValue(drug[["clearance"]]),
    international_brands = xmlValue(drug[["international-brands"]]),
    fda_label = xmlValue(drug[["fda-label"]]),
    msds = xmlValue(drug[["msds"]]),
    toxicity = xmlValue(drug[["toxicity"]])
  )
}
