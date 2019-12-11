# Extract drug df
drug_df <- function(rec) {
  tibble(
    primary_key = xmlValue(rec["drugbank-id"][[1]]),
    other_keys = ifelse(length(rec["drugbank-id"]) > 1,
      paste(map_chr(
        c(2:length(rec["drugbank-id"])),
        ~ xmlValue(rec["drugbank-id"][[.]])
      ),
      collapse = ","
      ), NA
    ),
    type = xmlGetAttr(node = rec, name = "type"),
    created = as.Date(xmlGetAttr(node = rec, name = "created")),
    updated = as.Date(xmlGetAttr(node = rec, name = "updated")),
    name = xmlValue(rec[["name"]]),
    description = xmlValue(rec[["description"]]),
    cas_number = xmlValue(rec[["cas-number"]]),
    unii = xmlValue(rec[["unii"]]),
    average_mass = xmlValue(rec[["average-mass"]]),
    monoisotopic_mass = xmlValue(rec[["monoisotopic-mass"]]),
    state = xmlValue(rec[["state"]]),
    groups_count = xmlSize(rec[["groups"]]),
    articles_count = xmlSize(rec[["general-references"]][["articles"]]),
    books_count = xmlSize(rec[["general-references"]][["textbooks"]]),
    links_count = xmlSize(rec[["general-references"]][["links"]]),
    synthesis_reference = xmlValue(rec[["synthesis-reference"]]),
    indication = xmlValue(rec[["indication"]]),
    pharmacodynamics = xmlValue(rec[["pharmacodynamics"]]),
    mechanism_of_action = xmlValue(rec[["mechanism-of-action"]]),
    metabolism = xmlValue(rec[["metabolism"]]),
    absorption = xmlValue(rec[["absorption"]]),
    half_life = xmlValue(rec[["half-life"]]),
    protein_binding = xmlValue(rec[["protein-binding"]]),
    route_of_elimination = xmlValue(rec[["route-of-elimination"]]),
    volume_of_distribution = xmlValue(rec[["volume-of-distribution"]]),
    clearance = xmlValue(rec[["clearance"]]),
    international_brands = xmlValue(rec[["international-brands"]]),
    pdb_entries = xmlSize(rec[["pdb-entries"]]),
    fda_label = xmlValue(rec[["fda-label"]]),
    msds = xmlValue(rec[["msds"]]),
    food_interactions = xmlSize(rec[["food-interactions"]]),
    drug_interactions_count = xmlSize(rec[["drug-interactions"]]),
    toxicity = xmlValue(rec[["toxicity"]])
  )
}