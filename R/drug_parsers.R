parse_drug <- function() {
    # db connection
    drugs <- map_df(children, ~drug_df(.x))
    save_drug_sub(con = con, df = drugs, table_name = "drug", primary_key = "primary_key", foreign_key = NULL, field.types = list(description = "varchar(6349)", 
        mechanism_of_action = "varchar(7189)", pharmacodynamics = "varchar(3179)", indication = "varchar(3165)", absorption = "nvarchar(3579)", 
        route_of_elimination = "varchar(1324)", metabolism = "varchar(2926)", international_brands = "varchar(2904)", protein_binding = "varchar(778)", 
        synthesis_reference = "varchar(946)", clearance = "varchar(2128)", half_life = "varchar(1173)", route_of_elimination = "varchar(1324)", 
        absorption = "varchar(3579)", volume_of_distribution = "varchar(1378)", toxicity = "varchar(max)", created = "date", 
        updated = "date"))
    return(drugs)
}


parse_drug_groups <- function() {
    drug_groups <- map_df(children, ~drug_sub_df(.x, "groups"))
    save_drug_sub(con = con, df = drug_groups, table_name = "drug_groups")
    return(drug_groups)
}

parse_drug_articles <- function() {
    drug_articles <- map_df(children, ~drug_sub_df(.x, "general-references", seconadary_node = "articles"))
    save_drug_sub(con = con, df = drug_articles, table_name = "drug_articles")
    return(drug_articles)
}

parse_drug_books <- function() {
    drug_books <- map_df(children, ~drug_sub_df(.x, "general-references", seconadary_node = "textbooks"))
    save_drug_sub(con = con, df = drug_books, table_name = "drug_books")
    return(drug_books)
}

parse_drug_links <- function() {
    drug_links <- map_df(children, ~drug_sub_df(.x, "general-references", seconadary_node = "links"))
    save_drug_sub(con = con, df = drug_links, table_name = "drug_links")
    return(drug_links)
}


parse_drug_classfications <- function() {
    drug_classfications <- map_df(children, ~drug_classfications_df(.x))
    save_drug_sub(con = con, df = drug_classfications, table_name = "drug_classfications")
    return(drug_classfications)
}

parse_drug_synonyms <- function() {
    drug_synonyms <- map_df(children, ~get_synonyms_df(.x))
    save_drug_sub(con = con, df = drug_synonyms, table_name = "drug_synonyms", field.types = list(synonym = "varchar(534)"))
    return(drug_synonyms)
}

parse_drug_products <- function() {
    drug_products <- map_df(children, ~drug_sub_df(.x, "products"))
    save_drug_sub(con = con, df = drug_products, table_name = "drug_products")
    return(drug_products)
}

parse_drug_mixtures <- function() {
    drug_mixtures <- map_df(children, ~drug_sub_df(.x, "mixtures"))
    save_drug_sub(con = con, df = drug_mixtures, table_name = "drug_mixtures")
    return(drug_mixtures)
}

parse_drug_packagers <- function() {
    drug_packagers <- map_df(children, ~drug_sub_df(.x, "packagers"))
    save_drug_sub(con = con, df = drug_packagers, table_name = "drug_packagers")
    return(drug_packagers)
}


parse_drug_manufacturers <- function() {
    drug_manufacturers <- map_df(children, ~drug_sub_df(.x, "manufacturers"))
    save_drug_sub(con = con, df = drug_manufacturers, table_name = "drug_manufacturers")
    return(drug_manufacturers)
}

parse_drug_prices <- function() {
    drug_prices <- map_df(children, ~get_prices_df(.x))
    save_drug_sub(con = con, df = drug_prices, table_name = "drug_prices")
    return(drug_prices)
}

parse_drug_categories <- function() {
    drug_categories <- map_df(children, ~drug_sub_df(.x, "categories"))
    save_drug_sub(con = con, df = drug_categories, table_name = "drug_categories")
    return(drug_categories)
}

parse_drug_affected_organisms <- function() {
    drug_affected_organisms <- map_df(children, ~drug_sub_df(.x, "affected-organisms"))
    save_drug_sub(con = con, df = drug_affected_organisms, table_name = "drug_affected_organisms")
    return(drug_affected_organisms)
}

parse_drug_dosages <- function() {
    drug_dosages <- map_df(children, ~drug_sub_df(.x, "dosages"))
    save_drug_sub(con = con, df = drug_dosages, table_name = "drug_dosages")
    return(drug_dosages)
}

parse_drug_atc_codes <- function() {
    drug_atc_codes <- map_df(children, ~get_atc_codes_df(.x))
    save_drug_sub(con = con, df = drug_atc_codes, table_name = "drug_atc_codes")
    return(drug_atc_codes)
}

parse_drug_ahfs_codes <- function() {
    drug_ahfs_codes <- map_df(children, ~drug_sub_df(.x, "ahfs-codes"))
    save_drug_sub(con = con, df = drug_ahfs_codes, table_name = "drug_ahfs_codes")
    return(drug_ahfs_codes)
}

parse_drug_pdb_entries <- function() {
    drug_pdb_entries <- map_df(children, ~drug_sub_df(.x, "pdb-entries"))
    save_drug_sub(con = con, df = drug_pdb_entries, table_name = "drug_pdb_entries")
    return(drug_pdb_entries)
}

parse_drug_patents <- function() {
    drug_patents <- map_df(children, ~drug_sub_df(.x, "patents"))
    save_drug_sub(con = con, df = drug_patents, table_name = "drug_patents")
    return(drug_patents)
}

parse_drug_food_interactions <- function() {
    drug_food_interactions <- map_df(children, ~drug_sub_df(.x, "food-interactions"))
    save_drug_sub(con = con, df = drug_food_interactions, table_name = "drug_food_interactions")
    return(drug_food_interactions)
}

parse_drug_interactions <- function() {
    drug_drug_interactions <- map_df(children, ~drug_sub_df(.x, "drug-interactions"))
    save_drug_sub(con = con, df = drug_drug_interactions, table_name = "drug_drug_interactions")
    return(drug_drug_interactions)
}

parse_drug_sequences <- function() {
    drug_sequences <- map_df(children, ~get_sequences_df(.x))
    save_drug_sub(con = con, df = drug_sequences, table_name = "drug_sequences")
    return(drug_sequences)
}

parse_drug_experimental_properties <- function() {
    drug_experimental_properties <- map_df(children, ~drug_sub_df(.x, "experimental-properties"))
    save_drug_sub(con = con, df = drug_experimental_properties, table_name = "drug_experimental_properties")
    return(drug_experimental_properties)
}

parse_drug_external_identifiers <- function() {
    drug_external_identifiers <- map_df(children, ~drug_sub_df(.x, "external-identifiers"))
    save_drug_sub(con = con, df = drug_external_identifiers, table_name = "drug_external_identifiers")
    return(drug_external_identifiers)
}

parse_drug_external_links <- function() {
    drug_external_links <- map_df(children, ~drug_sub_df(.x, "external-links"))
    save_drug_sub(con = con, df = drug_external_links, table_name = "drug_external_links")
    return(drug_external_links)
}


parse_drug_pathway <- function() {
    drug_pathway <- map_df(children, ~get_pathways_df(.x))
    save_drug_sub(con = con, df = drug_pathway, table_name = "drug_pathway")
    return(drug_pathway)
}

parse_drug_pathway_drugs <- function() {
    drug_pathway_drugs <- map_df(children, ~get_pathways_drugs_df(.x))
    save_drug_sub(con = con, df = drug_pathway_drugs, table_name = "drug_pathway_drugs", save_table_only = TRUE)
    return(drug_pathway_drugs)
}

parse_drug_pathway_enzyme <- function() {
    drug_pathway_enzymes <- map_df(children, ~get_pathways_enzymes_df(.x))
    save_drug_sub(con = con, df = drug_pathway_enzymes, table_name = "drug_pathway_enzyme", save_table_only = TRUE)
    return(drug_pathway_enzymes)
}

parse_drug_snp_effects <- function() {
    drug_snp_effects <- map_df(children, ~drug_sub_df(.x, "snp-effects"))
    save_drug_sub(con = con, df = drug_snp_effects, table_name = "drug_snp_effects")
    return(drug_snp_effects)
}

parse_drug_snp_adverse_drug_reactions <- function() {
    drug_snp_adverse_drug_reactions <- map_df(children, ~drug_sub_df(.x, "snp-adverse-drug-reactions"))
    save_drug_sub(con = con, df = drug_snp_adverse_drug_reactions, table_name = "drug_snp_adverse_drug_reactions")
    return(drug_snp_adverse_drug_reactions)
}

parse_drug_enzymes <- function() {
    drug_enzymes <- map_df(children, ~get_enzymes_df(.x))
    save_drug_sub(con = con, df = drug_enzymes, table_name = "drug_enzymes")
    return(drug_enzymes)
}

parse_drug_enzymes_actions <- function() {
    drug_enzymes_actions <- map_df(children, ~get_enzymes_actions_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_actions, table_name = "drug_enzymes_actions", save_table_only = TRUE)
    return(drug_enzymes_actions)
}

parse_drug_targets_actions <- function() {
    drug_targets_actions <- map_df(children, ~get_targets_actions_df(.x))
    save_drug_sub(con = con, df = drug_targets_actions, table_name = "drug_targets_actions", save_table_only = TRUE)
    return(drug_targets_actions)
}

parse_drug_carriers_actions <- function() {
    drug_carriers_actions <- map_df(children, ~get_carriers_actions_df(.x))
    save_drug_sub(con = con, df = drug_carriers_actions, table_name = "drug_carriers_actions", save_table_only = TRUE)
    return(drug_carriers_actions)
}

parse_drug_transporters_actions <- function() {
    drug_transporters_actions <- map_df(children, ~get_transporters_actions_df(.x))
    save_drug_sub(con = con, df = drug_transporters_actions, table_name = "drug_transporters_actions", save_table_only = TRUE)
    return(drug_transporters_actions)
}

parse_drug_enzymes_articles <- function() {
    drug_enzymes_articles <- map_df(children, ~get_enzymes_articles_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_articles, table_name = "drug_enzymes_articles", save_table_only = TRUE)
    return(drug_enzymes_articles)
}


parse_drug_targets_articles <- function() {
    drug_targets_articles <- map_df(children, ~get_targets_articles_df(.x))
    save_drug_sub(con = con, df = drug_targets_articles, table_name = "drug_targets_articles", save_table_only = TRUE)
    return(drug_targets_articles)
}


parse_drug_carriers_articles <- function() {
    drug_carriers_articles <- map_df(children, ~get_carriers_articles_df(.x))
    save_drug_sub(con = con, df = drug_carriers_articles, table_name = "drug_carriers_articles", save_table_only = TRUE)
    return(drug_carriers_articles)
}

parse_drug_transporters_articles <- function() {
    drug_transporters_articles <- map_df(children, ~get_transporters_articles_df(.x))
    save_drug_sub(con = con, df = drug_transporters_articles, table_name = "drug_transporters_articles", save_table_only = TRUE)
    return(drug_transporters_articles)
}

parse_drug_targets_textbooks <- function() {
    drug_targets_textbooks <- map_df(children, ~get_targets_textbooks_df(.x))
    save_drug_sub(con = con, df = drug_targets_textbooks, table_name = "drug_carriers_textbooks", save_table_only = TRUE)
    return(drug_targets_textbooks)
}

parse_drug_carriers_textbooks <- function() {
    drug_carriers_textbooks <- map_df(children, ~get_carriers_textbooks_df(.x))
    save_drug_sub(con = con, df = drug_carriers_textbooks, table_name = "drug_carriers_textbooks", save_table_only = TRUE)
    return(drug_carriers_textbooks)
}

parse_drug_transporters_textbooks <- function() {
    drug_transporters_textbooks <- map_df(children, ~get_transporters_textbooks_df(.x))
    save_drug_sub(con = con, df = drug_transporters_textbooks, table_name = "drug_transporters_textbooks", save_table_only = TRUE)
    return(drug_transporters_textbooks)
}

parse_drug_enzymes_textbooks <- function() {
    drug_enzymes_textbooks <- map_df(children, ~get_enzymes_textbooks_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_textbooks, table_name = "drug_enzymes_textbooks", save_table_only = TRUE)
    return(drug_enzymes_textbooks)
}

parse_drug_enzymes_links <- function() {
    drug_enzymes_links <- map_df(children, ~get_enzymes_links_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_links, table_name = "drug_enzymes_links", save_table_only = TRUE)
    return(drug_enzymes_links)
}

parse_drug_targets_links <- function() {
    drug_targets_links <- map_df(children, ~get_targets_links_df(.x))
    save_drug_sub(con = con, df = drug_targets_links, table_name = "drug_targets_links", save_table_only = TRUE, field.types = list(title = paste("varchar(", 
        max(nchar(drug_targets_links$title)) + 100, ")", sep = ""), url = paste("varchar(", max(nchar(drug_targets_links$url)) + 
        100, ")", sep = "")))
    return(drug_targets_links)
}

parse_drug_carriers_links <- function() {
    drug_carriers_links <- map_df(children, ~get_carriers_links_df(.x))
    save_drug_sub(con = con, df = drug_carriers_links, table_name = "drug_carriers_links", save_table_only = TRUE)
    return(drug_carriers_links)
}

parse_drug_transporters_links <- function() {
    drug_transporters_links <- map_df(children, ~get_transporters_links_df(.x))
    save_drug_sub(con = con, df = drug_transporters_links, table_name = "drug_transporters_links", save_table_only = TRUE)
    return(drug_transporters_links)
}

parse_drug_enzymes_polypeptides <- function() {
    drug_enzymes_polypeptides <- map_df(children, ~get_enzymes_polypeptide_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_polypeptides, table_name = "drug_enzymes_polypeptides", save_table_only = TRUE, 
        field.types = list(general_function = paste("varchar(", max(nchar(drug_enzymes_polypeptides$general_function)), ")", 
            sep = ""), specific_function = paste("varchar(", max(nchar(drug_enzymes_polypeptides$specific_function)), ")", 
            sep = ""), amino_acid_sequence = paste("varchar(", max(nchar(drug_enzymes_polypeptides$amino_acid_sequence)), 
            ")", sep = ""), gene_sequence = paste("varchar(", max(nchar(drug_enzymes_polypeptides$gene_sequence)), ")", sep = "")))
    return(drug_enzymes_polypeptides)
}

parse_drug_targets_polypeptides <- function() {
    drug_targets_polypeptides <- map_df(children, ~get_targets_polypeptide_df(.x))
    save_drug_sub(con = con, df = drug_targets_polypeptides, table_name = "drug_targets_polypeptides", save_table_only = TRUE, 
        field.types = list(general_function = paste("varchar(", max(nchar(drug_targets_polypeptides$general_function)), ")", 
            sep = ""), specific_function = paste("varchar(max)", sep = ""), amino_acid_sequence = paste("varchar(max)", sep = ""), 
            gene_sequence = paste("varchar(max)", sep = "")))
    return(drug_targets_polypeptides)
}

parse_drug_carriers_polypeptides <- function() {
    drug_carriers_polypeptides <- map_df(children, ~get_carriers_polypeptide_df(.x))
    save_drug_sub(con = con, df = drug_carriers_polypeptides, table_name = "drug_carriers_polypeptides", save_table_only = TRUE, 
        field.types = list(general_function = paste("varchar(", max(nchar(drug_carriers_polypeptides$general_function)), ")", 
            sep = ""), specific_function = paste("varchar(", max(nchar(drug_carriers_polypeptides$specific_function)), ")", 
            sep = ""), amino_acid_sequence = paste("varchar(", max(nchar(drug_carriers_polypeptides$amino_acid_sequence)), 
            ")", sep = ""), gene_sequence = paste("varchar(max)", sep = "")))
    return(drug_carriers_polypeptides)
}

parse_drug_transporters_polypeptides <- function() {
    drug_transporters_polypeptides <- map_df(children, ~get_transporters_polypeptide_df(.x))
    save_drug_sub(con = con, df = drug_transporters_polypeptides, table_name = "drug_transporters_polypeptides", save_table_only = TRUE, 
        field.types = list(general_function = paste("varchar(", max(nchar(drug_transporters_polypeptides$general_function)), 
            ")", sep = ""), specific_function = paste("varchar(", max(nchar(drug_transporters_polypeptides$specific_function)), 
            ")", sep = ""), amino_acid_sequence = paste("varchar(", max(nchar(drug_transporters_polypeptides$amino_acid_sequence)), 
            ")", sep = ""), gene_sequence = paste("varchar(max)", sep = "")))
    return(drug_transporters_polypeptides)
}


parse_drug_enzymes_polypeptides_external_identifiers <- function() {
    drug_enzymes_polypeptide_external_identifiers <- map_df(children, ~get_enzymes_polypeptide_external_identifiers_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_polypeptide_external_identifiers, table_name = "drug_enzymes_polypeptides_external_identifiers", 
        save_table_only = TRUE)
    return(drug_enzymes_polypeptide_external_identifiers)
}

parse_drug_targets_polypeptides_external_identifiers <- function() {
    drug_targets_polypeptide_external_identifiers <- map_df(children, ~get_targets_polypeptide_external_identifiers_df(.x))
    save_drug_sub(con = con, df = drug_targets_polypeptide_external_identifiers, table_name = "drug_targets_polypeptides_external_identifiers", 
        save_table_only = TRUE)
    return(drug_targets_polypeptide_external_identifiers)
}

parse_drug_carriers_polypeptides_external_identifiers <- function() {
    drug_carriers_polypeptide_external_identifiers <- map_df(children, ~get_carriers_polypeptide_external_identifiers_df(.x))
    save_drug_sub(con = con, df = drug_carriers_polypeptide_external_identifiers, table_name = "drug_carriers_polypeptides_external_identifiers", 
        save_table_only = TRUE)
    return(drug_carriers_polypeptide_external_identifiers)
}

parse_drug_transporters_polypeptides_external_identifiers <- function() {
    drug_transporters_polypeptide_external_identifiers <- map_df(children, ~get_transporters_polypeptide_external_identifiers_df(.x))
    save_drug_sub(con = con, df = drug_transporters_polypeptide_external_identifiers, table_name = "drug_transporters_polypeptides_external_identifiers", 
        save_table_only = TRUE)
    return(drug_transporters_polypeptide_external_identifiers)
}

parse_drug_enzymes_polypeptides_synonyms <- function() {
    drug_enzymes_polypeptide_synonyms <- map_df(children, ~get_enzymes_polypeptide_synonyms_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_polypeptide_synonyms, table_name = "drug_enzymes_polypeptides_synonyms", save_table_only = TRUE)
    return(drug_enzymes_polypeptide_synonyms)
}

parse_drug_targets_polypeptides_synonyms <- function() {
    drug_targets_polypeptide_synonyms <- map_df(children, ~get_targets_polypeptide_synonyms_df(.x))
    save_drug_sub(con = con, df = drug_targets_polypeptide_synonyms, table_name = "drug_targets_polypeptides_synonyms", save_table_only = TRUE)
    return(drug_targets_polypeptide_synonyms)
}

parse_drug_carriers_polypeptides_synonyms <- function() {
    drug_carriers_polypeptide_synonyms <- map_df(children, ~get_carriers_polypeptide_synonyms_df(.x))
    save_drug_sub(con = con, df = drug_carriers_polypeptide_synonyms, table_name = "drug_carriers_polypeptides_synonyms", 
        save_table_only = TRUE)
    return(drug_carriers_polypeptide_synonyms)
}

parse_drug_transporters_polypeptides_synonyms <- function() {
    drug_transporter_polypeptide_synonyms <- map_df(children, ~get_transporters_polypeptide_synonyms_df(.x))
    save_drug_sub(con = con, df = drug_transporter_polypeptide_synonyms, table_name = "drug_transporters_polypeptides_synonyms", 
        save_table_only = TRUE)
    return(drug_transporter_polypeptide_synonyms)
}

parse_drug_enzymes_polypeptides_pfams <- function() {
    drug_enzymes_polypeptide_pfams <- map_df(children, ~get_enzymes_polypeptide_pfams_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_polypeptide_pfams, table_name = "drug_enzymes_polypeptides_pfams", save_table_only = TRUE)
    return(drug_enzymes_polypeptide_pfams)
}

parse_drug_targets_polypeptides_pfams <- function() {
    drug_targets_polypeptide_pfams <- map_df(children, ~get_targets_polypeptide_pfams_df(.x))
    save_drug_sub(con = con, df = drug_targets_polypeptide_pfams, table_name = "drug_targets_polypeptides_pfams", save_table_only = TRUE)
    return(drug_targets_polypeptide_pfams)
}

parse_drug_carriers_polypeptides_pfams <- function() {
    drug_carriers_polypeptide_pfams <- map_df(children, ~get_carriers_polypeptide_pfams_df(.x))
    save_drug_sub(con = con, df = drug_carriers_polypeptide_pfams, table_name = "drug_carriers_polypeptide_pfams", save_table_only = TRUE)
    return(drug_carriers_polypeptide_pfams)
}

parse_drug_transporters_polypeptides_pfams <- function() {
    drug_transporters_polypeptides_pfams <- map_df(children, ~get_transporters_polypeptide_pfams_df(.x))
    save_drug_sub(con = con, df = drug_transporters_polypeptides_pfams, table_name = "drug_transporters_polypeptides_pfams", 
        save_table_only = TRUE)
    return(drug_transporters_polypeptides_pfams)
}

parse_drug_enzymes_polypeptides_go_classifiers <- function() {
    drug_enzymes_polypeptides_go_classifiers <- map_df(children, ~get_enzymes_polypeptide_go_classifiers_df(.x))
    save_drug_sub(con = con, df = drug_enzymes_polypeptides_go_classifiers, table_name = "drug_enzymes_polypeptides_go_classifiers", 
        save_table_only = TRUE)
    return(drug_enzymes_polypeptides_go_classifiers)
}

parse_drug_targets_polypeptides_go_classifiers <- function() {
    drug_targets_polypeptides_go_classifiers <- map_df(children, ~get_targets_polypeptide_go_classifiers_df(.x))
    save_drug_sub(con = con, df = drug_targets_polypeptides_go_classifiers, table_name = "drug_targets_polypeptides_go_classifiers", 
        save_table_only = TRUE)
    return(drug_targets_polypeptides_go_classifiers)
}

parse_drug_carriers_polypeptides_go_classifiers <- function() {
    drug_carriers_polypeptides_go_classifiers <- map_df(children, ~get_carriers_polypeptide_go_classifiers_df(.x))
    save_drug_sub(con = con, df = drug_carriers_polypeptides_go_classifiers, table_name = "drug_carriers_polypeptides_go_classifiers", 
        save_table_only = TRUE)
    return(drug_carriers_polypeptides_go_classifiers)
}

parse_drug_transporters_polypeptides_go_classifiers <- function() {
    drug_transporters_polypeptides_go_classifiers <- map_df(children, ~get_transporters_polypeptide_go_classifiers_df(.x))
    save_drug_sub(con = con, df = drug_transporters_polypeptides_go_classifiers, table_name = "drug_transporters_polypeptides_go_classifiers", 
        save_table_only = TRUE)
    return(drug_transporters_polypeptides_go_classifiers)
}

parse_drug_reactions <- function() {
    drug_reactions <- map_df(children, ~get_reactions_df(.x))
    save_drug_sub(con = con, df = drug_reactions, table_name = "drug_reactions", foreign_key = "drug_key")
    return(drug_reactions)
}

parse_drug_reactions_enzymes <- function() {
    drug_reactions_enzymes <- map_df(children, ~get_reactions_enzymes_df(.x))
    save_drug_sub(con = con, df = drug_reactions_enzymes, table_name = "drug_reactions_enzymes", save_table_only = TRUE)
    return(drug_reactions_enzymes)
}

parse_drug_carriers <- function() {
    drug_carriers <- map_df(children, ~get_carriers_df(.x))
    save_drug_sub(con = con, df = drug_carriers, table_name = "drug_carriers", foreign_key = "drug_key")
    return(drug_carriers)
}

parse_drug_transporters <- function() {
    drug_transporters <- map_df(children, ~get_transporters_df(.x))
    save_drug_sub(con = con, df = drug_transporters, table_name = "drug_transporters", foreign_key = "drug_key")
    return(drug_transporters)
}

parse_drug_targets <- function() {
    drug_targets <- map_df(children, ~get_targets_df(.x))
    save_drug_sub(con = con, df = drug_targets, table_name = "drug_targets", foreign_key = "drug_key")
    return(drug_targets)
}
