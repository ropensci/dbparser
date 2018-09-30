# Extract drug classfications df
drug_classfications_df <- function(rec) {
    if (is.null(rec[["classification"]])) 
        return()
    a <- xmlToList(rec[["classification"]])
    return(tibble(parent_key = xmlValue(rec["drugbank-id"][[1]]), classifications = paste(names(a), a, collapse = ";")))
}
