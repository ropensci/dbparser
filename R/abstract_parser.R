AbstractParser <-
  R6Class(
    "AbstractParser",
    public = list(
      initialize = function(save_table = FALSE,
                            save_csv = FALSE,
                            csv_path = ".",
                            override_csv = FALSE,
                            database_connection = NULL,
                            tibble_name = NULL,
                            ref_title = NULL,
                            ref_type = NULL,
                            id = NULL) {
        stopifnot(
          paste(
            "Please provide a valid database connection",
            "or disable save_table feature."
          ) = !(save_table && is.null(database_connection))
        )
        stopifnot("Please make sure to call read_drugbank_xml_db method first" =
                    !is.null(pkg_env$root))

        private$save_table  <- save_table
        private$save_csv <- save_csv
        private$csv_path <- csv_path
        private$override_csv <- override_csv
        private$database_connection <- database_connection
        private$tibble_name  <- tibble_name
        private$ref_title <- ref_title
        private$ref_type <- ref_type
        private$id <- id

      },
      parse = function() {
        path <- get_dataset_full_path(tibble_name, csv_path)
        if (!override_csv & file.exists(path)) {
          message(
            paste(
              "An existing csv is found and override_csv is not enabled.\n",
              "No parsing is done."
            )
          )
          return(readr::read_csv(path))
        }
        parsed_tbl <- private$parse_record()
        private$save_file(parsed_tbl)
        private$save_table(parsed_tbl)
        return(parsed_tbl)
      }
    ),
    private = list(
      save_table = FALSE,
      save_csv = FALSE,
      csv_path = ".",
      override_csv = FALSE,
      database_connection = NULL,
      tibble_name = NULL,
      ref_title = NULL,
      ref_type = NULL,
      id = NULL,
      parse_record = function(rec = xmlChildren(pkg_env$root),
                              main_node = NULL,
                              seconadary_node = NULL,
                              id = "drugbank-id",
                              attr_nodes = NULL) {
        message("I am the abstract parser, please use proper parser")
      },
      save_file = function(parsed_tbl) {
        parsed_tbl <- parsed_tbl %>% unique()
        write_csv(parsed_tbl, save_csv, csv_path)
      },
      save_table = function(parsed_tbl) {
        if (save_table) {
          save_drug_sub(con = database_connection,
                        df = references_tbl,
                        table_name = tibble_name)
        }
      }
    )
  )
