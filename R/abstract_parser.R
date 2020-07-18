AbstractParser <-
  R6::R6Class(
    "AbstractParser",
    public = list(
      initialize = function(save_table = FALSE,
                            save_csv = FALSE,
                            csv_path = ".",
                            override_csv = FALSE,
                            database_connection = NULL,
                            tibble_name = NULL,
                            object_node = NULL,
                            main_node = NULL,
                            secondary_node = NULL,
                            id = NULL) {
        stopifnot(
          "save_table feature is ON, Please provide a valid database connection"
          = !(save_table && is.null(database_connection))
        )
        stopifnot("Please make sure to call read_drugbank_xml_db method first" =
                    !is.null(pkg_env$root))

        private$save_table  <- save_table
        private$save_csv <- save_csv
        private$csv_path <- csv_path
        private$override_csv <- override_csv
        private$database_connection <- database_connection
        private$tibble_name  <- tibble_name
        private$object_node <- object_node
        private$main_node <- main_node
        private$secondary_node <- secondary_node
        private$id <- id

      },
      parse = function() {
        path <- get_dataset_full_path(private$tibble_name, private$csv_path)
        if (!private$override_csv & file.exists(path)) {
          message(
            paste(
              "An existing csv is found and override_csv is not enabled.\n",
              "No parsing is done."
            )
          )
          return(readr::read_csv(path))
        }
        parsed_tbl <- private$parse_record()
        private$save_csv_file(parsed_tbl)
        private$save_db_table(parsed_tbl)
        return(as_tibble(parsed_tbl))
      }
    ),
    private = list(
      save_table = FALSE,
      save_csv = FALSE,
      csv_path = ".",
      override_csv = FALSE,
      database_connection = NULL,
      tibble_name = NULL,
      object_node = NULL,
      main_node = NULL,
      secondary_node = NULL,
      id = NULL,
      parse_record = function() {
        message("I am the abstract parser, please use proper parser")
      },
      save_csv_file = function(parsed_tbl) {
        parsed_tbl <- parsed_tbl %>% unique()
        readr::locale(encoding = "ASCII")
        write_csv(parsed_tbl, private$save_csv, private$csv_path,
                  private$tibble_name)
      },
      save_db_table = function(parsed_tbl) {
        if (private$save_table) {
          save_drug_sub(
            con = private$database_connection,
            df = parsed_tbl,
            table_name = private$tibble_name
          )
        }
      }
    )
  )
