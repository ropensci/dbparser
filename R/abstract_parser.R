AbstractParser <- R6::R6Class(
    "AbstractParser",
    public = list(
      initialize = function(tibble_name = NULL,
                            object_node = NULL,
                            main_node = NULL,
                            secondary_node = NULL,
                            id = NULL) {
        stopifnot("Please make sure to call read_drugbank_xml_db method first" =
                    !is.null(pkg_env$root))
        private$tibble_name  <- tibble_name
        private$object_node <- object_node
        private$main_node <- main_node
        private$secondary_node <- secondary_node
        private$id <- id

      },
      parse = function() {
        parsed_tbl <- private$parse_record()
        as_tibble(parsed_tbl)
      }
    ),
    private = list(
      tibble_name = NULL,
      object_node = NULL,
      main_node = NULL,
      secondary_node = NULL,
      id = NULL,
      parse_record = function() {
        message("I am the abstract parser, please use proper parser")
      }
    )
  )
