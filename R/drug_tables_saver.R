save_drug_sub <-
  function(con,
           df,
           table_name,
           save_table_only = FALSE,
           field_types = NULL,
           primary_key = NULL,
           foreign_key = NULL,
           ref_table = "drug(primary_key)") {
    if (is_empty(df)) {
      return()
    }

    if (grepl("MariaDB", class(con))) {
      field_types <- unlist(field_types[1])
    }

    # store drug sub_Table in db
    dbWriteTable(
      conn = con,
      value = df,
      name = table_name,
      field.types = field_types,
      overwrite = TRUE
    )

    if (grepl("SQLServer", class(con))) {
      for (key in primary_key) {
        dbExecute(
          conn = con,
          statement = paste(
            "Alter table",
            table_name,
            "alter column",
            key,
            "varchar(255) NOT NULL;"
          )
        )
      }

      if (!save_table_only) {
        # add primary key of drug table
        if (!is.null(primary_key)) {
          dbExecute(
            conn = con,
            statement = paste(
              "Alter table",
              table_name,
              "add primary key(",
              paste(primary_key, collapse = ","),
              ");"
            )
          )
        }
      }

      # add foreign key of drug table
      if (!is.null(foreign_key)) {
        dbExecute(
          conn = con,
          statement = paste(
            "Alter table",
            table_name,
            "ADD CONSTRAINT",
            paste("FK_", table_name,
              "_drug",
              sep = ""
            ),
            paste(
              "FOREIGN KEY (",
              foreign_key,
              ") REFERENCES",
              ref_table,
              ";"
            )
          )
        )
      }
    }
  }
