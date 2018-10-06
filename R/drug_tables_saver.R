save_drug_sub <-
  function(con,
           df,
           table_name,
           save_table_only = FALSE,
           field.types = NULL,
           primary_key = NULL,
           foreign_key = "parent_key",
           ref_table = "drug(primary_key)") {
    # store drug sub_Table in db
    dbWriteTable(
      conn = con,
      value = df,
      name = table_name,
      field.types = field.types,
      overwrite = TRUE
    )
    if (!save_table_only) {
      # add primary key of drug table
      if (!is.null(primary_key)) {
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
      # add foreign key of drug table
      if (!is.null(foreign_key)) {
        dbExecute(
          conn = con,
          statement = paste(
            "Alter table",
            table_name,
            "ADD CONSTRAINT",
            paste("FK_", table_name,
                  "_drug", sep = ""),
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

#' Establish connection to given data base
#'
#' \code{open_db} opens connection to given database.
#'
#' This function establishes connection to given database
#' to store, \emph{optionally}, the parsed drug bank elements.
#'
#' @param driver odbc object to define database drivr.
#' @param server string, indicated the db server name.
#' @param output_database string, the database name to be used,
#' it has to be created before using it
#' @param trusted_connection boolean, is the connection secure
#' @return sets the open connection in memory to be used by other functions
#'
#' @examples
#' open_db(xml_db_name =  "drugbank.xml", driver = "SQL Server",
#' server = "MOHAMMED\\\\SQL2016", output_database = "drugbank2")
#' @export
open_db <-
  function(driver,
           server,
           output_database,
           trusted_connection = TRUE) {
    # db connection
    con <<- dbConnect(
      odbc(),
      Driver = driver,
      Server = server,
      Database = output_database,
      Trusted_Connection = trusted_connection
    )

  }

#' Close open drug bank sql database
#'
#' \code{close_db} closes connection to pre-given database.
#'
#' This function closes connection to pre-given databas.
#'
#' @examples
#' close_db()
#' @export
close_db <- function() {
  dbDisconnect(con)
}
