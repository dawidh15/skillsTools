#' Checks if database exists.
#'
#' The demo shiny app relies on a SQLite database. This function checks whether this database exists or not
#'
#' These are further details.
#'
#' @section A Custom Section:
#'
#' Text accompanying the custom section.
#'
#' @param shinyDir The path of the shiny App directory.
#'
#' @return TRUE if database exists
#'
#' @export
CheckDBExists <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  value <- RSQLite::dbCanConnect(RSQLite::SQLite(), all_path)
  return(value)
}



#' Creates a database for the shiny app.
#'
#' The demo shiny app relies on a SQLite database. This function creates an empty database.
#'
#'
#' @param shinyDir The path of the shiny App directory where the database is to be created.
#'
#' @returns
#' Creates a data base in the \code{shinyDir} directory. Throws an error if it fails.
#'
#' @export
CreateDB <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  con <- DBI::dbConnect(RSQLite::SQLite(), all_path) #creates a DB
  DBI::dbDisconnect(con)

  stopifnot(DBI::dbCanConnect(RSQLite::SQLite(), all_path))

}



#' Checks the existence of Longline Data Table.
#'
#' Checks if the data containing longline data for tuna and billfish exists. This table contains data on longline fishing, with a grid of 5 degree spatial resolution, on a monthly base splitted by country.
#'
#'
#' @param shinyDir The path of the shiny App directory where the database is to be created.
#'
#' @returns
#' Creates a data base in the \code{shinyDir} directory. Throws an error if it fails.
#'
#' @export
CheckTblLLTunaBillfish <- function(shinyDir)
{
  tbl_name <- skillsEnv$LLTunaBilltbl
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)
  tryCatch({
    tables <- DBI::dbListTables(con)
    val <- dbExistsTable(conn = con, name = tbl_name)

    if (val){
      val <- length(DBI::dbListFields(con, tbl_name)) > 0
    }

  }, finally = DBI::dbDisconnect(con))

  return(val)
}
