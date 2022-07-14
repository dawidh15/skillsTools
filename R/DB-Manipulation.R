#' Writes Longline Data to Database.
#'
#' Takes a data frame containing longline data for tuna and billfish and writes it to disk.
#' @param shinyDir The path of the shiny App directory.
#' @param LL_tuna_bill_table The data frame to be written on the database.
#'
#' @return TRUE if data table was written.
#'
#' @export

WriteLLTunaBilfishTbl <- function(shinyDir, LL_tuna_bill_table)
{
  tbl_name <- skillsEnv$LLTunaBilltbl
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)

  tryCatch({
    if (DBI::dbExistsTable(con, tbl_name)) DBI::dbRemoveTable(con, tbl_name)
    DBI::dbWriteTable(con, tbl_name,LL_tuna_bill_table)
    val <- DBI::dbExistsTable(con, tbl_name)
  }, finally = DBI::dbDisconnect(con))

  return(val)
}
