#' Creates all auxiliary tables
#'
#' For performance, there are many auxiliary tables of pre-computed queries. This function rebuild all auxiliary tables and their indexes
#'
#' @param shinyDir The path of the shiny app directory.
#'
#' @export
UpdateAuxiliaryTables <- function(shinyDir)
{
  # Catches over time and flag
  aux_summOverTimeFlag(shinyDir = shinyDir)


  # Optimize DB
  optimizeDB(shinyDir = shinyDir)
}



# Creates a table summarized over time and flag
# General spatial data
# GetData("general_spatial_data")
aux_summOverTimeFlag <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  tbl_name <- "aux_summ_time_flag"
  sql_qry <-
    "CREATE TABLE aux_summ_time_flag AS
    SELECT
	    Lat, Lon, Species, SUM(`Count`) AS `Count`, SUM(`Weight (mt)`) AS `Weight`
    FROM LLTunaBillfish
    GROUP BY Lat, Lon, Species

  UNION

  SELECT
    Lat, Lon, 'ALL' AS Species, SUM(`Count`) AS `Count`, SUM(`Weight (mt)`) AS `Weight`
    FROM LLTunaBillfish
    GROUP BY Lat, Lon

  ;"

  idx <- "CREATE INDEX idx_aux_summ_time_flag ON aux_summ_time_flag(Species);"
  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)

  tryCatch(
    {
      if (DBI::dbExistsTable(con, tbl_name)) DBI::dbRemoveTable(con, tbl_name)
      DBI::dbExecute(con, sql_qry)
      DBI::dbExecute(con, idx)
    },
    finally = DBI::dbDisconnect(con)
  )
}



# Optimize DB after rebuuild
optimizeDB <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  optimize_DB <- "PRAGMA optimize;"
  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)

  tryCatch(
    {
      DBI::dbExecute(con, optimize_DB)
    },
    finally = DBI::dbDisconnect(con)
  )
}
