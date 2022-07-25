#' Creates all auxiliary tables
#'
#' For performance, there are many auxiliary tables of pre-computed queries. This function rebuild all auxiliary tables and their indexes
#'
#' @param shinyDir The path of the shiny app directory.
#'
#' @export
UpdateAuxiliaryTables <- function(shinyDir = ".")
{
  # Catches over time and flag
  aux_summOverTimeFlag(shinyDir = shinyDir)

  # Build spp name table
  aux_spp_list(shinyDir = shinyDir)

  # Build scaled catches by flag and time period
  aux_scaled_catches_period(shinyDir = shinyDir)

  # Build flag table
  aux_flag_list(shinyDir = shinyDir)

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

# Build spp list
aux_spp_list <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  sql_qry <-
  "CREATE TABLE aux_spp_list AS
  WITH spp_cumm AS(
    	SELECT Species, SUM(Count) AS `Count`
    	FROM aux_summ_time_flag
    	GROUP BY Species
    )
    SELECT spp_cumm.Species,
    	dense_rank() OVER(ORDER BY spp_cumm.Count DESC) AS `Importance`
    	FROM spp_cumm;"

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)
  tryCatch(
    {
      if (DBI::dbExistsTable(con, "aux_spp_list")) DBI::dbRemoveTable(con, "aux_spp_list")
      DBI::dbExecute(con, sql_qry)
    }, finally = DBI::dbDisconnect(con)
  )
  }


# Build flag list
aux_flag_list <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  sql_qry <-
    "CREATE TABLE aux_flag_list AS
  WITH flag_cumm AS(
    	SELECT Flag, SUM(Weight) AS `Weight`
    	FROM aux_prop_catch_period
    	GROUP BY Flag
    )
    SELECT flag_cumm.Flag,
    	dense_rank() OVER(ORDER BY flag_cumm.Weight DESC) AS `Importance`
    	FROM flag_cumm;"

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)
  tryCatch(
    {
      if (DBI::dbExistsTable(con, "aux_flag_list")) DBI::dbRemoveTable(con, "aux_flag_list")
      DBI::dbExecute(con, sql_qry)
    }, finally = DBI::dbDisconnect(con)
  )
}



# Data for Flag Page
aux_scaled_catches_period <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))


  # Creates an intermediate table
  # that will be removed when the connection ends
  sql_intermediate_tbl <-
    "CREATE TEMPORARY TABLE IF NOT EXISTS tmp_period_table AS
  WITH tbl_period AS(
  SELECT *, ntile(5) OVER(ORDER BY Year) AS `Period` --binned time
  FROM LLTunaBillfish
  )
  SELECT
  	Period, Flag, Species,
  	min(Year) AS MinYear,
  	max(Year) AS MaxYear,
  	SUM(`Weight (mt)`) AS Weight,
  	dense_rank() OVER(
  		PARTITION BY Period
  		ORDER BY SUM(`Weight (mt)`) DESC
  		) AS Ranking
  FROM tbl_period
  GROUP BY Period, Flag, Species;"

  # This step joins the table of catches by period/spp/flag
  # With a similar table that only contains the maximum values per period
  # There's a left join, like dplyr::left_join(...)
  sql_proportional_catches <-
    "CREATE TABLE aux_prop_catch_period AS
    SELECT t1.Period, t1.Flag, t1.Species, t1.MinYear, t1.MaxYear,
  	t1.Weight ,	100 * (t1.Weight/t2.Weight) AS PropMaxCatch,
  	t1.Ranking
  FROM tmp_period_table AS t1
  	LEFT JOIN (
  		SELECT * FROM tmp_period_table WHERE Ranking = 1
  		) AS t2
  	ON t1.Period = t2.Period;"

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)
  tryCatch({
    # Create intermediate table if not exists
    if (!dbExistsTable(con, "aux_prop_catch_period"))
    {
      dbExecute(con, sql_intermediate_tbl)
      dbExecute(con, sql_proportional_catches)
    }
  },
  finally = dbDisconnect(con))
}
