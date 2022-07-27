#' Get a table from the database
#'
#' Name the table you want, and get it.
#'
#' @param shinyDir The directory of the shiny app.
#' @param table_name the \emph{descriptive name} of the table. See Details.
#'
#' @details
#'
#' \code{table_name} should have the following character values; otherwise returns an empty data frame:
#'
#' \describe{
#'   \item{General Spatial Data:}{Retrieves a table of catches by lon, lat and species. The database name for this table is \code{aux_summ_time_flag} (because data is summarized over time and flag.)}
#'   \item{Catch Time Series By Species:}{Retrieves a table with year, species and total weight.}
#'   \item{Proportional Catches by Period-Flag:}{Contains catches scaled by the maximum catch within a time period. Also, contains the min and max years that composed a period for each flag.}
#'   \item{Catch Time Series By Flag}{Retrieves a table with year, flag and total weight}
#'   \item{Catch By Space-Period-Flag}{Retrieves a table with coordinates, period, flag, and total weight.}
#' }

#'
#' @export
GetTable <- function(shinyDir = ".", table_name)
{

  retrieved_data <-
    switch (table_name,
      "General Spatial Data" = get_general_spatial_data(shinyDir),
      "Catch Time Series By Species" = get_ts_by_spp(shinyDir),
      "Proportional Catches by Period-Flag" = get_scaled_catches_period(shinyDir),
      "Catch Time Series By Flag" = get_ts_by_flag(shinyDir),
      "Catch By Space-Period-Flag" = get_sp_flag_period(shinyDir),
      data.frame() #default
    )
  return(retrieved_data)
}


# Internal query executers

## For map of historical catches
get_general_spatial_data <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  con <- dbConnect(RSQLite::SQLite(), all_path)
  tryCatch({
    val <- dbReadTable(con, "aux_summ_time_flag")
  }, finally = dbDisconnect(con)
  )
  return(val)
}


## Data for catch time series by spp

get_ts_by_spp <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  qry_time <-
    "SELECT Year, Species, SUM(`Weight (mt)`) AS Weight
      FROM LLTunaBillfish
      GROUP BY Year,  Species
      UNION
      SELECT Year, 'ALL' AS Species, SUM(`Weight (mt)`) AS Weight
      FROM LLTunaBillfish
      GROUP BY Year;"
  con <- dbConnect(RSQLite::SQLite(), all_path)
  tryCatch({
    val <- dbGetQuery(con, qry_time)
  }, finally = dbDisconnect(con)
  )
  return(val)
}


# Data for catch time series by flag
get_ts_by_flag <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  qry_time <-
    "SELECT Year, Flag, SUM(`Weight (mt)`) AS Weight
      FROM LLTunaBillfish
      GROUP BY Year,  Flag;"

  con <- dbConnect(RSQLite::SQLite(), all_path)
  tryCatch({
    val <- dbGetQuery(con, qry_time)
  }, finally = dbDisconnect(con)
  )
  return(val)
}


# Data of scaled catches by time period-flag-spp
get_scaled_catches_period <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))
  con <- dbConnect(RSQLite::SQLite(), all_path)
  tryCatch({
    val <- dbReadTable(con, "aux_prop_catch_period")
  }, finally = dbDisconnect(con)
  )
  return(val)
}

#' Get spp names
#'
#' Get a character vector with the abbreviations for the species, plus a class 'ALL'.
#'
#' Use this function to generate a list of valid choices of species that are contained in the database.
#'
#' @param shinyDir The directory of the shiny app.
#'
#'
#' @export
GetSppNames <- function(shinyDir)
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)
  val <- "Find technical support."
  tryCatch({
    if (DBI::dbExistsTable(con, "aux_spp_list"))
    {
      val <- DBI::dbGetQuery(con,
              "SELECT Species FROM aux_spp_list ORDER BY `Importance`")
    }
  }, finally = DBI::dbDisconnect(con))
  return(val)
}


#' Get Flag names
#'
#' Get a character vector with the abbreviations for the flags of the operating vessels.
#'
#' Use this function to generate a list of valid choices of flags that are contained in the database.
#'
#' @param shinyDir The directory of the shiny app.
#'
#'
#' @export
GetFlagNames <- function(shinyDir = ".")
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)
  val <- "Find technical support."
  tryCatch({
    if (DBI::dbExistsTable(con, "aux_flag_list"))
    {
      val <- DBI::dbGetQuery(con,
                             "SELECT Flag FROM aux_flag_list ORDER BY `Importance`")
    }
  }, finally = DBI::dbDisconnect(con))
  return(val)
}


#' Get a valid time period
#'
#' Get a integer vector of time periods that are contained in the database.
#'
#' @param shinyDir The directory of the shiny app.
#'
#'
#' @export
GetTimePeriod <- function(shinyDir = ".")
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)
  val <- "Find technical support."
  tryCatch({
    if (DBI::dbExistsTable(con, "aux_flag_list"))
    {
      val <- DBI::dbGetQuery(con, "SELECT DISTINCT Period FROM aux_prop_catch_period;")
      val <- as.integer(pull(val,Period))
    }
  }, finally = DBI::dbDisconnect(con))
  return(val)
}



# Retrieve auxiliary table of catches by coordinates, period and flag
get_sp_flag_period <- function(shinyDir = ".")
{
  all_path <- fs::path_join(c(shinyDir, skillsEnv$dbname))

  con <- DBI::dbConnect(RSQLite::SQLite(), all_path)
  val <-
  tryCatch({
    if (DBI::dbExistsTable(con, "aux_sp_flag_period"))
      DBI::dbReadTable(con, "aux_sp_flag_period")
    else
      "Find technical support."
  }, finally = DBI::dbDisconnect(con))
  return(val)
}
