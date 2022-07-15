#' Get a table from the database
#'
#' Name the table you want, and get it.
#'
#' @param shinyDir The directory of the shiny app.
#' @param table_name the \emph{descriptive name} of the table. See Details.
#'
#' @details
#'
#' \code{table_name} should have the following values:
#'
#' \describe{
#'   \item{general_spatial_data:}{Retrieves a table of catches by lon, lat and species. The database name for this table is \code{aux_summ_time_flag} (because data is summarized over time and flag.)}
#' }
#'
#' @export
GetTable <- function(shinyDir, table_name)
{
  .NotYetImplemented()
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
