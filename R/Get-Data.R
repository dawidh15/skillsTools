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
