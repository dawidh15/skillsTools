#' Maps all catches by species
#'
#' Gives a general spatial description of long-line fishing.
#'
#' @param catchData the reactive dataset filtered by the user.
#'
#' @export
MapCatchesBySpp <- function(catchData)
{
  skillsEnv$mapLayout


  spPoints <- SpatialPoints(
    coords = select(catchData, Lon, Lat) # A data frame of coordinates
    , proj4string = skillsEnv$dd_crs # Projection
    , bbox = skillsEnv$iattc_Box # surrounding box
  )

  # Creates a raster
  spPixDataFrame <- SpatialPixelsDataFrame(points = spPoints,
                                           data = catchData,
                                           proj4string = skillsEnv$dd_crs)

  return(spPixDataFrame)
}
