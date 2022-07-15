#' Maps all catches by species
#'
#' Gives a general spatial description of long-line fishing.
#'
#' @param catchData the reactive dataset filtered by the user.
#'
#' @export
MapCatchesBySpp <- function(catchData)
{
  spPoints <- SpatialPoints(
    coords = select(catchData, Lon, Lat) # A data frame of coordinates
    , proj4string = skillsEnv$dd_crs # Projection
    , bbox = skillsEnv$iattc_Box # surrounding box
  )

  # Creates a raster
  spPixDataFrame <-
    SpatialPixelsDataFrame(points = spPoints, data = catchData, proj4string = skillsEnv$dd_crs)

  spplot(
    spPixDataFrame
    , zcol = "Weight",
    colorkey = list(
      col= function(x)rev(gray.colors(x))
    ),
    cuts=10,
    col.regions= function(x)rev(gray.colors(x)),
    sp.layout= skillsTools:::skillsEnv$mapLayout,
    scales = list(draw = TRUE),
    pretty=TRUE
  )
}
