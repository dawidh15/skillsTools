skillsEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname)
{
  skillsEnv$dbname <- "iattc_pd.sqlite3"
  skillsEnv$LLTunaBilltbl <- "LLTunaBillfish"

  # Prepare map layout
  shp_path <- fs::path_dir(system.file("extdata/gis","AmericaBasicShoreline.shp", package = "skillsTools"))
  coastSHP <- rgdal::readOGR(shp_path, "AmericaBasicShoreline",verbose = FALSE)

  shoreline <- list("sp.polygons",
                    coastSHP,
                    col="gray50",
                    fill="gray10",
                    first=FALSE )

  skillsEnv$mapLayout <- list(shoreline)

  # Prepare Box for mapping
  skillsEnv$iattc_Box <- matrix(c(-150,-50,-60,50), ncol=2,
                      dimnames = list(NULL,c("min", "max")))
  skillsEnv$dd_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

  skillsEnv$iattc_spBox <- sp::Spatial(bbox = skillsEnv$iattc_Box, proj4string = skillsEnv$dd_crs)


}
