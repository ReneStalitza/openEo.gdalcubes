#' SessionConfig class
#' @description Session configuration
#'
#' @export
SessionConfig = function() {

  api.port = 8000
  host = "127.0.0.1"

  default = list(
    api_version = "1.0.0",
    backend_version = "0.1.0",
    stac_version = "0.9.0",
    stac_extensions = "datacube",

    id = "openeo-gdalcubes-R-driver",
    title = "gdalcubes",
    description = "This is an openEo-compliant R driver interfacing with the package gdalcubes",

    links = NULL,

    data.path = NULL,
    workspace.path = NULL,

    api.port = api.port,
    host = host,
    base_url = paste("http://",host, ":", api.port, sep = ""),

    outputFormats = list(
      GTiff = list(
        title = "GeoTiff",
        description = "Export to GeoTiff",
        gis_data_types = list(
          "raster"
        )
      ),
      NetCDF = list(
        title = "Network Common Data Form",
        description = "Export to NetCDF",
        gis_data_types = list(
          "raster"
        )
      )
    ),
    inputFormats = NULL,

    OGC_conformanceLink = "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core")

  class(default) = "ServerConfig"
  return(default)
}
