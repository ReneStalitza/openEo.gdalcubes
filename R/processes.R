#' @include Process-class.R
#' @import gdalcubes
NULL

#' schema_format
#' @description format for the schema
#'
#' @param type data type
#' @param subtype subtype of the data
#'
#' @return list with type and subtype(optional)
#' @export
schema_format = function(type, subtype = NULL) {
  schema = list()
  schema = append(schema,list(type=type))

  if (!is.null(subtype) && !is.na(subtype)) {
    schema = append(schema, list(subtype = subtype))
  }
  return(schema)
}

#' datacube_schema
#' @description Return a list with datacube description and schema
#'
#' @return datacube list
datacube_schema = function() {
  info = list(
    description = "A data cube for further processing",
    schema = schema_format(type = "object", subtype = "raster-cube")
  )
  return(info)
}

#' return object for the processes
eo_datacube = datacube_schema()

#' changeProjection
#' @description Change from EPSG:4326 to EPSG:3857
#' @param extent bbox of spatial extent
#'
#' @return changed spatial extent
changeProjection = function(extent) {

  p1 = sf::st_point(c(extent$east,extent$north))
  p2 = sf::st_point(c(extent$west,extent$south))
  sfc = sf::st_sfc(p1, p2, crs = 4326)
  trans = sf::st_transform(sfc, 3857)
  bb = sf::st_bbox(trans)

  spatial_extent = list(west = bb$xmin[[1]], east = bb$xmax[[1]], north = bb$ymax[[1]], south = bb$ymin[[1]])
  return(spatial_extent)
}


#' load collection
load_collection = Process$new(
  id = "load_collection",
  description = "Loads a collection from the current back-end by its id and returns it as processable data cube",
  categories = list("cubes", "import"),
  summary = "Load a collection",
  parameters = list(
    Parameter$new(
      name = "id",
      description = "The collection id",
      type = "string",
      subtype = "collection-id"
    ),
    Parameter$new(
      name = "spatial_extent",
      description = "Limits the data to load from the collection to the specified bounding box",
      type = "object",
      subtype = "bounding-box"
    ),
    Parameter$new(
      name = "temporal_extent",
      description = "Limits the data to load from the collection to the specified left-closed temporal interval.",
      type = "array",
      subtype = "temporal-interval"
    ),
    Parameter$new(
      name = "bands",
      description = "Only adds the specified bands into the data cube so that bands that don't match the list of band names are not available.",
      type = "array",
      optional = TRUE
    )

  ),
  returns = eo_datacube,
  operation = function(id, spatial_extent, temporal_extent, bands) {

    ic = Session$data[[id]]$getCollection()

    if (! is.null(spatial_extent$crs)) {
      crsString = toString(spatial_extent$crs)
    }
    else {
      crsString = "3857"
    }

    crs = paste("EPSG", crsString, sep = ":")
    ex = gdalcubes::extent(ic)

    if(is.null(spatial_extent)) {
      if(is.null(temporal_extent)) {
        extent = ic
      }
      else {
        extent = list(left = ex$left, right = ex$right, top = ex$top, bottom = ex$bottom,
                      t0 = temporal_extent[[1]], t1 = temporal_extent[[2]])
      }
    }
    else {
      spatial_extent = changeProjection(spatial_extent)

      if(is.null(temporal_extent)) {
        extent = list(left = spatial_extent$west, right = spatial_extent$east,
                      top = spatial_extent$north, bottom = spatial_extent$south,
                      t0 = ex$t0, t1 = ex$t1)
      }
      else {
        extent = list(left = spatial_extent$west, right = spatial_extent$east,
                             top = spatial_extent$north, bottom = spatial_extent$south,
                             t0 = temporal_extent[[1]], t1 = temporal_extent[[2]])
      }
    }

    view = cube_view(srs = crs, extent = extent,
                     dx=500, dy=500, dt = "P1Y", resampling="average", aggregation="median")


    cube = raster_cube(ic, view)

    cube = select_bands(cube, c("B02", "B03", "B04"))

    if(! is.null(bands)) {
      cube = select_bands(cube, bands)
    }
    return(cube)
  }
)

#' save result
save_result = Process$new(
  id = "save_result",
  description = "Saves processed data to the local user workspace / data store of the authenticated user.",
  categories = list("cubes", "export"),
  summary = "Save processed data to storage",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "The data to save.",
      type = "object",
      subtype = "raster-cube"
    ),
    Parameter$new(
      name = "format",
      description = "The file format to save to.",
      type = "string",
      subtype = "output-format"
    ),
    Parameter$new(
      name = "options",
      description = "The file format parameters to be used to create the file(s).",
      type = "object",
      subtype = "output-format-options"
    )
  ),
  returns = list(
    description = "false if saving failed, true otherwise.",
    schema = schema_format(type = "boolean")
  ),
  operation = function(data, format, options = NULL) {

    parent.frame()$job$setOutput(format)

    return(data)
  }
)
