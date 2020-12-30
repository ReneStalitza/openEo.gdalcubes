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
schema_format = function(type, subtype = NULL, items = NULL) {
  schema = list()
  schema = append(schema,list(type=type))

  if (!is.null(subtype) && !is.na(subtype)) {
    schema = append(schema, list(subtype = subtype))
  }
  if (!is.null(items) && !is.na(items)) {
    schema = append(schema, list(items = items))
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
    schema = list(type = "object", subtype = "raster-cube")
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
      schema = list(
        type = "string",
        subtype = "collection-id")
    ),
    Parameter$new(
      name = "spatial_extent",
      description = "Limits the data to load from the collection to the specified bounding box",
      schema = list(
        type = "object",
        subtype = "bounding-box")
    ),
    Parameter$new(
      name = "temporal_extent",
      description = "Limits the data to load from the collection to the specified left-closed temporal interval.",
      schema = list(
        type = "array",
        subtype = "temporal-interval")
    ),
    Parameter$new(
      name = "bands",
      description = "Only adds the specified bands into the data cube so that bands that don't match the list of band names are not available.",
      schema = list(
        type = "array"),
      optional = TRUE
    )

  ),
  returns = eo_datacube,
  operation = function(id, spatial_extent, temporal_extent, bands = NULL) {

    ic = Session$data[[id]]$getCollection()
    #ic = image_collection("C:/Users/ReneS/Documents/R/BSc/openEo.gdalcubes/data/L8.db")

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
      schema = list(
        type = "object",
        subtype = "raster-cube")
    ),
    Parameter$new(
      name = "format",
      description = "The file format to save to.",
      schema = list(
        type = "string",
        subtype = "output-format")
    ),
    Parameter$new(
      name = "options",
      description = "The file format parameters to be used to create the file(s).",
      schema = list(
        type = "object",
        subtype = "output-format-options")
    )
  ),
  returns = list(
    description = "false if saving failed, true otherwise.",
    schema = list(type = "boolean")
  ),
  operation = function(data, format, options = NULL) {

    parent.frame()$job$setOutput(format)

    return(data)
  }
)

#' filter bands
filter_bands = Process$new(
  id = "filter_bands",
  description = "Filters the bands in the data cube so that bands that don't match any of the criteria are dropped from the data cube.",
  categories = list("cubes", "filter"),
  summary = "Filter the bands by name",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A data cube with bands.",
      schema = list(
        type = "object",
        subtype = "raster-cube")
    ),
    Parameter$new(
      name = "bands",
      description = "A list of band names.",
      schema = list(
        type = "array")
    )
  ),
  returns = eo_datacube,
  operation = function(data, bands) {

    if(! is.null(bands)) {
      cube = select_bands(data, bands)
    }
    return(cube)
  }
)

#' filter bbox
filter_bbox = Process$new(
  id = "filter_bbox",
  description = "The filter retains a pixel in the data cube if the point at the pixel center intersects with the bounding box (as defined in the Simple Features standard by the OGC).",
  categories = list("cubes", "filter"),
  summary = "Limits the data cube to the specified bounding box.",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A data cube.",
      schema = list(
        type = "object",
        subtype = "raster-cube")
    ),
    Parameter$new(
      name = "extent",
      description = "A bounding box, which may include a vertical axis (see base and height).",
      schema = list(
        type = "object",
        subtype = "bounding-box")
    )
  ),
  returns = eo_datacube,
  operation = function(data, extent) {

    if (! is.null(extent$crs)) {
      crsString = toString(extent$crs)
    }
    else {
      crsString = "3857"
    }

    crs = paste("EPSG", crsString, sep = ":")

    extent = changeProjection(extent)
    nw = c(extent$west, extent$north)
    sw = c(extent$west, extent$south)
    se = c(extent$east, extent$south)
    ne = c(extent$east, extent$north)

    p = list(rbind(nw, sw, se, ne, nw))
    pol = sf::st_polygon(p)

    cube = filter_geom(data, pol, srs = crs)

    return(cube)
  }
)

#' reduce dimension
reduce_dimension = Process$new(
  id = "reduce_dimension",
  description = "Applies a unary reducer to a data cube dimension by collapsing all the pixel values along the specified dimension into an output value computed by the reducer. ",
  categories = list("cubes", "reducer"),
  summary = "Reduce dimensions",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A data cube with bands.",
      schema = list(
        type = "object",
        subtype = "raster-cube")
    ),
    Parameter$new(
      name = "reducer",
      description = "A reducer to apply on the specified dimension.",
      schema = list(
        type = "object",
        subtype = "process-graph",
        parameters = list(
          Parameter$new(
            name = "data",
            description = "A labeled array with elements of any type.",
            schema = list(
              type = "array",
              subtype = "labeled-array",
              items = list(description = "Any data type"))
          ),
          Parameter$new(
            name = "context",
            description = "Additional data passed by the user.",
            schema = list(
              type = "any",
              description = "Any data type")
          )
        )
      )
    ),
    Parameter$new(
      name = "dimension",
      description = "The name of the dimension over which to reduce.",
      schema = list(
        type = "string")
    ),
    Parameter$new(
      name = "context",
      description = "Additional data to be passed to the reducer.",
      schema = list(
        type = "any",
        description = "Any data type")
    )
  ),
  returns = eo_datacube,
  operation = function(data, reducer, dimension) {

    if(dimension == "t" || dimension == "time") {

      bands = bands(data)$name
      bandStr = c()

      for (i in 1:length(bands)) {
        bandStr = append(bandStr, sprintf("%s(%s)", reducer, bands[i]))
      }

      cube = reduce_time(data, bandStr)
      return(cube)
    }
    else {
      stop('Please select "t" or "time" as dimension')
    }
  }
)


#' apply
apply = Process$new(
  id = "apply",
  description = "Applies a unary process to each pixel value in the data cube (i.e. a local operation). ",
  categories = "cubes",
  summary = "Apply a process to each pixel",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "A data cube.",
      schema = list(
        type = "object",
        subtype = "raster-cube")
    ),
    Parameter$new(
      name = "process",
      description = "A unary process to be applied on each value, may consist of multiple sub-processes.",
      schema = list(
        type = "object",
        subtype = "process-graph",
        parameters = list(
          Parameter$new(
            name = "data",
            description = "A labeled array with elements of any type.",
            schema = list(
              type = "array",
              subtype = "labeled-array",
              items = list(description = "Any data type"))
          ),
          Parameter$new(
            name = "context",
            description = "Additional data passed by the user.",
            schema = list(
              type = "any",
              description = "Any data type")
          )
        )
      )
    ),
    Parameter$new(
      name = "context",
      description = "Additional data to be passed to the process.",
      schema = list(
        type = "any",
        description = "Any data type")
    )
  ),
  returns = eo_datacube,
  operation = function(data, process, context) {

  if (!is.null(context) && class(context) == "character") {
    cube = apply_pixel(data, process, names = context, keep_bands = FALSE)
  }
  else {
    cube = apply_pixel(data, process, keep_bands = FALSE)
  }

  return(cube)
  }
)

#'array_element
array_element = Process$new(
  id = "array_element",
  description = "Returns the element with the specified index or label from the array.",
  categories = list("arrays", "reducer"),
  summary = "Get an element from an array",
  parameters = list(
    Parameter$new(
      name = "data",
      description = "An array",
      schema = list(type = "array")
    ),
    Parameter$new(
      name = "index",
      description = "The zero-based index of the element to retrieve.",
      schema = list(type ="integer")
    ),
    Parameter$new(
      name = "label",
      description = "The label of the element to retrieve.",
      schema = list(type =c("number", "string"))
    ),
    Parameter$new(
      name = "return_nodata",
      description = "By default this process throws an ArrayElementNotAvailable exception if the index or label is invalid. If you want to return null instead, set this flag to true.",
      schema = list(type ="boolean")
    )
  ),
  returns = list(
    description = "The value of the requested element.",
    schema = list(type = "any")),
  operation = function(data, index = NULL, label = NULL, return_nodata = FALSE) {

    if (class(data) == "list") {
      band = bands(data$data)$name[index + 1]
    }
    else {
      band = bands(data)$name[index + 1]
    }
    return(band)
  }
)
