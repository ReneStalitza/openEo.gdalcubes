#' processes.R
#' @include Process-class.R


#' schema_format
#' @description format for the schema
#'
#' @param type data type
#' @param subtype subtype of the data
#'
#' @return list with type and subtype(optional)
#'
schema_format = function(type, subtype = NULL) {
  schema = list()
  schema = append(schema,list(type=type))

  if (!is.null(subtype) && !is.na(subtype)) {
    schema = append(schema, list(subtype = subtype))
  }
}

#' datacube_schema
#' @description Return a list with datacube description and schema
#'
#' @return datacube list
#'
datacube_schema = function() {
  info = list(
    description = "A data cube for further processing",
    schema = schema_format(type = "object", subtype = "raster-cube")
  )
  return(info)
}

eo_datacube = datacube_schema()


#' load collection
load_collection = Process$new(
  id = "load_collection",
  description = "Loads a collection from the current back-end by its id and returns it as processable data cube",
  categories = list("cubes", "import"),
  parameters = list(
    Parameter$new(
      name = "id",
      description = "The collection id",
      type = "string",
      subtype = "collection-id"
    ),
    Parameter$new(
      name = "spatial extent"
    )
  ),
  summary = "Load a collection",
  returns = eo_datacube,
  operation = NULL
)
