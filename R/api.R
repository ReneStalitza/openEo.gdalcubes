#' @title api
#' @description handler functions and creating corresponding endpoints
#
#' @import dplyr
#' @include Session-Class.R
#' @include Router.R
#' @include processes.R


#' Capailities handler
.capabilities = function() {

  config = Session$getConfig()

  endpoints = Session$getEndpoints()

  endpoints = endpoints %>% group_by(path) %>% summarise(
    paths=list(tibble(path,method) %>% (function(x,...){
      return(list(path=unique(x$path),methods=as.list(x$method)))
    }))
  )

  list(
    api_version = config$api_version,
    backend_version = config$backend_version,
    stac_version = config$stac_version,
    id = config$id,
    title = config$title,
    description = config$description,
    endpoints = endpoints$paths
    # TO DO: links
  )
}

.well_known = function() {

  config = Session$getConfig()
  version = list(versions = list())


  obj = tibble(url = config$base_url,
               api_version = config$api_version,
               production = FALSE)
  version$versions = obj

  return(version)


}

.file_formats = function() {

  config = Session$getConfig()

  list(
    output = config$outputFormats,
    input = config$inputFormats
  )


}

.conformance = function() {
  config = Session$getConfig()

  list(
    conformsTo = config$OGC_conformanceLink
  )

}

.collections = function() {

  data = Session$data
  list(collections = unname(lapply(data, function(x) {
   return(x$collectionInfo())
    })),
   links = list(
     rel = "self",
     href = paste(Session$getConfig()$base_url, "collections", sep = "/")
   )
  )
}

.collectionId = function(req, res, collection_id) {

  return (Session$data[[collection_id]]$collectionInfoExtended())

}

.processes = function() {

  processes = list(processes=unname(lapply(Session$processes, function(process){
    return(process$processInfo())
  })))

  links = list(
    rel = "self",
    href = paste(Session$getConfig()$base_url, "processes", sep = "/")
  )

  result = as.vector(c(links =list(links), processes))

  return(result)


}

.cors_filter = function(req,res) {
  res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN)
  res$setHeader("Access-Control-Expose-Headers", "Location, OpenEO-Identifier, OpenEO-Costs")
  forward()
}

.cors_option = function(req,res, ...) {
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,OPTIONS,PATCH")


  res$status = 204
}



#' dedicate the handler functions to the corresponding paths
addEndpoint = function() {

  Session$createEndpoint(path = "/",
                         method = "GET",
                         handler = .capabilities)

  Session$createEndpoint(path = "/.well-known/openeo",
                         method = "GET",
                         handler = .well_known)

  Session$createEndpoint(path = "/file_formats",
                         method = "GET",
                         handler = .file_formats)

  Session$createEndpoint(path = "/conformance",
                         method = "GET",
                         handler = .conformance)

  Session$createEndpoint(path = "/collections",
                         method = "GET",
                         handler = .collections)

  Session$createEndpoint(path = "/collections/<collection_id>",
                         method = "GET",
                         handler = .collectionId)

  Session$createEndpoint(path = "/processes",
                         method = "GET",
                         handler = .processes)

  Session$assignProcess(load_collection)
  Session$assignProcess(load_collectionB)
}
