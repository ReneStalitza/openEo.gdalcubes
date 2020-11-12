#' @description handler functions and creating corresponding endpoints
#
#' @import dplyr
#' @include Session-Class.R
#' @include Router.R


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

  list(
    versions = list(
      url = config$base_url,
      api_version = config$api_version
    )
  )

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
}
