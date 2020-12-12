#' @import dplyr
#' @include Session-Class.R
#' @include Router.R
#' @include processes.R
#' @include api_jobs.R
NULL

# Capabilities handler
.capabilities = function() {

  config = Session$getConfig()

  endpoints = Session$getEndpoints()

  endpoints = endpoints %>% group_by(path) %>% summarise(
    paths=list(tibble(path,method) %>% (function(x,...){
      return(list(path=unique(x$path),methods=as.list(x$method)))
    }))
  )

  list = list()
    list$api_version = config$api_version
    list$backend_version = config$backend_version
    list$stac_version = config$stac_version
    list$id = config$id
    list$title = config$title
    list$description = config$description
    list$endpoints = endpoints$paths
    list$links = list(list(
      rel = "self",
      href = paste(Session$getConfig()$base_url, "", sep = "/")))

  return(list)

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

  list = list()
  list$output = config$outputFormats
  list$input = config$inputFormats


  return(list)


}

.conformance = function() {
  config = Session$getConfig()

  list = list()
  list$conformsTo = list(config$OGC_conformanceLink)

  return(list)

}

.collections = function() {

  collections = list(collections = unname(lapply(Session$data, function(x) {
    return(x$collectionInfo())
  })))
  collections$links = list(list(
     rel = "self",
     href = paste(Session$getConfig()$base_url, "collections", sep = "/")
   ))

  return(collections)

}

.collectionId = function(req, res, collection_id) {

  return (Session$data[[collection_id]]$collectionInfoExtended())

}

.processes = function() {

  processes = list(processes = unname(lapply(Session$processes, function(process){
    return(process$processInfo())
  })))

  processes$links = list(list(
    rel = "self",
    href = paste(Session$getConfig()$base_url, "processes", sep = "/")
  ))

  return(processes)
}

.login_basic = function(req, res) {
  auth = req$HTTP_AUTHORIZATION
  encoded = substr(auth,7,nchar(auth))
  decoded = rawToChar(base64enc::base64decode(encoded))
  user_name = unlist(strsplit(decoded,":"))[1]
  user_pwd = unlist(strsplit(decoded,":"))[2]

  config = Session$getConfig()
  if(user_name == config$user && user_pwd == config$password) {
    #token = ids::random_id(bytes = 8)
    token = "b34ba2bdf9ac9ee1"
    Session$setToken(token)

    return(list(access_token = token))
  }
  else {
    stop("Credentials are invalid")
  }
}

.authorized = function(req, res) {

  auth = req$HTTP_AUTHORIZATION
  sub = substr(auth,15,nchar(auth))
  token = Session$getToken()

  if (is.null(auth)) {
    res$status <- 401
    list(error="Authentication required")
  }
  else if (sub != token) {
    res$status <- 403
    list(error="Authentication failed")
  }
   else {
    forward()
  }
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

  Session$createEndpoint(path = "/collections/{collection_id}",
                         method = "GET",
                         handler = .collectionId)

  Session$createEndpoint(path = "/processes",
                         method = "GET",
                         handler = .processes)

  Session$createEndpoint(path = "/process_graphs",
                         method = "POST",
                         handler = .createProcessGraph)

  Session$createEndpoint(path = "/jobs",
                         method = "GET",
                         handler = .listAllJobs)

  Session$createEndpoint(path = "/jobs",
                         method = "POST",
                         handler = .createNewJob,
                         filter = TRUE)

  Session$createEndpoint(path = "/jobs/{job_id}",
                         method = "GET",
                         handler = .getJobById)

  Session$createEndpoint(path = "/jobs/{job_id}/results",
                         method = "POST",
                         handler = .startJob)

 Session$createEndpoint(path = "/jobs/{job_id}/results",
                        method = "GET",
                        handler = .getJobResults)

  Session$createEndpoint(path = "/credentials/basic",
                         method = "GET",
                         handler = .login_basic)



  Session$assignProcess(load_collection)
  Session$assignProcess(save_result)
}
