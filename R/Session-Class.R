#' Session class
#'
#' @field processes Package processes
#' @field data Data of current SessionInstance
#'
#' @include api.R
#' @importFrom R6 R6Class
#' @importFrom tibble add_row
#' @import plumber
#' @export
SessionInstance <- R6Class(
  "SessionInstance",
  public = list(

    processes = NULL,
    data = NULL,

    #' @description Create a new session
    initialize = function() {

      self$processes = list()
      self$data = list()

      private$config = SessionConfig()

      self$initEndpoints()

    },

    #' @description Get endpoints
    #'
    #' @return endpoints
    getEndpoints = function() {
      return(private$endpoints)
    },

    #' @description Get configuration
    #'
    #' @return configuration
    getConfig = function() {
      return(private$config)
    },

    #' @description Start the session
    startSession = function(){

      private$initRouter()
      self$initDirectory()

      addEndpoint()

      loadDemoData()

      private$router$run(port=private$config$api.port, host=private$config$host)
    },

    #' @description initializes workspace and data paths
    #'
    initDirectory = function() {

      if (is.null(private$config$workspace.path)) {
        private$config$workspace.path <- getwd()
      }

      if (is.null(private$config$data.path)) {
        private$config$data.path <- paste(private$config$workspace.path,"data",sep="/")
      }
    },

    #' @description biuld a df to add the endpoints later on
    #'
    initEndpoints = function() {
      private$endpoints = tibble(path=character(0), method = character(0))
    },

    #' @description Create an endpoint
    #'
    #' @param path path for the endpoint
    #' @param method type of request
    #' @param handler function to be executed
    #'
    #' @return created Endpoint
    #'
    createEndpoint = function(path, method, handler=NULL) {

      private$endpoints = private$endpoints %>% add_row(path=path,method=method)

      vPath = paste("/v1.0", path, sep = "")

      private$router$handle(path = vPath, method = method, handler = handler)
      private$router$handle(path = vPath, methods = "OPTIONS", handler = .cors_option)

    },

    #' @description redirect to well-known/openeo
    #'
    #' @param path path for the endpoint
    #' @param method type of request
    #'
    redirect = function(path, method) {

      redirected = function(req, res) {
        res$status <- 302 # redirect
        res$setHeader("Location", "./v1.0/.well-known/openeo")
      }

      private$router$handle(path = path, method = method, handler = redirected)
    },

    #' @description Function to assign data of collection to the data path
    #'
    #' @param col Collection of class 'Collection'
    #'
    assignData = function(col) {

      if (col$id %in% names(Session$data)) {
        stop("This collection id is already assigned")
      }

      if(! is.Collection(col)) {
        stop("Assigned data is not a collection")
      }

      if (is.null(self$data)) {
        self$data = list()
      }
      newCol = list(col)
      names(newCol) = col$id
      self$data = append(self$data, newCol)

    }
  ),

  private = list(

    endpoints = NULL,
    router = NULL,
    config = NULL,

    initRouter = function(){

      private$router = Router$new()

      private$router$registerHook("postroute",.cors_filter)
    }

  )
)


#' Creates a new instance from the class 'SessionInstance' and assigns the name 'Session'
#'
#' @export
createSessionInstance = function(configuration = NULL) {
  assign("Session", SessionInstance$new(),envir=.GlobalEnv)
  invisible(Session)
}
