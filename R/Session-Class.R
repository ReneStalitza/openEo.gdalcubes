#config = ServerConfig()

#' Session
#'
#' This is the session class
#'
#' @field processes This field is also managed during runtime. Here all template processes are listed
#' @field data A list of products offered by the service which is managed at runtime.
#'
#' @importFrom R6 R6Class
#' @import plumber
#' @export
Session <- R6Class(
  "Session",
  public = list(

    # attributes
    processes = NULL,
    data = NULL,

    # functions
    initialize = function(configuration = NULL) {

      self$processes = list()
      self$data = list()

      private$config = configuration

    },

    getEndpoints = function() {
      return(private$endpoints)
    },

    getConfig = function() {
      return(private$config)
    },

    startup = function(){

      private$initRouter()

      private$router$run(port=8000,host="127.0.0.1")
    }


  ),

  # private ----
  private = list(
    # attributes ====
    endpoints = NULL,
    router = NULL, # plumber class
    config = NULL,

    # functions

    initRouter = function(){

      private$router = Router$new()
    }

  )
)





#' Creates a server instance
#'
#' The function creates a new server instance on the global variable 'openeo.server'. The names for
#' this variable is reserved and should not be changed by any means. It will crash the system, since
#' many endpoints will be accessing and depending on the correctly set variable 'openeo.server'.
#'
#' @export
createSessionInstance = function(configuration = NULL) {
  assign("SessionInstance", Session$new(configuration),envir=.GlobalEnv)
  invisible(SessionInstance)
}
