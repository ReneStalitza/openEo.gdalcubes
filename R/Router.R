# Router
Router = R6Class(
  "Router",
  inherit = plumber,

  # public
  public = list(
    initialize = function(filters=plumber:::defaultPlumberFilters,envir) {
      if (missing(envir)){
        private$envir <- new.env(parent=.GlobalEnv)
      } else {
        private$envir <- envir
      }


      private$errorHandler <- plumber:::defaultErrorHandler()
      private$notFoundHandler <- plumber:::default404Handler

    }
  ),

  # private
  private = list()

)
