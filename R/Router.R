#' Router class
Router = R6Class(
  "Router",
  inherit = plumber,

  public = list(
    #' Initialize router
    #'
    #' @param filters plumber filter
    #' @param envir global environment
    initialize = function(filters=plumber:::defaultPlumberFilters,envir) {
      if (missing(envir)){
        private$envir <- new.env(parent=.GlobalEnv)
      } else {
        private$envir <- envir
      }

      self$setSerializer(serializer_unboxed_json())

      private$errorHandler <- plumber:::defaultErrorHandler()
      private$notFoundHandler <- plumber:::default404Handler

    }
  ),

  private = list()

)
