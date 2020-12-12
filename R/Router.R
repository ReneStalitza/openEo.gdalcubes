#' Router class
Router = R6Class(
  "Router",
  inherit = Plumber,

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

      private$errorHandler <- plumber:::defaultErrorHandler()
      private$notFoundHandler <- plumber:::default404Handler

    }
  )
)
