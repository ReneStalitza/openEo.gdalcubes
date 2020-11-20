#' Parameter
#'
#' @field name Name of the parameter
#' @field description Short description of the parameter
#' @field type Type of the parameter
#' @field subtype More specific type of the parameter
#'
#' @include Process-class.R
#' @importFrom R6 R6Class
#' @export
Parameter <- R6Class(
  "Parameter",
  public = list(
    name = NA,
    description = NA,
    type = NA,
    subtype = NA,

    #' @description
    #'
    #' @param name Name of the parameter
    #' @param description Short description of the parameter
    #' @param type Type of the parameter
    #' @param subtype More specific type of the parameter
    #'
    initialize = function(name = NA,
                          description = NA,
                          type = NA,
                          subtype = NA) {

      self$name = name
      self$description = description
      self$type = type
      self$subtype = subtype

    },


    #' @description Get the information of the parameter
    #'
    #' @return list of information
    #'
    parameterInfo = function() {
      info = list()
      info = appendInfo(
        name = self$name,
        description = self$description,
        type = self$type,
        subtype = self$subtype)


      return(info)
    }

  )
)

#' appendInfo
#' @description Create a list with appended parameter
#'
#' @param name name of parameter
#' @param description description of parameter
#' @param type type of parameter
#' @param subtype subtype of parameter
#'
#' @return list with appended parameter
#'
appendInfo = function(name, description, type, subtype = NA) {

  info = list()
  info = append(info, list(name = name))
  info = append(info, list(description = description))
  info = append(info, list(type = type))

  if (! is.na(subtype)) {
    info = append(info, list(subtype = subtype))
  }
  return(info)
}



