#' Parameter
#'
#' @field name Name of the parameter
#' @field description Short description of the parameter
#' @field schema Type and subtype of the parameter
#' @field optional Is this parameter required for the process
#'
#' @include Process-class.R
#' @importFrom R6 R6Class
#'
#' @export
Parameter <- R6Class(
  "Parameter",
  public = list(
    name = NA,
    description = NA,
    schema = NA,
    optional = NA,

    #' @description
    #'
    #' @param name Name of the parameter
    #' @param description Short description of the parameter
    #' @param type Type of the parameter
    #' @param subtype More specific type of the parameter
    #' @param optional Is this parameter required for the process
    #'
    initialize = function(name = NA,
                          description = NA,
                          type = NA,
                          subtype = NA,
                          optional = NA) {

      self$name = name
      self$description = description
      self$schema = schema_format(type, subtype)
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
        schema = self$schema
        )


      return(info)
    }

  )
)

#' appendInfo
#'
#' @description Create a list with appended parameter
#'
#' @param name name of parameter
#' @param description description of parameter
#' @param type type of parameter
#' @param subtype subtype of parameter
#'
#' @return list with appended parameter
#'
appendInfo = function(name, description, schema, optional = NA) {

  info = list()
  info = append(info, list(name = name))
  info = append(info, list(description = description))
  info = append(info, list(schema = schema))

  if (! is.na(optional)) {
    info = append(info, list(optional = optional))
  }
  return(info)
}
