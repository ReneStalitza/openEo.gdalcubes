#' Process graph
#'
#' @field process_graph_id Id of the process graph
#' @field process_graph Attached process graph
#'
#' @importFrom R6 R6Class
#' @export
ProcessGraph <- R6Class(
  "ProcessGraph",
  public = list(
    process_graph_id = NA,
    process_graph = NA,

    #' @description Initialize process graph
    #'
    #' @param process_graph_id Id of the process graph
    #' @param process_graph Attached process graph
    #'
    initialize = function (process_graph_id = NA, process_graph = NULL) {

      if (!is.na(process_graph_id)) {
        if (is.na (getPgidIndex(self$process_graph_id))) {
          stop("No matching process graph id was found ")
        }
        self$process_graph_id = process_graph_id
        return(self$load())
      }

      if (!is.null(process_graph)) {
        if (is.list(process_graph)) {
          self$process_graph = process_graph
        }
        else if (is.character(process_graph)) {
          self$process_graph = fromJSON(process_graph, simplifyDataFrame=FALSE)
        }
        else {
          stop("Invalid process graph")
        }
      }

    },

    #' @description Attach corresponding process graph
    #'
    load = function() {
      index = getPgidIndex(self$process_graph_id)

      if (! is.na(index)) {
        self$process_graph = Session$graphs[[index]]$process_graph
      }

    },

    #' @description Attach process graph to the Session
    #'
    store = function() {

      if (is.na(self$process_graph_id)) {
        self$process_graph_id = ids::random_id(bytes = 8)
      }

      if (!is.null(self$process_graph)) {

        Session$graphs = append(Session$graphs, list(list(process_graph_id = self$process_graph_id,
                                                          process_graph = self$process_graph )))
      }
    },

    #' @description Call the private function loadProcess with given process graph and job
    #' @param job Current job
    #' @return Executable process
    #'
    buildExecutableProcessGraph = function(job=NULL) {
      res = private$loadProcess(self$process_graph, job=job)
      return(res)
    }
  ),

  private = list(

      #' Make an executable process for the given graph
      #'
      #' param graph_list Graph to load the corresponding process
      #' param job Attach the job to the process
      #'
      loadProcess = function(graph_list, job) {
        processId = graph_list[["id"]]
        graph_list[["id"]] = NULL
        graph_list[["description"]] = NULL

        if (!is.null(processId) && processId %in% names(Session$processes)) {
          process = Session$processes[[processId]]

        if (is.null(process) && ! is.Process(process)) {
          stop("Defined process is null or not a process")
        }

        params = graph_list

        executable = process$clone(deep=TRUE)

        clonedParameters = list()
        for (par in process$parameters) {
          clonedParameters=append(clonedParameters,par$clone(deep=TRUE))
        }
        executable$parameters = clonedParameters

        for (key in names(params)) {
          value = params[[key]]

          if (class(value) == "list" && "process_id" %in% names(value)) {
            executable$setParameter(key, private$loadProcess(value, job))
          }
          else {
            executable$setParameter(key, value)
          }
        }

        result = ExecutableProcess$new(process=executable)
        result$job = job
        return(result)
        }
        else {
        stop(paste("Cannot load process",processId))
        }
      }


  )
)
#' Check if process graph
#' @param obj process graph to be checked
is.ProcessGraph = function(obj) {
  return("ProcessGraph" %in% class(obj))
}

#' Check if graph id
#' @param obj id to be checked
is.graphId = function(obj) {
  if (nchar(graph_id) == 16) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#' Get Index of given process graph id in stored graphs
#' @param pgid process graph id
getPgidIndex = function(pgid) {
  ids = lapply(Session$graphs, function(x) {
    return(x$process_graph_id)
  })
  index = match(pgid, ids)
  return(index)
}
