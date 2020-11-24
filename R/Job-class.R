#' Job
#'
#' @field job_id Id of the job
#' @field status Current status of the job
#' @field process_graph Process graph of the job
#' @field created When the job was created
#' @field title Title of the job
#' @field description Shortly description of the planned result
#' @field results Result of the executed process
#'
#' @importFrom R6 R6Class
#'
#' @export
Job <- R6Class(
  "Job",
  public = list(
    job_id = NULL,
    status=NA,
    process_graph = NULL,
    created = NULL,
    title = NULL,
    description = NULL,
    results = NULL,

    #' @description Initialize job
    #'
    #' @param job_id Id of the job
    #' @param status Current status of the job
    #' @param process_graph Process graph of the job
    #' @param created When the job was created
    #' @param title Title of the job
    #' @param description Shortly description of the planned result
    #' @param results Result of the executed process
    #'
    initialize = function(job_id = NULL, process_graph = NULL) {
      self$job_id = job_id
      self$created = Sys.time()
      self$status = "created"

      if (!is.null(process_graph)) {
        if (!is.ProcessGraph(process_graph)) {
          if (is.graph_id(process_graph)) {
            proGraph = ProcessGraph$new(process_graph_id=process_graph)
            proGraph$process_graph_id = NULL # will be created on store
          }
          else {
            proGraph = ProcessGraph$new(process_graph = process_graph)
          }
        }
        else {
          proGraph$process_graph = process_graph
        }
        self$process_graph = proGraph$buildExecutableProcessGraph(job=self)
      }
      return(self)
    },

    #' @description Store the job in the Session
    #'
    store = function() {

      if (is.na(self$job_id)) {
        self$job_id = ids::random_id(bytes = 6)
      }
      if (is.na(self$process_graph$process_graph_id) || is.null(self$process_graph$process_graph_id)) {
        self$process_graph$store()
      }

      if (is.null(getJobIdIndex(self$job_id))) {

        Session$jobs = append(Session$jobs, list(list(job_id = self$job_id,
                                                      status = self$status,
                                                      created = self$created,
                                                      process_graph = self$process_graph,
                                                      title = self$title,
                                                      description = self$description )))
      }
      invisible(self)
    },

    #' @description Load the job properties from the stored jobs
    #'
    load = function() {
      index = getJobIdIndex(self$job_id)

      if (! is.na(index)) {
        storedJob = Session$job[[index]]

        self$status = storedJob$status
        self$created = storedJob$created
        self$title = storedJob$title
        self$description = storedJob$description

        proGraph = ProcessGraph$new(process_graph_id = storedJob$process_graph)
        self$process_graph = proGraph$buildExecutableProcessGraph(job = self)

        invisible(self)
      }
    },

    #' @description Execute the executable process graph and store it in the results of the job
    #'
    #' @return The job
    run = function() {

      tryCatch({
        self$status = "running"
        self$results = self$process_graph$execute()
        self$status = "finished"
      },
        error=function (e) {
        self$status = "error"
        self$results = NULL
      },
      finally = {
        return(self)
      })
    },

    #' @description Get information about the job
    #'
    #' @return Info list
    #'
    jobInfo = function() {

      info = list(
        job_id = self$job_id,
        title = self$title,
        description = self$description,
        process_graph = self$process_graph,
        status = self$status,
        created = self$created
      )

      return(info)
    }

  )

)

#' getJobIdIndex
#' @param jid Job id
#' @return Index of the given id in the stored jobs
#'
#' @export
getJobIdIndex = function(jid) {
  ids = lapply(Session$jobs, function(x) {
    return(x$job_id)
  })
  index = match(jid, ids)
  return(index)
}
