# job handlers

.listAllJobs = function() {
  jobs = list(jobs = unname(lapply(Session$jobs, function(job_id){
      job = Job$new(job_id)
      job$load()
      return(job$jobInfo())
    })))

    links = list(
      rel = "self",
      href = paste(Session$getConfig()$base_url, "jobs", sep = "/")
    )

    result = as.vector(c(jobs, links =list(list(links))))

    return(result)
  }

.getJobById = function(req, res, job_id) {
  index = getJobIdIndex(job_id)

  if (! is.na(index)) {
    job = Job$new(job_id = job_id)
    job$load()

    res$body = toJSON(job$jobInfo(),na="null",null="null",auto_unbox = TRUE)
    res$setHeader("Content-Type","application/json")

    return(res)
  }
  else {
    throwError("Job not found")
  }
}

.createNewJob = function(req,res) {

    sent_job = fromJSON(req$postBody,simplifyDataFrame = FALSE)

    # TODO check if postBody is valid
    process_graph = sent_job$process_graph

    job = Job$new(process_graph = process_graph)
    job$status = "submitted"
    job$created = Sys.time()

    if (!is.null(sent_job$title)) job$title = sent_job$title
    if (!is.null(sent_job$description)) job$description = sent_job$description

    job$store()
    res$setHeader(name = "Location",
                  value= paste(Session$getConfig()$base_url, "jobs", job$job_id, sep ="/"))
    res$setHeader(name = "OpenEO-Identifier",value = job$job_id)
    res$status = 201

    return(res)
}
