#' Submit a Batch Prediction Job
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#' @param jobDisplayName STRING Required. The user-defined name of this BatchPredictionJob.
#' @param model STRING The name of the Model resource that produces the predictions via this job,
#' must share the same ancestor Location. Format: `projects/{project}/locations/{location}/models/{model}`
#' Starting this job has no impact on any existing deployments of the Model and their resources.
#' Exactly one of model and unmanagedContainerModel must be set.
#' The model resource name may contain version id or version alias to specify the version,
#' if no version is specified, the default version will be used.
#' @param gcsSource string Required. Google Cloud Storage URI(-s) to the input file(s). May contain wildcards. For more information on wildcards, see https://cloud.google.com/storage/docs/gsutil/addlhelp/WildcardNames.
#' @param bigquerySource string Required. BigQuery URI to a project or table, up to 2000 characters long.
#' When only the project is specified, the Dataset and Table is created. When the full table reference is specified,
#' the Dataset must exist and table must not exist.
#' Accepted forms: BigQuery URI to a table, up to 2000 characters long.
#' Accepted forms: "bq://projectId.bqDatasetId.bqTableId"
#' @param instancesFormat
#' @param predictionsFormat STRING and one of the following: "bigquery"
#' @param bigqueryDestinationPrefix BigQuery path. For example: bq://projectId or bq://projectId.bqDatasetId or bq://projectId.bqDatasetId.bqTableId.
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.batchPredictionJobs#BatchPredictionJob
#' @param sync If set to TRUE, the call will block while waiting for the asynchronous batch job to complete.
#'
#' @returns \href{https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.batchPredictionJobse}{BatchPredictionJob} object
#'
#' @export
gcva_batch_predict <- function(projectId = gcva_project_get(),
                               locationId = gcva_region_get(),
                               jobDisplayName,
                               model,
                               gcsSource = NULL,
                               bigquerySource = NULL,
                               instancesFormat = c("jsonl", "csv", "bigquery", "file-list"),
                               predictionsFormat = c("jsonl","csv","bigquery"),
                               gcsDestinationPrefix = NULL,
                               bigqueryDestinationPrefix = NULL,
                               sync=TRUE) {

  # align with require inputs
  instancesFormat <- match.arg(instancesFormat)
  predictionsFormat <- match.arg(predictionsFormat)

  # merge into request body
  batchPredictionJob <- structure(
    rmNullObs(
      list(
        displayName = jobDisplayName,
        model = model,
        inputConfig = list(
          instancesFormat = instancesFormat,
          gcsSource = gcsSource,
          bigquerySource = list(
            inputUri = bigquerySource
          )
        ),
        outputConfig = list(
          predictionsFormat = predictionsFormat,
          gcsDestination = gcsDestinationPrefix,
          bigqueryDestination = list(
            outputUri = bigqueryDestinationPrefix
          )
        )
      )
    ), class = c("gcva_batchPredictionJob", "list")
  )

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/batchPredictionJobs",
                 locationId,
                 parent)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  stopifnot(inherits(batchPredictionJob, "gcva_batchPredictionJob"))

  batchPredictionJob <- f(the_body = batchPredictionJob)

  if(sync == FALSE) {
    #return right away
    out <- gcva_batch_prediction_job(batchPredictionJob = batchPredictionJob$name)
    out
  }  else if(sync == TRUE) {
    #wait until completed
    batchPredictionJob <- gcva_wait_for_batch_prediction_job(batchPredictionJob = batchPredictionJob$name)
    out <- gcva_batch_prediction_job(batchPredictionJob = batchPredictionJob$name)
    out
  }

}


#' Wait for a BatchPredctionJob operation
#'https://cloud.google.com/vertex-ai/docs/reference/rest/v1/JobState
#' @param locationId locationId of training pipeline
#' @param batchPredictionJobName Resource name of the BatchPredictionJob.
#' @param wait INTEGER number of seconds to wait between checks. Default is 5minutes
#'
#' @return batchPredictionJob object
#'
#' @export
gcva_wait_for_batch_prediction_job <- function(locationId = gcva_region_get(),
                                               batchPredictionJob,
                                               wait=300){

  job <- gcva_batch_prediction_job(batchPredictionJob = batchPredictionJob)
  # console_url <- sprintf(
  #   "https://console.cloud.google.com/vertex-ai/locations/%s/training/%s?project=%s",
  #   locationId, trainingPipelineId, projectId)
  # myMessage("view job: ", console_url, level = 3)
  myMessage("job state: ", job$state, level = 3)

  status <- FALSE

  while(!status){
    Sys.sleep(wait)
    job <- gcva_batch_prediction_job(batchPredictionJob = batchPredictionJob)

    # myMessage("view job: ", console_url, level = 3)
    myMessage("job state: ", job$state, level = 3)

    if(job$state == "JOB_STATE_SUCCEEDED" |
       job$state == "JOB_STATE_FAILED" |
       job$state == "JOB_STATE_CANCELLED"){
      status <- TRUE
    } else {
      status <- FALSE
    }
  }
  job

}


#' Get a BatchPrediction object
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.batchPredictionJobs/get
#'
#' @param locationId locationId of training pipeline
#' @param batchPredictionJob string Required. The name of the BatchPredictionJob resource.
#' Format: `projects/{project}/locations/{location}/batchPredictionJobs/{batchPredictionJob}`
#'
#' @return `BatchPredictionJob` object https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.batchPredictionJobs
#'
#' @export
gcva_batch_prediction_job <- function(locationId = gcva_region_get(),
                                      batchPredictionJob){

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s",
                 locationId,
                 batchPredictionJob)

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f()

  out <- structure(response, class = "gcva_batchPredictionJob")

  out

}




