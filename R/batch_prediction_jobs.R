






#'
#' @param jobDisplayName STRING Required. The user-defined name of this BatchPredictionJob.
#' @param model STRING The name of the Model resource that produces the predictions via this job,
#' must share the same ancestor Location. Starting this job has no impact on any existing deployments of the Model and their resources.
#' Exactly one of model and unmanagedContainerModel must be set.
#' The model resource name may contain version id or version alias to specify the version,
#' if no version is specified, the default version will be used.
#' @param bigquerySource BigQuery URI to a table, up to 2000 characters long. Accepted forms: "bq://projectId.bqDatasetId.bqTableId"
#'
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.batchPredictionJobs#BatchPredictionJob
gcva_batch_predict <- function(jobDisplayName,
                               model,
                               bigquerySource,
                               instancesFormat = c("bigquery"),
                               predictionsFormat = c("bigquery"),
                               bigqueryDestinationPrefix) {

  # TODO - what is default params to run function? bq or gcs?
  # check bq url
  tryCatch({
    stopifnot(grepl("bq://", bigquerySource, fixed = TRUE))

  }, error = function(e) {
    stop("error: ", e$message)

  })


  instancesFormat <- match.arg(instancesFormat)
  predictionsFormat <- match.arg(predictionsFormat)


  # build input config
  inputConfig = list(
    instancesFormat = instancesFormat,
    bigquerySource = bigquerySource
  )

  # build output config
  outputConfig =list(
    predictionsFormat = predictionsFormat
  )

  # merge into request body
  request_body <- structure(
    rmNullObs(
      list(
        displayName = jobDisplayName,
        inputConfig = inputConfig,
        outputConfig = outputConfig

      )
    ), class = c("gcva_batchPredictionJob", "list")
  )

  request_body

}
