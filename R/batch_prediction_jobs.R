






#' Submit a Batch Prediction Job
#'
#'
#' @param jobDisplayName STRING Required. The user-defined name of this BatchPredictionJob.
#' @param model STRING The name of the Model resource that produces the predictions via this job,
#' must share the same ancestor Location. Starting this job has no impact on any existing deployments of the Model and their resources.
#' Exactly one of model and unmanagedContainerModel must be set.
#' The model resource name may contain version id or version alias to specify the version,
#' if no version is specified, the default version will be used.
#' @param bigquerySource string Required. BigQuery URI to a project or table, up to 2000 characters long.
#' When only the project is specified, the Dataset and Table is created. When the full table reference is specified,
#' the Dataset must exist and table must not exist.
#' Accepted forms: BigQuery URI to a table, up to 2000 characters long.
#' Accepted forms: "bq://projectId.bqDatasetId.bqTableId"
#' @param instancesFormat
#' @param predictionsFormat STRING and one of the following: "bigquery"
#' @param bigqueryDestinationPrefix BigQuery path. For example: bq://projectId or bq://projectId.bqDatasetId or bq://projectId.bqDatasetId.bqTableId.
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.batchPredictionJobs#BatchPredictionJob
#' @export
gcva_batch_predict <- function(jobDisplayName,
                               model,
                               bigquerySource,
                               instancesFormat = c("bigquery"),
                               predictionsFormat = c("bigquery"),
                               bigqueryDestinationPrefix) {

  # TODO - what is default params to run function? bq or gcs?

  # check bq url for bigquerySource
  # stopifnot(grepl("bq://", bigquerySource, fixed = TRUE))

  # check bq url
  tryCatch({
    stopifnot(grepl("bq://", bigquerySource, fixed = TRUE))

  }, error = function(e) {
    stop("error: ", e$message)
    myMessage("bigquerySource must be a BigQuery URI to a table, e.g. - 'bq://projectId.bqDatasetId.bqTableId'", level = 3)

  })



  instancesFormat <- match.arg(instancesFormat)
  predictionsFormat <- match.arg(predictionsFormat)


  # build input config
  inputConfig_l = list(
    instancesFormat = instancesFormat,
    bigquerySource = bigquerySource
  )

  # build output config
  outputConfig_l =list(
    predictionsFormat = predictionsFormat
  )

  # merge into request body
  request_body <- structure(
    rmNullObs(
      list(
        displayName = jobDisplayName,
        inputConfig = inputConfig_l,
        outputConfig = outputConfig_l
      )
    ), class = c("gcva_batchPredictionJob", "list")
  )

  request_body

}
