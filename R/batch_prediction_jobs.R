#' Submit a Batch Prediction Job
#'
#'
#' @param jobDisplayName STRING Required. The user-defined name of this BatchPredictionJob.
#' @param model STRING The name of the Model resource that produces the predictions via this job,
#' must share the same ancestor Location. Starting this job has no impact on any existing deployments of the Model and their resources.
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
#' @export
gcva_batch_predict <- function(jobDisplayName,
                               model,
                               gcsSource = NULL,
                               bigquerySource = NULL,
                               instancesFormat = c("jsonl", "csv", "bigquery", "file-list"),
                               predictionsFormat = c("jsonl","csv","bigquery"),
                               gcsDestinationPrefix = NULL,
                               bigqueryDestinationPrefix = NULL) {

  # # check uri for gcsSource
  # stopifnot(
  #   "gcsSource must be a Cloud Storage URI or 'gs://bucket-name/filename'" =
  #     grepl("gs://", gcsSource, fixed = TRUE) == TRUE)
  #
  # # check uri for bigquerySource
  # stopifnot(
  #   "bigquerySource must be a BigQuery URI to a table or 'bq://projectId.bqDatasetId.bqTableId'" =
  #     grepl("bq://", bigquerySource, fixed = TRUE) == TRUE)

  # # check uri for gcsSource
  # if(grepl("gs://", gcsSource, fixed = TRUE)) {
  #   stop("gcsSource must be a Cloud Storage URI or 'gs://bucket-name/filename'")
  # }
  #
  # # check uri for bigquerySource
  # if(grepl("bq://", bigquerySource, fixed = TRUE)) {
  #   stop("bigquerySource must be a BigQuery URI to a table or 'bq://projectId.bqDatasetId.bqTableId'")
  # }

  instancesFormat <- match.arg(instancesFormat)
  predictionsFormat <- match.arg(predictionsFormat)


  # merge into request body
  request_body <- structure(
    rmNullObs(
      list(
        displayName = jobDisplayName,
        model = model,
        inputConfig = list(
          instancesFormat = instancesFormat,
          gcsSource = gcsSource,
          bigquerySource = bigquerySource
        ),
        outputConfig = list(
          predictionsFormat = predictionsFormat,
          gcsDestination = gcsDestinationPrefix,
          bigqueryDestination = bigqueryDestinationPrefix
        )
      )
    ), class = c("gcva_batchPredictionJob", "list")
  )

  request_body

}
