
#'
#'
#'
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.batchPredictionJobs#BatchPredictionJob
gcva_batch_predict <- function(jobDisplayName,
                               model,
                               bigquerySource,
                               instancesFormat,
                               predictionsFormat,
                               bigqueryDestinationPrefix) {

  # what is default params to run function?


  # model
  # check if
  # Bq sourc
  bigquerySource <- match.arg()
  instancesFormat <- match.arg()
  predictionsFormat <- match.arg()


  # build input config
  # inputConfig =



  # build output config
  # outputConfig =

  # merge into request body


  request_body <- structure(
    rmNullObs(
      list(
        displayName = displayName,
        inputConfig = list(
          instancesFormat = instancesFormat,
          bigquerySource = bigquerySource
        ),
        outputConfig = list(
          predictionsFormat = predictionsFormat

        ),

      )
    ), class = c("gcva_batchPredictionJob", "list")
  )

  request_body

}
