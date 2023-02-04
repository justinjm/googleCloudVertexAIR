#' @title
#' Creates a Vertex AI Endpoint
#'
#' @description
#' Creates an Endpoint Resource. Models are deployed into it, and afterwards Endpoint is called to obtain predictions and explanations.
#'
#' @param name
#' @param displayName STRING Required. Required. The display name of the Endpoint. The name can be up to 128 characters long and can consist of any UTF-8 characters.
#' @param description
#' @param deployedModels
#' @param trafficSplit
#' @param etag
#' @param labels
#' @param createTime
#' @param updateTime
#' @param encryptionSpec
#' @param network
#' @param enablePrivateServiceConnect
#' @param modelDeploymentMonitoringJob
#' @param predictRequestResponseLoggingConfig
#' @return operation object
#' https://cloud.google.com/vertex-ai/docs/reference/rest/Shared.Types/ListOperationsResponse#Operation
#' @export
gcva_create_endpoint <- function(
    project=NULL,
    location=NULL,
    name=NULL,
    display_name=NULL,
    description=NULL,
    deployedModels=NULL,
    trafficSplit=NULL,
    etag=NULL,
    labels=NULL,
    createTime=NULL,
    updateTime=NULL,
    encryptionSpec=NULL,
    network=NULL,
    enablePrivateServiceConnect=NULL,
    modelDeploymentMonitoringJob=NULL,
    predictRequestResponseLoggingConfig=NULL
    ){

  # build request body
  request_body <- structure(
    rmNullObs(
      list(
        name = name,
        displayName = display_name,
        description = description,
        deployedModels = deployedModels,
        trafficSplit = trafficSplit,
        etag = etag,
        labels = labels,
        createTime = createTime,
        updateTime = updateTime,
        encryptionSpec = encryptionSpec,
        network = network,
        enablePrivateServiceConnect = enablePrivateServiceConnect,
        modelDeploymentMonitoringJob = modelDeploymentMonitoringJob,
        predictRequestResponseLoggingConfig = predictRequestResponseLoggingConfig
      )

    )
  )
  request_body



  # parent for API request
  # parent <- sprintf("projects/%s/locations/%s",
  #                   projectId,
  #                   locationId)
  #
  # url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/endpoints",
  #                locationId,
  #                parent)
  #
  # f <- googleAuthR::gar_api_generator(url,
  #                                     "POST",
  #                                     data_parse_function = function(x) x,
  #                                     checkTrailingSlash = FALSE)

  # projects.locations.endpoints.create

}




#### Example
# library(jsonlite)
#
# name <- "My Endpoint"
# displayName <- "My Display Name"
# description <- "My Endpoint Description"
# deployedModels <- list(list(name = "Deployed Model 1"))
# trafficSplit <- list(model1 = 50, model2 = 50)
# etag <- "etag123"
# labels <- list(label1 = "value1", label2 = "value2")
# createTime <- "2023-02-03T10:00:00Z"
# updateTime <- "2023-02-03T11:00:00Z"
# encryptionSpec <- list(type = "EncryptionType")
# network <- "default"
# enablePrivateServiceConnect <- TRUE
# modelDeploymentMonitoringJob <- "monitoring-job"
# predictRequestResponseLoggingConfig <- list(enabled = TRUE)
#
# endpoint_json <- toJSON(list(
#   name = name,
#   displayName = displayName,
#   description = description,
#   deployedModels = deployedModels,
#   trafficSplit = trafficSplit,
#   etag = etag,
#   labels = labels,
#   createTime = createTime,
#   updateTime = updateTime,
#   encryptionSpec = encryptionSpec,
#   network = network,
#   enablePrivateServiceConnect = enablePrivateServiceConnect,
#   modelDeploymentMonitoringJob = modelDeploymentMonitoringJob,
#   predictRequestResponseLoggingConfig = predictRequestResponseLoggingConfig
# ), auto_unbox = TRUE,
# pretty = TRUE)
#
# # cat(endpoint_json)
# endpoint_json



