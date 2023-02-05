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
    projectId = gcva_project_get(),
    locationId = gcva_region_get(),
    name=NULL,
    displayName=NULL,
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
        displayName = displayName,
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
    ),class = c("gcva_endpoint", "list")
  )

  # parent for API request
  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/endpoints",
                 locationId,
                 parent)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  # projects.locations.endpoints.create
  response <- f(the_body = request_body)

  response <- gcva_wait_for_op(operation = response$name)
  response

}


gcva_list_endpoints <- function(projectId = gcva_project_get(),
                                locationId = gcva_region_get()) {

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/endpoints",
                 locationId,
                 parent)

  parse_ld <- function(x) {
    x <- x$endpoints
    x$createTime <- timestamp_to_r(x$createTime)

    x

  }

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = parse_ld)
  response <- f()

  out <- response

  out

}


# gcva_endpoint <- function() {
#
# }


# gcva_endpoint_undeploy_all <- function(){
#
# }


# gcva_delete_endpoint <- function(){
#
# }
