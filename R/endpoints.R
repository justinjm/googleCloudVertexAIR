#' @title
#' Lists Vertex AI Endpoints within a specific GCP Project
#'
#' @param projectId
#' @param locationId
#'
#' @family Endpoints
#'
#' @export
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


#'
#' @param locationId
#' @param endpointName
#'
#' @return a Endpoint object
#'
#' @family Endpoints
#' @export
gcva_endpoint <- function(locationId = gcva_region_get(),
                          endpointName){

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s",
                 locationId,
                 endpointName)

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f()

  out <- structure(response, class = "gcva_endpoint")

  out

}



#' @title
#' Creates a Vertex AI Endpoint
#'
#' @description
#' Creates an Endpoint Resource. Models are deployed into it, and afterwards Endpoint is called to obtain predictions and explanations.
#'
#' @param projectId
#' @param locationId
#' @param name
#' @param displayName STRING Required. Required. The display name of the Endpoint. The name can be up to 128 characters long and can consist of any UTF-8 characters.
#' @param description
#' @param deployedModels
#' @param trafficSplit
#' @param etag
#' @param labels
#' @param createTime
#' @param updateTime
#' @param encryptionSpecs
#' @param network
#' @param enablePrivateServiceConnect
#' @param modelDeploymentMonitoringJob
#' @param predictRequestResponseLoggingConfig
#' @return Endpoint object
#' https://cloud.google.com/vertex-ai/docs/reference/rest/Shared.Types/ListOperationsResponse#Operation
#'
#' @family Endpoints
#'
#' @export
gcva_create_endpoint <- function(projectId = gcva_project_get(),
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
                                 predictRequestResponseLoggingConfig=NULL){

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

  out  <- gcva_endpoint(endpointName = response$response$name)
  out

}

#' @title
#' Deploy a model to an Endpoint
#'
#' @param projectId
#' @param locationId
#' @param model Model object
#' @param endpoint Endpoint object
#' @param machineType STRING
#'
#' @return Endpoint object
#'
#' @family Endpoints
#'
#' @export
gcva_deploy <- function(projectId = gcva_project_get(),
                        locationId = gcva_region_get(),
                        model=NULL,
                        modelVersionId=NULL,
                        endpoint=NULL,
                        enableAccessLogging=NULL,
                        trafficSplit=NULL,
                        machineType="n1-standard-4",
                        minReplicaCount=1,
                        maxReplicaCount=NULL) {
  ## checks
  ### check if model object
  stopifnot(inherits(model, "gcva_model"))
  ### check if endpoint object
  stopifnot(inherits(endpoint, "gcva_endpoint"))

  # projects/442003009360/locations/us-central1/endpoints/5359762943640600576
  # endpoint name for API request
  name <- endpoint$name

  # model name for request body
  model <- model$name

  request_body <- structure(
    rmNullObs(
      list(deployedModel = list(
        model = model,
        modelVersionId = modelVersionId,
        enableAccessLogging = enableAccessLogging,
        dedicatedResources = list(
          machineSpec = list(
            machineType = machineType
          ),
          minReplicaCount = minReplicaCount,
          maxReplicaCount = maxReplicaCount
        )
      ),
      trafficSplit = trafficSplit
      )
    )
  )

  # https://{service-endpoint}/v1/{endpoint}:deployModel
  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s:deployModel",
                 locationId,
                 name)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  # projects.locations.endpoints.deployModel
  response <- f(the_body = request_body)

  response
  # response <- gcva_wait_for_op(operation = response$name)

  # TODO - update after initial test
  # out  <- gcva_endpoint(endpointName = response$response$name)
  # out

}






# gcva_endpoint_undeploy_all <- function(){
#
# }


#' @title
#' Deletes a Vertex AI Endpoint
#'
#' @family Endpoints
#'
#' @export
gcva_delete_endpoint <- function(projectId = gcva_project_get(),
                                 locationId = gcva_region_get(),
                                 displayName = NULL,
                                 endpoint) {

  # if not using display name, use endpoint object to delete
  if(is.null(displayName)) {

    name <- endpoint$name

  } else {
    endpoints_list <- gcva_list_endpoints(projectId = projectId,
                                          locationId = locationId)

    endpoint_display_name <- displayName

    name <- subset(endpoints_list,
                   displayName == endpoint_display_name,
                   select = c(name))

    if (dim(name)[1] == 0) {
      stop(sprintf("endpoint %s does not exist. Please check the endpoint displayname is correct and try again.",
                   displayName))
    }
  }



  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s",
                 locationId,
                 name)

  f <- googleAuthR::gar_api_generator(url,
                                      "DELETE",
                                      data_parse_function = function(x) x)

  response <- f()

  out <- response

  if(out$done==TRUE) {
    myMessage("endpoint successfully deleted.", level = 3)

  } else {
    out
  }

}
