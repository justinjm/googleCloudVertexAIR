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


#' @title
#' Get an Endpoint
#' @param locationId
#' @param endpointName
#'
#' @return a Endpoint object
#'
#' @family Endpoints
#'
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

  response2 <- gcva_wait_for_op(operation = response$name)

  out  <- gcva_endpoint(endpointName = response2$response$name)
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

  # endpoint name for API request
  name <- endpoint$name

  # model name for request body
  model <- model$name

  # request body
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

  # projects.locations.endpoints.deployModel
  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s:deployModel",
                 locationId,
                 name)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f(the_body = request_body)

  response <- gcva_wait_for_model_deploy(operation = response$name)

  out  <- gcva_endpoint(endpointName = response$name)
  out

}


#' @title
#' Wait for an Model to deploy
#'
#' @param locationId locationId of operation
#' @param operation A operation object
#' @param wait the number of seconds to wait between checks
#' Use this function to do a loop to check progress of a operation running
#'
#' @return After a while, a completed model deployment operation
#'
#' @family Endpoints
#'
#' @export
gcva_wait_for_model_deploy <- function(locationId = gcva_region_get(),
                                       operation,
                                       wait=10) {

  status <- FALSE
  time <- Sys.time()

  myMessage("Deploying model to endpoint...", level = 3)

  while(!status){
    Sys.sleep(wait)
    myMessage("Model deployment in progress. Waiting ", wait,
              " seconds before checking operation status.", level = 3)

    operation_response <- gcva_get_op(locationId, operationId = operation)

    if("done" %in% names(operation_response) && operation_response[["done"]] == TRUE){
      status <- TRUE
      myMessage("Model deployment completed successfully.", level = 3)
    } else {
      status <- FALSE
    }
  }

  operation_response

}

#' @title
#' Predict
#'
#' @param locationId locationId of operation
#' @param endpoint an `Endpoint` resource name "projects/{project}/locations/{location}/endpoints/{endpoint}"
#' @param instances a json object for online prediction request
#'
#' @family Endpoints
#'
#' @export
gcva_predict <- function(locationId = gcva_region_get(),
                         endpoint,
                         instances){

  request_body <- structure(
    list(instances = instances$instances)
  )

  # resource name for API request
  name <- endpoint$name

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s:predict",
                 locationId,
                 name)


  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f(the_body = request_body)
  out <- response
  out
}


#' @title
#' Undeploy Model from Endpoint
#'
#' @param projectId
#' @param locationId
#' @param endpoint	string Required. Endpoint object `gcva_endpoint` or
#' The name of the Endpoint resource from which to undeploy a Model.
#' Format: projects/{project}/locations/{location}/endpoints/{endpoint}
#'
#' @family Endpoints
#'
#' @export
gcva_undeploy <- function(projectId = gcva_project_get(),
                          locationId = gcva_region_get(),
                          endpoint
){

  if(inherits(endpoint, "gcva_endpoint")){
    # if passing trainingPipeine objectm get from object
    name <- endpoint$name
  }

  # "projects/442003009360/locations/us-central1/endpoints/1203107613345054720"
  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s:undeployModel",
                 locationId,
                 name)

  # POST https://{service-endpoint}/v1/{endpoint}:undeployModel
  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  # response <- f(the_body = request_body)

}


#'
#'
#'
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
