#' @title
#' Constructs an AutoML Tabular Training Job
#'
#' @description
#' constructs a training job for running
#'
#' @param displayName STRING Required. The user-defined name of this TrainingPipeline
#' @param weightColumn STRING column name of weight
#' @param optimizationPredictionType STRING Required. model type, one of: "regression", "classification"
#' @param budgetMilliNodeHours model budget, defaults to 1 hour or 1000
#' @param optimizationObjective objective to optimize for
#' @param column_transformations list of columns and column types, each pair a list
#'
#' @return "gcva_automlTabularTrainingJob" object
#'
#' @export
gcva_automl_tabluar_training_job <- function(
  displayName,
  weightColumn=NULL,
  optimizationPredictionType = c("regression", "classification"),
  budgetMilliNodeHours=1000,
  optimizationObjective=NULL,
  column_transformations=NULL) {

  # set prediction type from available list
  optimizationPredictionType <- match.arg(optimizationPredictionType)

  # TODO - documentation of trainingTaskDefinition
  # https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.trainingPipelines#TrainingPipeline.FIELDS.training_task_definition
  # command to download specs locally:
  # gsutil cp -r gs://google-cloud-aiplatform/schema/trainingjob/definition/ ./definition

  request_body <- structure(
    rmNullObs(list(
      displayName = displayName,
      trainingTaskDefinition = "gs://google-cloud-aiplatform/schema/trainingjob/definition/automl_tabular_1.0.0.yaml",
      trainingTaskInputs = list(
        targetColumn = c(""),
        weightColumn = weightColumn,
        predictionType = optimizationPredictionType,
        trainBudgetMilliNodeHours = budgetMilliNodeHours,
        optimizationObjective = optimizationObjective,
        transformations = column_transformations
      )
    )), class = c("gcva_automlTabularTrainingJob", "list")
  )

  request_body

}



#' @title
#' Create custom container training job
#' @param projectId
#' @param locationId
#' @param displayName
#' @param containerUri
#' @param command
#' @param model_serving_container_image_uri
#' @param model_serving_container_command
#'
#'
#'
gcva_custom_container_training_job <- function(
    projectId = gcva_project_get(),
    locationId = gcva_region_get(),
    displayName,
    containerUri,
    command,
    modelServingContainerCommand,
    modelServingContainerImageUri,
    serviceAccount = NULL) {

  # projects.locations.customJobs
  # TODO - change function name to camelCase `gcvaCustomContainerTrainingJob`
  # https://{service-endpoint}/v1/{parent}/customJobs
  # parent
  # string
  # Required. The resource name of the Location to create the CustomJob in.
  # Format: projects/{project}/locations/{location}
  # https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.customJobs/create


  # merge into request body
  customContainerTrainingJob <- structure(
    rmNullObs(
      list(
        displayName = displayName,
        jobSpec = list(
          workerPoolSpecs = list(
            containerSpec = list(
              imageUri = containerUri,
              command = command
            )
          ),
          serviceAccount = serviceAccount
        )
      )
    ), class = c("gcva_customContainerTrainingJob", "list")
  )

  customContainerTrainingJob

}




#' OBJECT
# CustomJobSpec -

# ' OBJECT
# WorkerPoolSpec - https://cloud.google.com/vertex-ai/docs/reference/rest/v1/CustomJobSpec#WorkerPoolSpec




#' Executes an training job
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.trainingPipelines/create
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#' @param job a job object of:  "gcva_automlTabularTrainingJob" OR TBD
#' @param dataset a vertex ai dataset object with class "gcva_dataset"
#' @param targetColumn STRING full column name of the model target
#' @param trainingFractionSplit decimal, percentage of dataset to use for training model during training
#' @param validationFractionSplit decimal, percentage of dataset to use for validating model during training
#' @param testFractionSplit decimal, percentage of dataset to use for testing model during training
#' @param modelDisplayName display name of the model
#' @param disableEarlyStopping disable early stopping default FALSE
#' @param sync If set to TRUE, the call will block while waiting for the asynchronous batch job to complete.
#'
#' @returns \href{https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.trainingPipelines#TrainingPipeline}{trainingPipeline} object
#'
#' @export
gcva_run_job <- function(projectId = gcva_project_get(),
                         locationId = gcva_region_get(),
                         job,
                         dataset,
                         targetColumn=NULL,
                         trainingFractionSplit=0.8,
                         validationFractionSplit=0.1,
                         testFractionSplit=0.1,
                         modelDisplayName,
                         disableEarlyStopping=FALSE,
                         sync=TRUE){

  # check if dataset object
  stopifnot(inherits(dataset, "gcva_dataset"))

  # get dataset ID from datasetBane uri
  dataset_id <- unlist(strsplit(dataset$name, "/"))[6]

  request_body_partial <- structure(
    list(
      modelToUpload = list(
        displayName = modelDisplayName
      ),
      inputDataConfig = list(
        datasetId = dataset_id
      )
    )
  )

  ## create response body for API call
  TrainingPipeline <- c(job, request_body_partial)

  TrainingPipeline <- structure(
    rmNullObs(TrainingPipeline,
              class = c("gcva_job", "list")
    )
  )

  ## set target column value
  TrainingPipeline[["trainingTaskInputs"]][["targetColumn"]] <- targetColumn

  parent <- gsub("/datasets/.*" , "", dataset$name)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/trainingPipelines",
                 locationId,
                 parent)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  stopifnot(inherits(TrainingPipeline, "gcva_job"))

  trainingPipeline <- f(the_body = TrainingPipeline)

  if(sync == FALSE) {
    #return right away
    out <- gcva_trainingPipeline(trainingPipelineName = trainingPipeline$name)
    out
  }  else if(sync == TRUE) {
    #wait until completed
    trainingPipeline <- gcva_wait_for_training_pipeline(trainingPipelineName = trainingPipeline$name)
    out <- gcva_trainingPipeline(trainingPipelineName = trainingPipeline$name)
    out
  }

}


#' Wait for a training pipeline operation
#'
#' @param locationId locationId of training pipeline
#' @param trainingPipelineName an object representing a model training pipeline
#' @param wait INTEGER number of seconds to wait between checks. Default is 5minutes
#'
#' @return trainingPipeline object
#'
#' @export
gcva_wait_for_training_pipeline <- function(locationId = gcva_region_get(),
                                            trainingPipelineName,
                                            wait=300) {

  tp <- gcva_trainingPipeline(trainingPipelineName = trainingPipelineName)
  trainingPipelineId <- unlist(strsplit(tp$name, "/"))[6]
  console_url <- sprintf(
    "https://console.cloud.google.com/vertex-ai/locations/%s/training/%s?project=%s",
    locationId, trainingPipelineId, projectId)
  myMessage("view training: ", console_url, level = 3)
  myMessage("pipeline state: ", tp$state, level = 3)

  status <- FALSE

  while(!status){
    Sys.sleep(wait)
    tp <- gcva_trainingPipeline(trainingPipelineName = trainingPipelineName)

    myMessage("view training: ", console_url, level = 3)
    myMessage("pipeline state: ", tp$state, level = 3)

    if(tp$state == "PIPELINE_STATE_SUCCEEDED" |
       tp$state == "PIPELINE_STATE_FAILED" |
       tp$state == "PIPELINE_STATE_CANCELLED"){
      status <- TRUE
    } else {
      status <- FALSE
    }
  }
  tp
}


#' Get a Training Pipeline object
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.trainingPipelines/get
#'
#' @param locationId locationId of training pipeline
#' @param trainingPipelineName trainingPipeline full name or uri
#'
#' @return a training Pipeline object
#'
#' @export
gcva_trainingPipeline <- function(locationId = gcva_region_get(),
                                  trainingPipelineName) {

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s",
                 locationId,
                 trainingPipelineName)

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f()

  out <- structure(response, class = "gcva_trainingPipeline")

  out

}
