#' Constructs an AutoML Tabular Training Job
#'
#' @param displayName STRING Required. The user-defined name of this TrainingPipeline.
#' @param weightColumn
#' @param optimizationPredictionType
#' @param budgetMilliNodeHours
#' @param optimizationObjective
#' @param column_transformations
#'
#'
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


#' Executes a custom container training job
#'
#'
#'
#'
#'
#'
# gcva_custom_container_training_job <- function(){
#
# }


#' Executes an training job
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.trainingPipelines/create
#'
#' @param projectId
#' @param locationId
#' @param job
#' @param dataset
#' @param targetColumn STRING
#' @param trainingFractionSplit
#' @param validationFractionSplit
#' @param testFractionSplit
#' @param modelDisplayName
#' @param disableEarlyStopping
#' @param wait wait for model training to complete before completing function
#'
#' @returns model object
#'
#' @export
gcva_run_job <- function(projectId = gcva_project_get(),
                         locationId = gcva_region_get(),
                         job,
                         dataset,
                         targetColumn,
                         trainingFractionSplit=0.8,
                         validationFractionSplit=0.1,
                         testFractionSplit=0.1,
                         modelDisplayName,
                         disableEarlyStopping=FALSE,
                         wait=TRUE){

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

  TrainingPipeline <- structure(TrainingPipeline,
                                class = c("gcva_job", "list"))

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

  # trainingPipeline <- f(the_body = TrainingPipeline)

  trainingPipeline <- gcva_wait_for_training_pipeline(trainingPipelineName = trainingPipeline$name)
  out <- gcva_trainingPipeline(trainingPipelineName = trainingPipeline$name)

  out

  # structure(trainingPipeline, class = c("gcva_trainingPipeline"))
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
                                            wait=5) {

  tp_obj <- gcva_trainingPipeline(trainingPipelineName = trainingPipelineName)
  trainingPipelineId <- unlist(strsplit(tp_obj$name, "/"))[6]
  tp_console_url <- sprintf(
    "https://console.cloud.google.com/ai/platform/locations/%s/training/%s?project=%s",
    locationId, trainingPipelineId, projectId)

  myMessage("view training:\n", tp_console_url, level = 3)

  status <- FALSE
  time <- Sys.time()

  while(!status){
    Sys.sleep(wait)
    tp <- gcva_trainingPipeline(trainingPipelineName = trainingPipelineName)

    myMessage("view training:\n", tp_console_url, level = 3)
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
