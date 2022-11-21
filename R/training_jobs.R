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
#' @param
#'
#'
#'
#'
#' @export
# gcva_custom_container_training_job <- function(){
#
# }


#' Executes an training job
#' @param projectId
#' @param locationId
#' @param job
#' @param dataset
#' @param
#'
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
                         disableEarlyStopping=FALSE){

  # check if dataset object
  stopifnot(inherits(dataset, "gcva_dataset"))

  # get dataset ID from url since not sure how else?
  # https://cloud.google.com/apis/design/resource_names
  # dataset_id <- gsub(".*/datasets/" , "", dataset$name)
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

  response <- f(the_body = TrainingPipeline)

  out <- structure(response, class = c("gcva_trainingPipeline"))

  out

}


