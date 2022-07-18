#' Constructs an AutoML Tabular Training Job
#'
#' @param displayName STRING Required. The user-defined name of this TrainingPipeline.
#'
#' @export
gcva_automl_tabluar_training_job <- function(
  displayName,
  optimizationPredictionType = c("regression", "classification"),
  column_transformations=NULL,
  budgetMilliNodeHours=1000) {

  # set prediction type from available list
  optimizationPredictionType <- match.arg(optimizationPredictionType)


  request_body <- structure(
    list(
      displayName = displayName,
      trainingTaskDefinition = "gs://google-cloud-aiplatform/schema/trainingjob/definition/automl_tabular_1.0.0.yaml",
      trainingTaskInputs = list(
        targetColumn = c(""),
        predictionType = optimizationPredictionType,
        trainBudgetMilliNodeHours = budgetMilliNodeHours,
        transformations = list(
          numeric = list(columnn_name = "V1"),
          categorical = list(columnn_name = "V2"),
          categorical = list(columnn_name = "V3"),
          categorical = list(columnn_name = "V4"),
          categorical = list(columnn_name = "V5"),
          numeric = list(columnn_name = "V6"),
          categorical = list(columnn_name = "V7"),
          categorical = list(columnn_name = "V8"),
          categorical = list(columnn_name = "V9"),
          numeric = list(columnn_name = "V10"),
          categorical = list(columnn_name = "V11"),
          numeric = list(columnn_name = "V12"),
          numeric = list(columnn_name = "V13"),
          numeric = list(columnn_name = "V14"),
          numeric = list(columnn_name = "V15"),
          categorical = list(columnn_name = "V16")
        )
      )
    )
    , class = c("gcva_automlTabularTrainingJob", "list")
  )

  request_body

}

#' Executes an training job
#'
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

  # DEV - BREAKPOINT ##########################################################
  browser()
  ##########################################################

  # get existing datasets to grab datasetID for job submission
  datasets_list <- gcva_list_datasets(projectId, locationId)

  dataset_display_name <- dataset

  datasets_list_name <- subset(datasets_list,
                               displayName == dataset_display_name,
                               select = c(name))

  if (dim(datasets_list_name)[1] == 0) {
    stop(sprintf("Dataset %s does not exist. Please check the dataset displayname is correct and try again.",
                 displayName))
  }

  # get dataset ID from url since not sure how else?
  dataset_id <- gsub(".*/datasets/" , "", datasets_list_name$name)

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

  parent <- gsub("/datasets/.*" , "", datasets_list_name$name)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/trainingPipelines",
                 locationId,
                 parent)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  stopifnot(inherits(TrainingPipeline, "gcva_job"))

  response <- f(the_body = TrainingPipeline)

  out <- response

}
