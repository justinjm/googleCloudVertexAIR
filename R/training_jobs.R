#' Constructs an AutoML Tabular Training Job
#'
#' @param displayName STRING Required. The user-defined name of this TrainingPipeline.
#'
#' @export
gcva_automl_tabluar_training_job <- function(
  displayName,
  optimizationPredictionType = c("regression", "classification"),
  transformations=NULL,
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
        transformations = transformations,
        budgetMilliNodeHours = budgetMilliNodeHours
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

  # get existing datasets to grab datasetID for job submission
  datasets_list <- gcva_list_datasets(projectId, locationId)

  dataset_display_name <- dataset

  parent <- subset(datasets_list,
                    displayName == dataset_display_name,
                    select = c(name))

  if (dim(parent)[1] == 0) {
    stop(sprintf("Dataset %s does not exist. Please check the dataset displayname is correct and try again.",
                 displayName))
  }

  # get dataset ID from url since not sure how else?
  # TODO - FIX THIS ??
  dataset_id <- gsub(".*/datasets/" , "", parent$name)

  request_body_partial <- structure(
    list(
      inputDataConfig = list(
        datasetId = dataset_id)
    ),
    class = c("gcva_job", "list")
  )

  ## create response body for API call
  request_body <- c(job, request_body_partial)

  ## set target column value
  request_body[["trainingTaskInputs"]][["targetColumn"]] <- targetColumn

  ## return body for debugging during package dev #############################
  request_body

  # TODO - finalize api call
  # "https://{service-endpoint}/v1/{parent}/trainingPipelines"
  # https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.trainingPipelines/create

  url <- sprintf("https://%s/v1/%s/trainingPipelines",
                 locationId,
                 parent)
  url

  # f <- googleAuthR::gar_api_generator(
  #   url,
  #   "POST",
  #   data_parse_function = function(x) x)
  #
  # stopifnot(inherits(request_body, "gcva_job"))
  #
  # response <- f(the_body = request_body)
  #
  # out <- response

}
