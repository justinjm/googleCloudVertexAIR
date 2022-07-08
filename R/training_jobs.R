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
      # inputDataConfig = list(), # below in run_job
      trainingTaskDefinition = "gs://google-cloud-aiplatform/schema/trainingjob/definition/automl_tabular_1.0.0.yaml",
      trainingTaskInputs = list(
        targetColumn = c(""),
        predictionType = optimizationPredictionType,
        transformations = transformations,
        budgetMilliNodeHours = budgetMilliNodeHours
      )
    ), class = c("gcva_automl_tabluar_training_job", "list")
  )

  request_body

}

#'
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

  # get dataset id
  # insert value in trainingTaskInputs `targetColumn`

  # get existing datasets to grab datasetID for job submission
  datasets_list <- gcva_list_datasets(projectId, locationId)

  dataset_display_name <- dataset

  dataset <- subset(datasets_list,
                 displayName == dataset_display_name,
                 select = c(name))

  # TODO - update for this function
  # if (dim(name)[1] == 0) {
  #   stop(sprintf("Dataset %s does not exist. Please check the dataset displayname is correct and try again.",
  #                displayName))
  # }

  # get dataset ID from url since not sure how else?
  dataset_id <- gsub(".*/datasets/" , "", dataset$name)

  request_body_partial <- structure(
    list(
      inputDataConfig = list(
        datasetId = dataset_id)
    ),
    class = c("gcva_automl_tabluar_training_job", "list")
  )

  ## create response body for API call
  request_body <- c(job, request_body_partial)

  ## set target column value
  request_body[["trainingTaskInputs"]][["targetColumn"]] <- targetColumn

  ## return body for debugging during package dev #############################
  request_body

  # TODO - fix printing of output
  # structure(request_body, class = "gcva_automl_tabluar_training_job")



  # url <- sprintf("https://automl.googleapis.com/v1beta1/%s/models",
  #                parent)
  #
  # f <- googleAuthR::gar_api_generator(
  #   url,
  #   "POST",
  #   data_parse_function = function(x) x)
  #
  # # stopifnot(inherits(request_body, "gcva_automl_tabluar_training_job"))
  #
  # response <- f(the_body = request_body)
  #
  # out <- response

}
