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
      # inputDataConfig = list(), # below in job Run?
      trainingTaskDefinition = "gs://google-cloud-aiplatform/schema/trainingjob/definition/automl_tabular_1.0.0.yaml",
      trainingTaskInputs = list(
        targetColumn = c("column_name"),
        predictionType = optimizationPredictionType,
        transformations = list(
          transformations
        ),
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
#'
gcva_run_job <- function(job,
                         dataset,
                         targetColumn,
                         modelDisplayName){

  # get existing datasets to grab datasetID for job submission
  existing_datasets <- gcva_list_datasets(projectId, locationId)

  dataset_display_name <- displayName

  name <- subset(datasets_list,
                 displayName == dataset_display_name,
                 select = c(name))

  if (dim(name)[1] == 0) {
    stop(sprintf("Dataset %s does not exist. Please check the dataset displayname is correct and try again.",
                 displayName))
  }

  request_body <- structure(
    list(
      # inputDataConfig = list(), # below in job Run?
    )

  )

}
