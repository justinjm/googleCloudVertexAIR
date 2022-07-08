#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#' @param displayName STRING Required. The user-defined name of this TrainingPipeline.
#'
#' @export
gcva_automl_tabluar_training_job <- function(projectId = gcva_project_get(),
                                             locationId = gcva_region_get(),
                                             displayName) {

  request_body <- structure(
    list(
      displayName = displayName,
      # inputDataConfig = list(), # below in job Run?
      trainingTaskDefinition = "gs://google-cloud-aiplatform/schema/trainingjob/definition/automl_tabular_1.0.0.yaml",
      # trainingTaskInputs = list(
      #   targetColumn = ,
      #   predictionType = ,
      #   transformations = list(),
      #   budgetMilliNodeHours
      # )



    )
  )

  out <- request_body

  structure(out, class = "gcva_automl_tabluar_training_job")


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

}
