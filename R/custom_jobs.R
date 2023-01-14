
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
              imageUri = imageUri,
              command = command,
            )
          ),
          serviceAccount = serviceAccount
        )
      )
    ), class = c("gcva_customContainerTrainingJob", "list")
  )

  customContainerTrainingJob

}

#'
gcva_runCustomContainerJob <- function( {
  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/customJobs",
                 locationId,
                 parent)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  stopifnot(inherits(customContainerTrainingJob, "gcva_customContainerTrainingJob"))

  # response <- f(the_body = customContainerTrainingJob)
  # response
}



#' OBJECT
# CustomJobSpec -

# ' OBJECT
# WorkerPoolSpec - https://cloud.google.com/vertex-ai/docs/reference/rest/v1/CustomJobSpec#WorkerPoolSpec

