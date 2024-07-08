#' Lists datasets in a project.
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#'
#' @return a list of datasets
#'
#' @export
gcva_list_datasets <- function(projectId = gcva_project_get(),
                               locationId = gcva_region_get()) {

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/datasets",
                 locationId,
                 parent)

  parse_ld <- function(x) {
    x <- x$datasets
    x$createTime <- timestamp_to_r(x$createTime)

    x

  }

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = parse_ld)
  response <- f()

  out <- response[, c("displayName",
                      "name",
                      "metadataSchemaUri",
                      "createTime",
                      "updateTime",
                      "etag",
                      "metadata",
                      "metadataArtifact")]
  out

}

#' Get a dataset object
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.datasets/get
#'
#' @param locationId locationId of operation
#' @param datasetName the full name of the dataset object to get
#'
#' @return a Dataset object
#'
#' @export
gcva_dataset <- function(locationId = gcva_region_get(),
                         datasetName){

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s",
                 locationId,
                 datasetName)

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f()

  out <- structure(response, class = "gcva_dataset")

  out

}


#' Creates a tabluar dataset
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#' @param displayName the name of the dataset that is shown in the interface.
#' The name can be up to 32 characters long and can consist only of ASCII
#' Latin letters A-Z and a-z, underscores (_), and ASCII digits 0-9.
#'
#' @export
gcva_create_tabluar_dataset <- function(projectId = gcva_project_get(),
                                        locationId = gcva_region_get(),
                                        displayName,
                                        gcsSource) {

  existing_datasets <- gcva_list_datasets(projectId, locationId)

  if(displayName %in% existing_datasets$displayName) {
    stop("Existing dataset already exists, must specify new, unique name.")
  }

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  Dataset <- structure(
    list(
      display_name = displayName,
      metadata_schema_uri = "gs://google-cloud-aiplatform/schema/dataset/metadata/tabular_1.0.0.yaml",
      metadata = list(
        input_config = list(
          gcs_source = list(
            uri = list(
              gcsSource
            )
          )
        )
      )
    )
    ,class = c("gcva_createDatasetRequest", "list")
  )

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/datasets",
                 locationId,
                 parent)

  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f(the_body = Dataset)

  response <- gcva_wait_for_op(operation = response$name)

  out  <- gcva_dataset(datasetName = response$response$name)

  out

}


#' Deletes a dataset
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#' @param displayName the name of the dataset that is shown in the interface.
#' @param dataset dataset object returned by gcva_dataset function
#'
#' @export
gcva_delete_dataset <- function(projectId = gcva_project_get(),
                                locationId = gcva_region_get(),
                                displayName = NULL,
                                dataset) {

  # if not using display name, use dataset object to delete
  if(is.null(displayName)) {

    name <- dataset$name

  } else {
    datasets_list <- gcva_list_datasets(projectId = projectId,
                                        locationId = locationId)

    dataset_display_name <- displayName

    name <- subset(datasets_list,
                   displayName == dataset_display_name,
                   select = c(name))

    if (dim(name)[1] == 0) {
      stop(sprintf("Dataset %s does not exist. Please check the dataset displayname is correct and try again.",
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
    myMessage("Dataset successfully deleted.", level = 3)

  } else {
    out
  }

}
