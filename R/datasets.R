#' Lists datasets in a project.
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#'
#' @export
vair_list_datasets <- function(projectId = vair_project_get(),
                               locationId = vair_region_get()) {

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  url <- sprintf("https://us-central1-aiplatform.googleapis.com/v1/%s/datasets",
                 parent)

  parse_ld <- function(x) {
    x <- x$datasets
    x$createTime <- timestamp_to_r(x$createTime)

    x

  }

  # browser()

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = parse_ld)

  response <- f()

  out <- response

  out[, c("displayName", "createTime", "etag", "name")]

}

#' Creates a dataset
#'
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#' @param displayName the name of the dataset that is shown in the interface.
#' The name can be up to 32 characters long and can consist only of ASCII
#' Latin letters A-Z and a-z, underscores (_), and ASCII digits 0-9.
#'
#' @export
vair_create_dataset <- function(projectId = vair_project_get(),
                                locationId = vair_region_get(),
                                displayName) {

  existing_datasets <- vair_list_datasets(projectId, locationId)

  if(displayName %in% existing_datasets$displayName) {
    stop("Existing dataset already exists, must specify new, unique name.")
  }

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  Dataset <- sprintf('{"displayName": "%s","tablesDatasetMetadata": { }}',
                     displayName)
  # GCS
  import_data_request <- structure(
    list(
      inputConfig = list(
        params = list(
          schema_inference_version = "1"
        ),
        gcsSource = list(
          inputUris = input_url
        )
      )
    )
    # ,class = c("gar_ImportDataRequest", "list")
  )







  url <- sprintf("https://us-central1-aiplatform.googleapis.com/v1/%s/datasets",
                 parent)

  # automl.projects.locations.datasets.create
  f <- googleAuthR::gar_api_generator(url,
                                      "POST",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f(the_body = Dataset)

  out <- response

  structure(out, class = "vair_dataset")

}
