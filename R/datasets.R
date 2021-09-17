


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

  # https://us-central1-aiplatform.googleapis.com/v1/{parent}/datasets
  
  url <- sprintf("https://automl.googleapis.com/v1beta1/%s/datasets",
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

  out <- response

  out[, c("displayName", "createTime", "etag", "name")]

}