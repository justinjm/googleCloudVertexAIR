#' Lists models in a project.
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#'
#' @export
gcva_list_models <- function(projectId = gcva_project_get(),
                             locationId = gcva_region_get()) {

  parent <- sprintf("projects/%s/locations/%s",
                    projectId,
                    locationId)

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s/models",
                 locationId,
                 parent)

  parse_ld <- function(x) {
    x <- x$models
    x$createTime <- timestamp_to_r(x$createTime)

    x

  }

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = parse_ld)
  response <- f()

  out <- response

  out

}


#' Get a Model object
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.models/get
#'
#' @param locationId locationId of operation
#' @param Model the full name of the model object to get
#'
#' @return a Model object
#'
#' @export
gcva_model <- function(locationId = gcva_region_get(),
                       modelName){

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s",
                 locationId,
                 modelName)

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f()

  out <- structure(response, class = "gcva_model")

  out

}
