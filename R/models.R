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
