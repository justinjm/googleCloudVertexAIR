#' Lists models in a project.
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources
#'
#' @family Models
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
#' @param model the full name of the model object to get OR
#' a trainingPipelineJob object
#'
#' @return a Model object
#'
#' @family Models
#'
#' @export
gcva_model <- function(locationId = gcva_region_get(),
                       model){

  if(inherits(model, "gcva_trainingPipeline")){
    # if passing trainingPipeine objectm get from object
    model <- model$modelToUpload$name
  }


  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s",
                 locationId,
                 model)

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f()

  out <- structure(response, class = "gcva_model")

  out

}
