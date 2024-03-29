#' Wait for an Operation
#'
#' @param locationId locationId of operation
#' @param operation A operation object
#' @param wait the number of seconds to wait between checks
#' Use this function to do a loop to check progress of a operation running
#'
#' @return After a while, a completed operation
#'
#' @export
gcva_wait_for_op <- function(locationId = gcva_region_get(),
                             operation,
                             wait=2) {

  status <- FALSE
  time <- Sys.time()

  while(!status){
    Sys.sleep(wait)
    myMessage("Waiting ", wait, " seconds...", level = 3)

    operation <- gcva_get_op(locationId, operationId = operation)

    if(operation$done == "TRUE"){
      status <- TRUE
    } else {
      status <- FALSE
    }
  }

  operation
}


#' Poll an operationId
#' https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.operations/get
#' @param locationId locationId of operation
#' @param operationId operationId to poll
#'
#' @return an operation object
#'
#' @export
gcva_get_op <- function(locationId = gcva_region_get(),
                        operationId) {

  url <- sprintf("https://%s-aiplatform.googleapis.com/v1/%s",
                 locationId,
                 operationId)

  f <- googleAuthR::gar_api_generator(url,
                                      "GET",
                                      data_parse_function = function(x) x,
                                      checkTrailingSlash = FALSE)

  response <- f()

  response

}

