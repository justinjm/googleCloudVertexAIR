#' Lists information about the supported locations
#'
#' @param projectId GCP project Id to fetch a list of locations from
#'
#' @export
#'
#' @family locations functions
vair_list_locations <- function(projectId = vair_project_get()) {

    url <- sprintf("https://automl.googleapis.com/v1beta1/projects/%s/locations",
                   projectId)

    f <- googleAuthR::gar_api_generator(url,
                                        "GET",
                                        # pars_args = rmNullObs(pars),
                                        data_parse_function = function(x) x)
    response <- f()

    out <- response$locations

    out

}

#' Get information about a location
#'
#' @param projectId GCP project id
#' @param locationId location of GCP resources, retrieved by
#' \link{vair_list_locations}
#'
#' @export
#'
#' @family loctions functions
vair_get_location <- function(projectId = vair_project_get(),
                              locationId = vair_region_get()) {

    locations <- vair_list_locations(projectId = projectId)

    # change since subset doesn't like input same as column name?
    location_id <- locationId

    name <- subset(locations,
                   locationId == location_id,
                   select = c(name))

    url <- sprintf("https://automl.googleapis.com/v1beta1/%s",
                   name)

    f <- googleAuthR::gar_api_generator(url,
                                        "GET",
                                        data_parse_function = function(x) x)

    response <- f()

    out <- response

    structure(out, class = "vair_location")

}


