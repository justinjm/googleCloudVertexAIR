#' Set the region
#'
#' Can also use environment argument VAIR_DEFAULT_REGION
#'
#' Current
#' @param region Region for the endpoint, full list of service endpoints
#' [here](https://cloud.google.com/vertex-ai/docs/reference/rest#service-endpoint)
#' @import assertthat
#' @export
vair_region_set <- function(region = c("us-central1",
                                       "us-east1",
                                       "us-east4",
                                       "us-west1",
                                       "northamerica-northeast1",
                                       "europe-west1",
                                       "europe-west2",
                                       "europe-west4",
                                       "asia-east1",
                                       "asia-northeast1",
                                       "asia-northeast3",
                                       "asia-southeast1",
                                       "australia-southeast1")){

  region <- match.arg(region)

  .vair_env$region <- region

  myMessage("Region set to '", .vair_env$region, "'", level = 3)
  return(invisible(.vair_env$region))
}

#' Get Region Set
#'
#' @export
vair_region_get <- function(){

  if(!is.null(.vair_env$region)){
    return(.vair_env$region)
  }

  if(Sys.getenv("VAIR_DEFAULT_REGION") != ""){
    .vair_env$region <- Sys.getenv("VAIR_DEFAULT_REGION")
  }
  if(is.null(.vair_env$region)){
    stop("No region set - use vair_region_set() or env arg VAIR_DEFAULT_REGION",
         call. = FALSE)
  }
  .vair_env$region
}

#' Set the projectId for your Vertex AI services
#'
#' Can also use environment argument VAIR_DEFAULT_PROJECT_ID
#'
#' @param projectId The projectId
#' @import assertthat
#' @export
vair_project_set <- function(projectId){

  .vair_env$project <- projectId

  myMessage("ProjectId set to '", .vair_env$project, "'", level = 3)
  return(invisible(.vair_env$project))
}

#' Get ProjectId
#'
#' @export
vair_project_get <- function(){

  if(!is.null(.vair_env$project)){
    return(.vair_env$project)
  }

  if(Sys.getenv("VAIR_DEFAULT_PROJECT_ID") != ""){
    .vair_env$project <- Sys.getenv("VAIR_DEFAULT_PROJECT_ID")
  }
  if(is.null(.vair_env$project)){
    stop("No projectId set - use vair_project_set() or env arg VAIR_DEFAULT_PROJECT_ID",
         call. = FALSE)
  }
  .vair_env$project
}
