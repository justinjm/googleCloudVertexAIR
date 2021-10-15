#' Set the region
#'
#' Can also use environment argument GCVA_DEFAULT_REGION
#'
#' Current
#' @param region Region for the endpoint, full list of service endpoints
#' [here](https://cloud.google.com/vertex-ai/docs/reference/rest#service-endpoint)
#' @import assertthat
#' @export
gcva_region_set <- function(region = c("us-central1",
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

  .gcva_env$region <- region

  myMessage("Region set to '", .gcva_env$region, "'", level = 3)
  return(invisible(.gcva_env$region))
}

#' Get Region Set
#'
#' @export
gcva_region_get <- function(){

  if(!is.null(.gcva_env$region)){
    return(.gcva_env$region)
  }

  if(Sys.getenv("GCVA_DEFAULT_REGION") != ""){
    .gcva_env$region <- Sys.getenv("GCVA_DEFAULT_REGION")
  }
  if(is.null(.gcva_env$region)){
    stop("No region set - use gcva_region_set() or env arg GCVA_DEFAULT_REGION",
         call. = FALSE)
  }
  .gcva_env$region
}

#' Set the projectId for your Vertex AI services
#'
#' Can also use environment argument GCVA_DEFAULT_PROJECT_ID
#'
#' @param projectId The projectId
#' @import assertthat
#' @export
gcva_project_set <- function(projectId){

  .gcva_env$project <- projectId

  myMessage("ProjectId set to '", .gcva_env$project, "'", level = 3)
  return(invisible(.gcva_env$project))
}

#' Get ProjectId
#'
#' @export
gcva_project_get <- function(){

  if(!is.null(.gcva_env$project)){
    return(.gcva_env$project)
  }

  if(Sys.getenv("GCVA_DEFAULT_PROJECT_ID") != ""){
    .gcva_env$project <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
  }
  if(is.null(.gcva_env$project)){
    stop("No projectId set - use gcva_project_set() or env arg GCVA_DEFAULT_PROJECT_ID",
         call. = FALSE)
  }
  .gcva_env$project
}
