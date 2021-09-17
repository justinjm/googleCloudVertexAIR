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