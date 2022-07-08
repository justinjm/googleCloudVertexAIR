#' #' @export
#' print.gcva_dataset <- function(x,...){
#'   cat("==Google Cloud Vertex AI Dataset==\n")
#'   cat0("displayName:         ", x$displayName)
#'   cat0("exampleCount:        ", x$exampleCount)
#'   cat0("createTime:          ", as.character(timestamp_to_r(x$createTime)))
#'   cat0("primaryTableSpecId:  ", x[["tablesDatasetMetadata"]][["primaryTableSpecId"]])
#'   cat0("targetColumnSpecId:  ", x[["tablesDatasetMetadata"]][["targetColumnSpecId"]])
#'   cat0("statsUpdateTime:     ", as.character(timestamp_to_r(x[["tablesDatasetMetadata"]][["statsUpdateTime"]])))
#' }


#' #' @export
#' print.gcva_automl_tabluar_training_job <- function(x,...){
#'   cat("==Google Cloud Vertex AI AutoML Tabular Training Job==\n")
#'   cat0("name:                ", x$name)
#'   cat0("createTime:          ", as.character(timestamp_to_r(x[["metadata"]][["createTime"]])))
#'   cat0("updateTime:          ", as.character(timestamp_to_r(x[["metadata"]][["updateTime"]])))
#'
#' }
