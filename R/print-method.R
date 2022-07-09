#' @export
print.gcva_dataset <- function(x,...){
  cat("==Google Cloud Vertex AI Dataset==\n")
  cat0("displayName:         ", x$displayName)
  cat0("createTime:          ", as.character(timestamp_to_r(x[["metadata"]][["genericMetadata"]][["createTime"]])))
  cat0("DatasetId:           ", gsub(".*datasets/(.*?)/.*", "\\1", x$name))
 }

#' @export
print.gcva_automlTabularTrainingJob <- function(x,...){
  cat("==Google Cloud Vertex AI AutoML Tabular Training Job==\n")
  cat0("Display Name:           ", x$displayName)
  cat0("Prediction Type:        ", x[["trainingTaskInputs"]][["predictionType"]])
  cat0("Target Column:          ", x[["trainingTaskInputs"]][["targetColumn"]])
  cat0("DatasetID:              ", x[["inputDataConfig"]][["datasetId"]])
}
