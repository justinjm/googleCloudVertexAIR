#' @export
print.gcva_dataset <- function(x,...){
  cat("==Google Cloud Vertex AI Dataset==\n")
  cat0("name:                ", x$name)
  cat0("displayName:         ", x$displayName)
  cat0("createTime:          ", as.character(timestamp_to_r(x$createTime)))
  cat0("gcsSource:           ", x$metadata$inputConfig$gcsSource$uri)
 }

#' @export
print.gcva_automlTabularTrainingJob <- function(x,...){
  cat("==Google Cloud Vertex AI AutoML Tabular Training Job==\n")
  cat0("displayName:         ", x$displayName)
  cat0("predictionType:       ", x[["trainingTaskInputs"]][["predictionType"]])
  cat0("datasetId:           ", x[["inputDataConfig"]][["datasetId"]])
}
