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

#' @export
print.gcva_trainingPipeline <- function(x,...){
  locationId <- unlist(strsplit(x$name, "/"))[4]
  projectId <- unlist(strsplit(x$name, "/"))[2]
  trainingPipelineId <- unlist(strsplit(x$name, "/"))[6]
  console_url <- sprintf(
    "https://console.cloud.google.com/vertex-ai/locations/%s/training/%s?project=%s",
    locationId, trainingPipelineId, projectId)
  cat("==Google Cloud Vertex AI TrainingPipeline Job==\n")
  cat0("console:             ", console_url)
  cat0("state:               ", x$state)
  # cat0("name:                ", x$name)
  # cat0("displayName:         ", x$displayName)
  # cat0("createTime:          ", as.character(timestamp_to_r(x$createTime)))
  # cat0("startTime:           ", as.character(timestamp_to_r(x$startTime)))


}

#' @export
print.gcva_model <- function(x,...){
  cat("==Google Cloud Vertex AI Model==\n")
  cat0("name:                ", x$name)
  cat0("displayName:         ", x$displayName)
  cat0("createTime:          ", as.character(timestamp_to_r(x$createTime)))
  cat0("versionId:           ", x$versionId)
  cat0("versionAlias:        ", x$versionAlias)
}
