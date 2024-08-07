context("Setup")

options(
  googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform"
)
googleAuthR::gar_auth_service(json_file = Sys.getenv("GAR_SERVICE_JSON"))

skip_if_no_token <- function() {
  testthat::skip_if_not(googleAuthR::gar_has_token(), "No token")
}

context("Authentication")

test_that("Authentication", {
  skip_if_no_token()

  # manual auth
  expect_true(googleAuthR::gar_has_token())
})

context("Datasets")

test_that("We can get a list of datasets", {
  skip_if_no_token()

  projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
  locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
  expect_true(projectId != "")
  expect_true(locationId != "")
  l <- gcva_list_datasets(projectId, locationId)

  expect_s3_class(l, "data.frame")
  expect_true(
    all(
      names(l) %in% c("name", "displayName", "metadataSchemaUri", "createTime",
                      "updateTime", "etag", "labels", "metadata")
    )
  )
})


test_that("We can create a dataset", {
  skip_if_no_token()

  projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
  locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
  expect_true(projectId != "")
  expect_true(locationId != "")
  d <- gcva_create_tabluar_dataset(
    projectId,locationId,displayName = "test_gcva_dataset",
    gcsSource = "gs://cloud-samples-data/ai-platform-unified/datasets/tabular/california-housing-tabular-regression.csv")

  expect_s3_class(d, "gcva_dataset")
  expect_true(
    all(
      names(d) %in% c("name", "displayName", "metadataSchemaUri", "createTime",
                      "updateTime", "etag", "metadata",
                      "metadataArtifact")
    )
  )
})


test_that("We can get a dataset", {
  skip_if_no_token()

  locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
  datasetName <- Sys.getenv("GCVA_TEST_DATASET_NAME")
  expect_true(locationId != "")
  expect_true(datasetName != "")
  d <- gcva_dataset(locationId, datasetName)

  expect_s3_class(d, "gcva_dataset")
  expect_true(
    all(
      names(d) %in% c("name", "displayName", "metadataSchemaUri", "createTime",
                      "updateTime", "etag", "labels", "metadata",
                      "metadataArtifact")
    )
  )
})


test_that("We can delete a dataset", {
  skip_if_no_token()

  projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
  locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
  expect_true(projectId != "")
  expect_true(locationId != "")
  r <- gcva_delete_dataset(projectId, locationId,
                           displayName = "test_gcva_dataset")

  expect_null(r)

})


# context("trainingPipelineJobs")
#
# test_that("We can get a trainingPipelineJob", {
#   skip_if_no_token()
#
#   projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
#   locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
#   trainingPipelineName <- Sys.getenv("GCVA_TRAINING_PIPELINE")
#   expect_true(projectId != "")
#   expect_true(locationId != "")
#   expect_true(trainingPipelineName != "")
#   p <- gcva_trainingPipeline(trainingPipelineName = trainingPipelineName)
#
#   expect_s3_class(p, "gcva_trainingPipeline")
#   expect_true(
#     all(
#       names(p) %in% c("name",
#                       "displayName",
#                       "inputDataConfig",
#                       "trainingTaskDefinition",
#                       "trainingTaskInputs",
#                       "trainingTaskMetadata",
#                       "modelToUpload",
#                       "state",
#                       "createTime",
#                       "startTime",
#                       "endTime",
#                       "updateTime")
#     )
#   )
# }
# )


context("Models")

test_that("We can fetch a list of Models", {
  skip_if_no_token()

  projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
  locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
  expect_true(projectId != "")
  expect_true(locationId != "")
  l <- gcva_list_models(projectId, locationId)

  expect_s3_class(l, "data.frame")
  expect_true(
    all(
      names(l) %in% c("name", "displayName", "predictSchemata",
                      "metadataSchemaUri",
                      "metadata","trainingPipeline",
                      "supportedDeploymentResourcesTypes",
                      "supportedInputStorageFormats",
                      "supportedOutputStorageFormats",
                      "createTime",  "updateTime", "etag",
                      "supportedExportFormats","explanationSpec",
                      "versionId","versionAliases",
                      "versionCreateTime","versionUpdateTime",
                      "modelSourceInfo","containerSpec", "artifactUri"
      )
    )
  )
})


test_that("We can get a Model", {
  skip_if_no_token()

  locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
  modelName <- Sys.getenv("GCVA_TEST_MODEL_NAME_AUTOML")
  expect_true(locationId != "")
  expect_true(modelName != "")
  m <- gcva_model(locationId,modelName)

  expect_s3_class(m, "gcva_model")
  expect_true(
    all(
      names(m) %in% c("name", "displayName", "predictSchemata",
                      "metadataSchemaUri",
                      "metadata","trainingPipeline",
                      "supportedDeploymentResourcesTypes",
                      "supportedInputStorageFormats",
                      "supportedOutputStorageFormats",
                      "createTime",  "updateTime", "etag",
                      "supportedExportFormats","explanationSpec",
                      "versionId","versionAliases",
                      "versionCreateTime","versionUpdateTime",
                      "modelSourceInfo"
      )
    )
  )
})


context("Batch Prediction Jobs")

test_that("We can get a batchPredictionJob", {
  skip_if_no_token()

  projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
  locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
  batchPredictionJobName <- Sys.getenv("GCVA_BATCH_PREDICTION")
  expect_true(projectId != "")
  expect_true(locationId != "")
  expect_true(batchPredictionJobName != "")
  p <- gcva_batch_prediction_job(batchPredictionJob = batchPredictionJobName)

  expect_s3_class(p, "gcva_batchPredictionJob")
  expect_true(
    all(
      names(p) %in% c("name",
                      "displayName",
                      "model",
                      "inputConfig",
                      "outputConfig",
                      "dedicatedResources",
                      "manualBatchTuningParameters",
                      "outputInfo",
                      "state",
                      "completionStats",
                      "createTime",
                      "startTime",
                      "endTime",
                      "updateTime",
                      "labels",
                      "modelVersionId")
    )
  )
}
)


# context("Endpoints")

# test_that("We can get a list of endpoints", {
#   skip_if_no_token()

#   projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
#   locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
#   expect_true(projectId != "")
#   expect_true(locationId != "")
#   l <- gcva_list_endpoints(projectId, locationId)

#   expect_s3_class(l, "data.frame")
#   expect_true(
#     all(
#       names(l) %in% c("name", "displayName", "metadataSchemaUri", "createTime",
#                       "updateTime", "etag", "labels", "metadata")
#     )
#   )
# })


# test_that("We can create an endpoint", {
#   skip_if_no_token()

#   projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
#   locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
#   expect_true(projectId != "")
#   expect_true(locationId != "")
#   d <- gcva_create_endpoint(
#     displayName = "test_gcva_dataset"

#   expect_s3_class(d, "gcva_endpoint")
#   expect_true(
#     all(
#       names(d) %in% c("name", "displayName", "metadataSchemaUri", "createTime",
#                       "updateTime", "etag", "labels", "metadata",
#                       "metadataArtifact")
#     )
#   )
# })


# test_that("We can get an endpoint", {
#   skip_if_no_token()

#   locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
#   datasetName <- Sys.getenv("GCVA_TEST_DATASET_NAME")
#   expect_true(locationId != "")
#   expect_true(datasetName != "")
#   d <- gcva_endpoint(locationId, XXXXXX)

#   expect_s3_class(d, "gcva_endpoint")
#   expect_true(
#     all(
#       names(d) %in% c("name", "displayName", "metadataSchemaUri", "createTime",
#                       "updateTime", "etag", "labels", "metadata",
#                       "metadataArtifact")
#     )
#   )
# })


# test_that("We can delete an endpoint", {
#   skip_if_no_token()

#   projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")
#   locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
#   expect_true(projectId != "")
#   expect_true(locationId != "")
#   r <- gcva_delete_endpoint(projectId, locationId,
#                            XXXXXXXXX)

#   expect_null(r)

# })

#  to get list of names easily copy/paste-able from R console, use:
# cat(names(OBJECT), sep=", ")
