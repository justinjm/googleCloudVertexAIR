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
                      "updateTime", "etag", "labels", "metadata")
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
                      "updateTime", "etag", "labels", "metadata")
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
                      "modelSourceInfo"
      )
    )
  )
})


test_that("We can get a Model", {
  skip_if_no_token()

  locationId <- Sys.getenv("GCVA_DEFAULT_REGION")
  modelName <- Sys.getenv("GCVA_TEST_MODEL_NAME")
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
