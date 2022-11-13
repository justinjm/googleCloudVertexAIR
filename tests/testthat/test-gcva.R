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

test_that("We can fetch a list of datasets", {
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





