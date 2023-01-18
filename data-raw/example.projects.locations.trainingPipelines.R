json_obj <- list(
  displayName = "TRAINING_PIPELINE_NAME",
  inputDataConfig = list(
    datasetId = "DATASET_ID",
    annotationsFilter = "ANNOTATIONS_FILTER",
    annotationSchemaUri = "ANNOTATION_SCHEMA_URI",
    fractionSplit = list(
      trainingFraction = "TRAINING_FRACTION",
      validationFraction = "VALIDATION_FRACTION",
      testFraction = "TEST_FRACTION"
    ),
    filterSplit = list(
      trainingFilter = "TRAINING_FILTER",
      validationFilter = "VALIDATION_FILTER",
      testFilter = "TEST_FILTER"
    ),
    predefinedSplit = list(
      key = "PREDEFINED_SPLIT_KEY"
    ),
    timestampSplit = list(
      trainingFraction = "TIMESTAMP_TRAINING_FRACTION",
      validationFraction = "TIMESTAMP_VALIDATION_FRACTION",
      testFraction = "TIMESTAMP_TEST_FRACTION",
      key = "TIMESTAMP_SPLIT_KEY"
    ),
    gcsDestination = list(
      outputUriPrefix = "OUTPUT_URI_PREFIX"
    )
  ),
  trainingTaskDefinition = "gs://google-cloud-aiplatform/schema/trainingjob/definition/custom_task_1.0.0.yaml",
  trainingTaskInputs = list(
    workerPoolSpecs = list(
      list(
        machineSpec = list(
          machineType = "MACHINE_TYPE",
          acceleratorType = "ACCELERATOR_TYPE",
          acceleratorCount = "ACCELERATOR_COUNT"
        ),
        replicaCount = "REPLICA_COUNT",
        containerSpec = list(
          imageUri = "CUSTOM_CONTAINER_IMAGE_URI",
          command = list("CUSTOM_CONTAINER_COMMAND"),
          args = list("CUSTOM_CONTAINER_ARGS")
        ),
        pythonPackageSpec = list(
          executorImageUri = "PYTHON_PACKAGE_EXECUTOR_IMAGE_URI",
          packageUris = list("PYTHON_PACKAGE_URIS"),
          pythonModule = "PYTHON_MODULE",
          args = list("PYTHON_PACKAGE_ARGS")
        )
      )
    ),
    scheduling = list(
      TIMEOUT = "TIMEOUT"
    )
  ),
  modelToUpload = list(
    displayName = "MODEL_NAME",
    predictSchemata = list(),
    containerSpec = list(
      imageUri = "IMAGE_URI"
    )
  ),
  labels = list(
    LABEL_NAME_1 = "LABEL_VALUE_1",
    LABEL_NAME_2 = "LABEL_VALUE_2"
  )
)
