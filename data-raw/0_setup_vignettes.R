# 0_setup_vingettes.R
# for creating datasets in GCP for vingettes

# load packages
library(googleAuthR)
library(bigQueryR)
library(glue)

options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")

projectId <- Sys.getenv("GCVA_DEFAULT_PROJECT_ID")

bqr_auth(json_file = Sys.getenv("GAR_SERVICE_JSON"))

bqr_list_projects()
bqr_list_datasets(projectId)

# Steps
# create dataset if does not exist
# create external table -  copy of training dataset in BQ (for training job and batch)
# create create permenant table -  batch of examples (25) of random for batch prediction

# Create BQ dataset
# https://cloud.google.com/bigquery/docs/datasets#sql
bq_dataset_name <- "california_housing"

query_bq_dataset <- glue("
#standardSQL
CREATE SCHEMA `{projectId}.{bq_dataset_name}`
OPTIONS (
  location = 'us'
)
")

bqr_query(projectId = projectId,
          datasetId = bq_dataset_name,
          query = query_bq_dataset,
          useLegacySql = FALSE)


# Create BQ external table
# https://cloud.google.com/bigquery/docs/external-data-cloud-storage#sql
query_bq_table <- glue("
#standardSQL
CREATE EXTERNAL TABLE california_housing.source_data
OPTIONS(
  format = 'CSV',
  uris = ['gs://cloud-samples-data/ai-platform-unified/datasets/tabular/california-housing-tabular-regression.csv']
)
")

bqr_query(projectId = projectId,
          datasetId = bq_dataset_name,
          query = query_bq_table,
          useLegacySql = FALSE)

# Create BQ permenant table
# https://cloud.google.com/bigquery/docs/reference/standard-sql/data-definition-language#create_table_statement
query_bq_table2 <- glue("
#standardSQL
CREATE TABLE `california_housing.data` AS (
    SELECT * FROM `california_housing.source_data`
  )
")


bqr_query(projectId = projectId,
          datasetId = bq_dataset_name,
          query = query_bq_table2,
          useLegacySql = FALSE)

# Create BQ permenant table - batch
# https://cloud.google.com/bigquery/docs/reference/standard-sql/data-definition-language#create_table_statement

query_bq_table2 <- glue("
#standardSQL
CREATE TABLE `california_housing.batch_01` AS (
    SELECT * FROM `california_housing.source_data` LIMIT 10
  )
")

bqr_query(projectId = projectId,
          datasetId = bq_dataset_name,
          query = query_bq_table2,
          useLegacySql = FALSE)
